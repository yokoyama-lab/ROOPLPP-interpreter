(**
  roopl.v -- Rocq formalization of ROOPL++ (Cservenka 2018), the reversible
  object-oriented language implemented by this interpreter, with
  machine-checked proofs of

     invert_invert   invert (invert s) = s
     exec_invert     exec s a b -> exec (invert s) b a
     exec_iff        exec s a b <-> exec (invert s) b a
     exec_det        forward determinism
     exec_inj        exec s a1 b -> exec s a2 b -> a1 == a2   (REVERSIBILITY)
     exec_round_trip running a program then its inverse is the identity

  State.  ROOPL++ is statically typed, so integer variables and object
  variables live in separate stores.  Objects are allocated by the *block
  structured* `construct C x s destruct x`, so the heap is a stack: the fresh
  location is always [hn], the current height, which makes allocation a
  deterministic function -- the reason the inverse block re-allocates exactly
  the same location.  States are compared up to the live prefix of the heap
  (cells at or above [hn] are unreachable), so `dealloc (alloc a) == a` holds
  without any invariant.

  Reads through nil or through a location that is no longer live yield 0;
  writes require a live location.

  Axioms.  None: every theorem reports `Closed under the global context`.
  Stores are functions, so instead of assuming functional extensionality the
  rules that build a state relate it *pointwise* (==) to the intended update.
*)

From Stdlib Require Import ZArith List Bool Arith Lia.
Import ListNotations.
Open Scope Z_scope.

(* ------------------------------------------------------------------ *)
(** * States                                                           *)
(* ------------------------------------------------------------------ *)

Definition id := nat.
Definition mid := nat.
Definition loc := nat.
Definition field := nat.

(** [vs]: integer variables, [os]: object variables (None = nil),
    [hn]: heap height, [hp]: the fields of every allocated object. *)
Record state := St {
  vs : id -> Z;
  os : id -> option loc;
  hn : nat;
  hp : loc -> field -> Z
}.

Definition setv (a : state) (x : id) (v : Z) : state :=
  St (fun y => if Nat.eqb x y then v else vs a y) (os a) (hn a) (hp a).

Definition seto (a : state) (x : id) (r : option loc) : state :=
  St (vs a) (fun y => if Nat.eqb x y then r else os a y) (hn a) (hp a).

Definition setf (a : state) (l : loc) (f : field) (v : Z) : state :=
  St (vs a) (os a) (hn a)
     (fun l' f' => if andb (Nat.eqb l l') (Nat.eqb f f') then v else hp a l' f').

(** Allocation: the fresh location is the current height, its fields are
    zeroed, and the object variable [x] is bound to it. *)
Definition alloc (a : state) (x : id) : state :=
  St (vs a)
     (fun y => if Nat.eqb x y then Some (hn a) else os a y)
     (S (hn a))
     (fun l f => if Nat.eqb l (hn a) then 0 else hp a l f).

(** Deallocation: pop the top object and set [x] back to nil. *)
Definition dealloc (a : state) (x : id) : state :=
  St (vs a)
     (fun y => if Nat.eqb x y then None else os a y)
     (pred (hn a))
     (hp a).

(** Pointwise equality of states, up to the live prefix of the heap. *)
Definition steq (a b : state) : Prop :=
  (forall x, vs a x = vs b x)
  /\ (forall x, os a x = os b x)
  /\ hn a = hn b
  /\ (forall l f, (l < hn a)%nat -> hp a l f = hp b l f).
Infix "==" := steq (at level 70, no associativity).

Lemma steq_refl : forall a, a == a.
Proof. intro a; repeat split; auto. Qed.

Lemma steq_sym : forall a b, a == b -> b == a.
Proof.
  intros a b (Hv & Ho & Hn & Hh); repeat split.
  - intro x; symmetry; apply Hv.
  - intro x; symmetry; apply Ho.
  - symmetry; apply Hn.
  - intros l f Hl; symmetry; apply Hh; lia.
Qed.

Lemma steq_trans : forall a b c, a == b -> b == c -> a == c.
Proof.
  intros a b c (Hv1 & Ho1 & Hn1 & Hh1) (Hv2 & Ho2 & Hn2 & Hh2); repeat split.
  - intro x; rewrite Hv1; apply Hv2.
  - intro x; rewrite Ho1; apply Ho2.
  - lia.
  - intros l f Hl; rewrite Hh1 by lia; apply Hh2; lia.
Qed.

Hint Resolve steq_refl steq_sym steq_trans : core.

(** Rewriting both ends of a state equality; used throughout the proofs to
    transport a rule premise [b == c] to [b' == c']. *)
Lemma steq_rewrite : forall b b' c c', b == b' -> c == c' -> b == c -> b' == c'.
Proof. intros b b' c c' H1 H2 H3; eauto using steq_trans, steq_sym. Qed.

(** Component accessors. *)
Lemma steq_vs : forall a b x, a == b -> vs a x = vs b x.
Proof. intros a b x (H & _); apply H. Qed.
Lemma steq_os : forall a b x, a == b -> os a x = os b x.
Proof. intros a b x (_ & H & _); apply H. Qed.
Lemma steq_hn : forall a b, a == b -> hn a = hn b.
Proof. intros a b (_ & _ & H & _); apply H. Qed.
Lemma steq_hp : forall a b l f, a == b -> (l < hn a)%nat -> hp a l f = hp b l f.
Proof. intros a b l f (_ & _ & _ & H); apply H. Qed.

(* ------------------------------------------------------------------ *)
(** * Expressions                                                      *)
(* ------------------------------------------------------------------ *)

Inductive binop := Oadd | Osub | Omul | Oeq | Olt.

Inductive exp :=
| Cst (z : Z)
| Var (x : id)                (**r integer variable *)
| Fld (x : id) (f : field)    (**r x.f *)
| Bop (o : binop) (e1 e2 : exp).

Definition bval (b : bool) : Z := if b then 1 else 0.

Definition eval_binop (o : binop) (a b : Z) : Z :=
  match o with
  | Oadd => a + b
  | Osub => a - b
  | Omul => a * b
  | Oeq  => bval (Z.eqb a b)
  | Olt  => bval (Z.ltb a b)
  end.

(** Reading a field through nil, or through a location that is no longer
    live, yields 0.  The clamp is what makes [eval] respect [==]. *)
Definition rdf (a : state) (x : id) (f : field) : Z :=
  match os a x with
  | Some l => if Nat.ltb l (hn a) then hp a l f else 0
  | None => 0
  end.

Fixpoint eval (e : exp) (a : state) : Z :=
  match e with
  | Cst z => z
  | Var x => vs a x
  | Fld x f => rdf a x f
  | Bop o e1 e2 => eval_binop o (eval e1 a) (eval e2 a)
  end.

(** The integer variables of an expression (object variables live in a
    different namespace, so an integer assignment cannot disturb a field). *)
Fixpoint fv (e : exp) : list id :=
  match e with
  | Cst _ => []
  | Var x => [x]
  | Fld _ _ => []
  | Bop _ e1 e2 => fv e1 ++ fv e2
  end.

Lemma rdf_steq : forall a b x f, a == b -> rdf a x f = rdf b x f.
Proof.
  intros a b x f (Hv & Ho & Hn & Hh). unfold rdf.
  rewrite (Ho x), <- Hn.
  destruct (os b x) as [l|]; auto.
  destruct (Nat.ltb l (hn a)) eqn:E; auto.
  apply Hh. now apply Nat.ltb_lt in E.
Qed.

Lemma eval_steq : forall e a b, a == b -> eval e a = eval e b.
Proof.
  induction e; intros a b H; simpl; auto.
  - apply H.
  - now apply rdf_steq.
  - now rewrite (IHe1 a b H), (IHe2 a b H).
Qed.

(** An integer assignment to [x] leaves an expression without [x] alone. *)
Lemma eval_setv_notin :
  forall e a w v, ~ In w (fv e) -> eval e (setv a w v) = eval e a.
Proof.
  induction e; intros a w v H; simpl in *; auto.
  - unfold setv; simpl. destruct (Nat.eqb w x) eqn:E; auto.
    apply Nat.eqb_eq in E; subst; exfalso; apply H; now left.
  - rewrite IHe1, IHe2; auto.
    + intro; apply H; apply in_or_app; now right.
    + intro; apply H; apply in_or_app; now left.
Qed.

Lemma eval_off_v :
  forall e a b w v, ~ In w (fv e) -> b == setv a w v -> eval e b = eval e a.
Proof.
  intros e a b w v Hn Hb.
  rewrite (eval_steq e b (setv a w v) Hb). now apply eval_setv_notin.
Qed.

(* ------------------------------------------------------------------ *)
(** * Reversible update operators                                      *)
(* ------------------------------------------------------------------ *)

Inductive modop := MAdd | MSub | MXor.

Definition mapp (o : modop) (a b : Z) : Z :=
  match o with MAdd => a + b | MSub => a - b | MXor => Z.lxor a b end.

Definition minv (o : modop) : modop :=
  match o with MAdd => MSub | MSub => MAdd | MXor => MXor end.

Lemma minv_involutive : forall o, minv (minv o) = o.
Proof. destruct o; reflexivity. Qed.

Lemma mapp_minv : forall o a b, mapp (minv o) (mapp o a b) b = a.
Proof.
  destruct o; simpl; intros a b; try lia.
  rewrite Z.lxor_assoc, Z.lxor_nilpotent, Z.lxor_0_r; reflexivity.
Qed.

(* ------------------------------------------------------------------ *)
(** * Statements and inversion                                         *)
(* ------------------------------------------------------------------ *)

Inductive stm :=
| Sskip
| Sassign (x : id) (o : modop) (e : exp)             (**r x op= e *)
| Sfassign (x : id) (f : field) (o : modop) (e : exp)(**r x.f op= e *)
| Sswap (x y : id)                                    (**r int x <=> y *)
| Soswap (x y : id)                                   (**r object x <=> y *)
| Scopy (x y : id)                                    (**r copy C x y *)
| Suncopy (x y : id)                                  (**r uncopy C x y *)
| Sseq (s1 s2 : stm)
| Sif (e1 : exp) (s1 s2 : stm) (e2 : exp)
| Sloop (e1 : exp) (s1 s2 : stm) (e2 : exp)
| Slocal (x : id) (e1 : exp) (s : stm) (e2 : exp)
| Sobj (x : id) (s : stm)                             (**r construct C x s destruct x *)
| Scall (m : mid)
| Suncall (m : mid).

Fixpoint invert (s : stm) : stm :=
  match s with
  | Sskip => Sskip
  | Sassign x o e => Sassign x (minv o) e
  | Sfassign x f o e => Sfassign x f (minv o) e
  | Sswap x y => Sswap x y
  | Soswap x y => Soswap x y
  | Scopy x y => Suncopy x y
  | Suncopy x y => Scopy x y
  | Sseq s1 s2 => Sseq (invert s2) (invert s1)
  | Sif e1 s1 s2 e2 => Sif e2 (invert s1) (invert s2) e1
  | Sloop e1 s1 s2 e2 => Sloop e2 (invert s1) (invert s2) e1
  | Slocal x e1 s e2 => Slocal x e2 (invert s) e1
  | Sobj x s => Sobj x (invert s)
  | Scall m => Suncall m
  | Suncall m => Scall m
  end.

Theorem invert_invert : forall s, invert (invert s) = s.
Proof.
  induction s; simpl; try reflexivity;
    try (rewrite IHs1, IHs2; reflexivity);
    try (rewrite IHs; reflexivity);
    try (rewrite minv_involutive; reflexivity).
Qed.

(* ------------------------------------------------------------------ *)
(** * State-operation lemmas                                           *)
(* ------------------------------------------------------------------ *)

Lemma setv_steq : forall a b x v, a == b -> setv a x v == setv b x v.
Proof.
  intros a b x v (Hv & Ho & Hn & Hh); split; [ | split; [ | split ] ]; simpl.
  - intro y; destruct (Nat.eqb x y); [ reflexivity | apply Hv ].
  - apply Ho.
  - apply Hn.
  - intros l f Hl; apply Hh; exact Hl.
Qed.

Lemma seto_steq : forall a b x r, a == b -> seto a x r == seto b x r.
Proof.
  intros a b x r (Hv & Ho & Hn & Hh); split; [ | split; [ | split ] ]; simpl.
  - apply Hv.
  - intro y; destruct (Nat.eqb x y); [ reflexivity | apply Ho ].
  - apply Hn.
  - intros l f Hl; apply Hh; exact Hl.
Qed.

Lemma setf_steq : forall a b l f v, a == b -> setf a l f v == setf b l f v.
Proof.
  intros a b l f v (Hv & Ho & Hn & Hh); split; [ | split; [ | split ] ]; simpl.
  - apply Hv.
  - apply Ho.
  - apply Hn.
  - intros l' f' Hl'.
    destruct (Nat.eqb l l' && Nat.eqb f f')%bool; [ reflexivity | apply Hh; exact Hl' ].
Qed.

Lemma alloc_steq : forall a b x, a == b -> alloc a x == alloc b x.
Proof.
  intros a b x (Hv & Ho & Hn & Hh); split; [ | split; [ | split ] ]; simpl.
  - apply Hv.
  - intro y; destruct (Nat.eqb x y); [ rewrite Hn; reflexivity | apply Ho ].
  - rewrite Hn; reflexivity.
  - intros l f Hl; simpl in Hl; rewrite <- Hn.
    destruct (Nat.eqb l (hn a)) eqn:E; [ reflexivity | ].
    apply Hh; apply Nat.eqb_neq in E; lia.
Qed.

Lemma dealloc_steq : forall a b x, a == b -> dealloc a x == dealloc b x.
Proof.
  intros a b x (Hv & Ho & Hn & Hh); split; [ | split; [ | split ] ]; simpl.
  - apply Hv.
  - intro y; destruct (Nat.eqb x y); [ reflexivity | apply Ho ].
  - rewrite Hn; reflexivity.
  - intros l f Hl; apply Hh; simpl in Hl; lia.
Qed.

(** Allocating then deallocating is the identity (the fresh cell is above
    the live prefix, so nothing observable changes). *)
Lemma dealloc_alloc : forall a x, os a x = None -> dealloc (alloc a x) x == a.
Proof.
  intros a x Hx; split; [ | split; [ | split ] ]; simpl.
  - reflexivity.
  - intro y; destruct (Nat.eqb x y) eqn:E; [ | reflexivity ].
    apply Nat.eqb_eq in E; subst; now rewrite Hx.
  - reflexivity.
  - intros l f Hl; destruct (Nat.eqb l (hn a)) eqn:E; [ | reflexivity ].
    apply Nat.eqb_eq in E; subst; lia.
Qed.

(* ------------------------------------------------------------------ *)
(** * Big-step operational semantics                                   *)
(* ------------------------------------------------------------------ *)

Definition menv := mid -> option stm.

Inductive exec (G : menv) : stm -> state -> state -> Prop :=
| E_skip : forall a b,
    a == b -> exec G Sskip a b
| E_assign : forall x o e a b,
    ~ In x (fv e) ->
    b == setv a x (mapp o (vs a x) (eval e a)) ->
    exec G (Sassign x o e) a b
| E_fassign : forall x f o e a b l,
    os a x = Some l -> (l < hn a)%nat ->
    b == setf a l f (mapp o (hp a l f) (eval e a)) ->
    eval e b = eval e a ->
    exec G (Sfassign x f o e) a b
| E_swap : forall x y a b,
    b == setv (setv a x (vs a y)) y (vs a x) ->
    exec G (Sswap x y) a b
| E_oswap : forall x y a b,
    b == seto (seto a x (os a y)) y (os a x) ->
    exec G (Soswap x y) a b
| E_copy : forall x y a b,
    x <> y -> os a y = None ->
    b == seto a y (os a x) ->
    exec G (Scopy x y) a b
| E_uncopy : forall x y a b,
    x <> y -> os a x = os a y ->
    b == seto a y None ->
    exec G (Suncopy x y) a b
| E_seq : forall s1 s2 a b c,
    exec G s1 a b -> exec G s2 b c -> exec G (Sseq s1 s2) a c
| E_if_t : forall e1 s1 s2 e2 a b,
    eval e1 a <> 0 -> exec G s1 a b -> eval e2 b <> 0 ->
    exec G (Sif e1 s1 s2 e2) a b
| E_if_f : forall e1 s1 s2 e2 a b,
    eval e1 a = 0 -> exec G s2 a b -> eval e2 b = 0 ->
    exec G (Sif e1 s1 s2 e2) a b
| E_loop : forall e1 s1 s2 e2 a b c,
    eval e1 a <> 0 -> exec G s1 a b -> loopx G e1 s1 s2 e2 b c ->
    exec G (Sloop e1 s1 s2 e2) a c
| E_local : forall x e1 s e2 a b c,
    ~ In x (fv e1) -> ~ In x (fv e2) ->
    exec G s (setv a x (eval e1 a)) b ->
    vs b x = eval e2 b ->
    c == setv b x (vs a x) ->
    exec G (Slocal x e1 s e2) a c
| E_obj : forall x s a b c,
    os a x = None ->
    exec G s (alloc a x) b ->
    os b x = Some (hn a) ->
    hn b = S (hn a) ->
    (forall f, hp b (hn a) f = 0) ->
    c == dealloc b x ->
    exec G (Sobj x s) a c
| E_call : forall m s a b,
    G m = Some s -> exec G s a b -> exec G (Scall m) a b
| E_uncall : forall m s a b,
    G m = Some s -> exec G (invert s) a b -> exec G (Suncall m) a b

with loopx (G : menv) : exp -> stm -> stm -> exp -> state -> state -> Prop :=
| L_done : forall e1 s1 s2 e2 a b,
    eval e2 a <> 0 -> a == b -> loopx G e1 s1 s2 e2 a b
| L_step : forall e1 s1 s2 e2 a b c d,
    eval e2 a = 0 -> exec G s2 a b -> eval e1 b = 0 -> exec G s1 b c ->
    loopx G e1 s1 s2 e2 c d -> loopx G e1 s1 s2 e2 a d.

Scheme exec_min := Minimality for exec Sort Prop
  with loopx_min := Minimality for loopx Sort Prop.
Combined Scheme exec_loopx_min from exec_min, loopx_min.

(** Execution respects pointwise equality of states on both ends. *)
Theorem exec_loopx_eq : forall G,
  (forall s a b, exec G s a b ->
     forall a' b', a == a' -> b == b' -> exec G s a' b')
  /\ (forall e1 s1 s2 e2 a b, loopx G e1 s1 s2 e2 a b ->
     forall a' b', a == a' -> b == b' -> loopx G e1 s1 s2 e2 a' b').
Proof.
  intro G; apply exec_loopx_min.
  - (* skip *)
    intros a b Hab a' b' Ha Hb. apply E_skip; eauto.
  - (* assign *)
    intros x o e a b Hn Hb a' b' Ha Hb'.
    apply E_assign; auto.
    rewrite <- (steq_vs a a' x Ha), <- (eval_steq e a a' Ha).
    eapply steq_rewrite with (b := b) (c := setv a x (mapp o (vs a x) (eval e a)));
      [ assumption | apply setv_steq; assumption | assumption ].
  - (* field assign *)
    intros x f o e a b l Hl Hlt Hb He a' b' Ha Hb'.
    eapply E_fassign with (l := l).
    + rewrite <- (steq_os a a' x Ha); assumption.
    + rewrite <- (steq_hn a a' Ha); assumption.
    + rewrite <- (steq_hp a a' l f Ha Hlt), <- (eval_steq e a a' Ha).
      eapply steq_rewrite
        with (b := b) (c := setf a l f (mapp o (hp a l f) (eval e a)));
        [ assumption | apply setf_steq; assumption | assumption ].
    + rewrite <- (eval_steq e b b' Hb'), <- (eval_steq e a a' Ha); assumption.
  - (* int swap *)
    intros x y a b Hb a' b' Ha Hb'.
    apply E_swap.
    rewrite <- (steq_vs a a' x Ha), <- (steq_vs a a' y Ha).
    eapply steq_rewrite with (b := b) (c := setv (setv a x (vs a y)) y (vs a x));
      [ assumption | apply setv_steq; apply setv_steq; assumption | assumption ].
  - (* object swap *)
    intros x y a b Hb a' b' Ha Hb'.
    apply E_oswap.
    rewrite <- (steq_os a a' x Ha), <- (steq_os a a' y Ha).
    eapply steq_rewrite with (b := b) (c := seto (seto a x (os a y)) y (os a x));
      [ assumption | apply seto_steq; apply seto_steq; assumption | assumption ].
  - (* copy *)
    intros x y a b Hxy Hy Hb a' b' Ha Hb'.
    apply E_copy; auto.
    + rewrite <- (steq_os a a' y Ha); assumption.
    + rewrite <- (steq_os a a' x Ha).
      eapply steq_rewrite with (b := b) (c := seto a y (os a x));
        [ assumption | apply seto_steq; assumption | assumption ].
  - (* uncopy *)
    intros x y a b Hxy Hxy2 Hb a' b' Ha Hb'.
    apply E_uncopy; auto.
    + rewrite <- (steq_os a a' x Ha), <- (steq_os a a' y Ha); assumption.
    + eapply steq_rewrite with (b := b) (c := seto a y None);
        [ assumption | apply seto_steq; assumption | assumption ].
  - (* seq *)
    intros s1 s2 a b c H1 IH1 H2 IH2 a' c' Ha Hc.
    eapply E_seq.
    + apply IH1; [ exact Ha | apply steq_refl ].
    + apply IH2; [ apply steq_refl | exact Hc ].
  - (* if true *)
    intros e1 s1 s2 e2 a b H1 H2 IH H3 a' b' Ha Hb.
    apply E_if_t.
    + rewrite <- (eval_steq e1 a a' Ha); assumption.
    + apply IH; assumption.
    + rewrite <- (eval_steq e2 b b' Hb); assumption.
  - (* if false *)
    intros e1 s1 s2 e2 a b H1 H2 IH H3 a' b' Ha Hb.
    apply E_if_f.
    + rewrite <- (eval_steq e1 a a' Ha); assumption.
    + apply IH; assumption.
    + rewrite <- (eval_steq e2 b b' Hb); assumption.
  - (* loop *)
    intros e1 s1 s2 e2 a b c H1 H2 IH1 H3 IH2 a' c' Ha Hc.
    eapply E_loop.
    + rewrite <- (eval_steq e1 a a' Ha); assumption.
    + apply IH1; [ exact Ha | apply steq_refl ].
    + apply IH2; [ apply steq_refl | exact Hc ].
  - (* local *)
    intros x e1 s e2 a b c Hn1 Hn2 Hs IH Hx Hc a' c' Ha Hc'.
    eapply E_local; eauto.
    + apply IH; [ | apply steq_refl ].
      rewrite <- (eval_steq e1 a a' Ha). now apply setv_steq.
    + rewrite <- (steq_vs a a' x Ha).
      eapply steq_rewrite with (b := c) (c := setv b x (vs a x));
        [ assumption | apply steq_refl | assumption ].
  - (* object block *)
    intros x s a b c Hx Hs IH Hbx Hbn Hbz Hc a' c' Ha Hc'.
    eapply E_obj with (b := b).
    + rewrite <- (steq_os a a' x Ha); assumption.
    + apply IH; [ now apply alloc_steq | apply steq_refl ].
    + rewrite <- (steq_hn a a' Ha); assumption.
    + rewrite <- (steq_hn a a' Ha); assumption.
    + rewrite <- (steq_hn a a' Ha); assumption.
    + eapply steq_rewrite with (b := c) (c := dealloc b x);
        [ assumption | apply steq_refl | assumption ].
  - (* call *)
    intros m s a b Hm Hs IH a' b' Ha Hb. eapply E_call; eauto.
  - (* uncall *)
    intros m s a b Hm Hs IH a' b' Ha Hb. eapply E_uncall; eauto.
  - (* loop done *)
    intros e1 s1 s2 e2 a b H1 Hab a' b' Ha Hb.
    apply L_done.
    + rewrite <- (eval_steq e2 a a' Ha); assumption.
    + eauto.
  - (* loop step *)
    intros e1 s1 s2 e2 a b c d H1 H2 IH1 H3 H4 IH2 H5 IH3 a' d' Ha Hd.
    eapply L_step.
    + rewrite <- (eval_steq e2 a a' Ha); assumption.
    + apply IH1; [ exact Ha | apply steq_refl ].
    + eassumption.
    + eassumption.
    + apply IH3; [ apply steq_refl | exact Hd ].
Qed.

Definition exec_eq G := proj1 (exec_loopx_eq G).
Definition loopx_eq G := proj2 (exec_loopx_eq G).

(* ------------------------------------------------------------------ *)
(** * Inversion is a semantic inverse                                  *)
(* ------------------------------------------------------------------ *)

Lemma loopx_exit : forall G e1 s1 s2 e2 a b,
  loopx G e1 s1 s2 e2 a b -> eval e2 b <> 0.
Proof.
  intros G e1 s1 s2 e2 a b H; induction H.
  - rewrite <- (eval_steq e2 a b H0); assumption.
  - assumption.
Qed.

Theorem exec_loopx_invert : forall G,
  (forall s a b, exec G s a b -> exec G (invert s) b a)
  /\ (forall e1 s1 s2 e2 a b, loopx G e1 s1 s2 e2 a b ->
        forall t u,
          exec G (invert s1) a t ->
          loopx G e2 (invert s1) (invert s2) e1 t u ->
          exists m, exec G (invert s1) b m
                    /\ loopx G e2 (invert s1) (invert s2) e1 m u).
Proof.
  intro G; apply exec_loopx_min.
  - (* skip *)
    intros a b Hab. apply E_skip. now apply steq_sym.
  - (* assign *)
    intros x o e a b Hn Hb. apply E_assign; auto.
    assert (Hv : vs b x = mapp o (vs a x) (eval e a))
      by (rewrite (steq_vs b _ x Hb); simpl; now rewrite Nat.eqb_refl).
    assert (He : eval e b = eval e a) by (eapply eval_off_v; eauto).
    rewrite Hv, He, mapp_minv.
    split; [ | split; [ | split ] ]; simpl.
    + intro y. destruct (Nat.eqb x y) eqn:E.
      * apply Nat.eqb_eq in E; subst y; reflexivity.
      * rewrite (steq_vs b _ y Hb); simpl; now rewrite E.
    + intro y; symmetry; rewrite (steq_os b _ y Hb); reflexivity.
    + symmetry; rewrite (steq_hn b _ Hb); reflexivity.
    + intros l f Hl; symmetry.
      rewrite (steq_hp b _ l f Hb); [ reflexivity | ].
      rewrite (steq_hn b _ Hb); simpl; assumption.
  - (* field assign *)
    intros x f o e a b l Hl Hlt Hb He.
    assert (Hos : os b x = Some l)
      by (rewrite (steq_os b _ x Hb); simpl; assumption).
    assert (Hhn : hn b = hn a) by (rewrite (steq_hn b _ Hb); reflexivity).
    assert (Hf : hp b l f = mapp o (hp a l f) (eval e a)).
    { rewrite (steq_hp b _ l f Hb) by (rewrite Hhn; assumption).
      simpl. now rewrite Nat.eqb_refl, Nat.eqb_refl. }
    eapply E_fassign with (l := l); auto.
    + rewrite Hhn; assumption.
    + rewrite Hf, He, mapp_minv.
      split; [ | split; [ | split ] ]; simpl.
      * intro y; symmetry; rewrite (steq_vs b _ y Hb); reflexivity.
      * intro y; symmetry; rewrite (steq_os b _ y Hb); reflexivity.
      * symmetry; assumption.
      * intros l' f' Hl'; destruct (Nat.eqb l l' && Nat.eqb f f')%bool eqn:E.
        -- apply andb_true_iff in E as [E1 E2].
           apply Nat.eqb_eq in E1; apply Nat.eqb_eq in E2; subst; reflexivity.
        -- symmetry. rewrite (steq_hp b _ l' f' Hb) by (rewrite Hhn; assumption).
           simpl. now rewrite E.
  - (* int swap *)
    intros x y a b Hb. apply E_swap.
    assert (Hbx : vs b x = if Nat.eqb y x then vs a x else vs a y).
    { rewrite (steq_vs b _ x Hb); simpl; rewrite Nat.eqb_refl.
      destruct (Nat.eqb y x); reflexivity. }
    assert (Hby : vs b y = vs a x)
      by (rewrite (steq_vs b _ y Hb); simpl; now rewrite Nat.eqb_refl).
    rewrite Hbx, Hby.
    split; [ | split; [ | split ] ]; simpl.
    + intro z. destruct (Nat.eqb y z) eqn:Eyz.
      * apply Nat.eqb_eq in Eyz; subst z.
        destruct (Nat.eqb y x) eqn:Eyx; [ apply Nat.eqb_eq in Eyx; subst y | ]; reflexivity.
      * destruct (Nat.eqb x z) eqn:Exz.
        -- apply Nat.eqb_eq in Exz; subst z.
           destruct (Nat.eqb y x) eqn:Eyx; [ congruence | reflexivity ].
        -- rewrite (steq_vs b _ z Hb); simpl; now rewrite Eyz, Exz.
    + intro z; symmetry; rewrite (steq_os b _ z Hb); reflexivity.
    + symmetry; rewrite (steq_hn b _ Hb); reflexivity.
    + intros l f Hl; symmetry. rewrite (steq_hp b _ l f Hb); [ reflexivity | ].
      rewrite (steq_hn b _ Hb); simpl; assumption.
  - (* object swap *)
    intros x y a b Hb. apply E_oswap.
    assert (Hbx : os b x = if Nat.eqb y x then os a x else os a y).
    { rewrite (steq_os b _ x Hb); simpl; rewrite Nat.eqb_refl.
      destruct (Nat.eqb y x); reflexivity. }
    assert (Hby : os b y = os a x)
      by (rewrite (steq_os b _ y Hb); simpl; now rewrite Nat.eqb_refl).
    rewrite Hbx, Hby.
    split; [ | split; [ | split ] ]; simpl.
    + intro z; symmetry; rewrite (steq_vs b _ z Hb); reflexivity.
    + intro z. destruct (Nat.eqb y z) eqn:Eyz.
      * apply Nat.eqb_eq in Eyz; subst z.
        destruct (Nat.eqb y x) eqn:Eyx; [ apply Nat.eqb_eq in Eyx; subst y | ]; reflexivity.
      * destruct (Nat.eqb x z) eqn:Exz.
        -- apply Nat.eqb_eq in Exz; subst z.
           destruct (Nat.eqb y x) eqn:Eyx; [ congruence | reflexivity ].
        -- rewrite (steq_os b _ z Hb); simpl; now rewrite Eyz, Exz.
    + symmetry; rewrite (steq_hn b _ Hb); reflexivity.
    + intros l f Hl; symmetry. rewrite (steq_hp b _ l f Hb); [ reflexivity | ].
      rewrite (steq_hn b _ Hb); simpl; assumption.
  - (* copy *)
    intros x y a b Hxy Hy Hb. apply E_uncopy; auto.
    + assert (Hbx : os b x = os a x).
      { rewrite (steq_os b _ x Hb); simpl.
        destruct (Nat.eqb y x) eqn:E; [ apply Nat.eqb_eq in E; congruence | reflexivity ]. }
      assert (Hby : os b y = os a x)
        by (rewrite (steq_os b _ y Hb); simpl; now rewrite Nat.eqb_refl).
      now rewrite Hbx, Hby.
    + split; [ | split; [ | split ] ]; simpl.
      * intro z; symmetry; rewrite (steq_vs b _ z Hb); reflexivity.
      * intro z; destruct (Nat.eqb y z) eqn:E.
        -- apply Nat.eqb_eq in E; subst z; now rewrite Hy.
        -- symmetry; rewrite (steq_os b _ z Hb); simpl; now rewrite E.
      * symmetry; rewrite (steq_hn b _ Hb); reflexivity.
      * intros l f Hl; symmetry. rewrite (steq_hp b _ l f Hb); [ reflexivity | ].
        rewrite (steq_hn b _ Hb); simpl; assumption.
  - (* uncopy *)
    intros x y a b Hxy Hxy2 Hb. apply E_copy; auto.
    + rewrite (steq_os b _ y Hb); simpl; now rewrite Nat.eqb_refl.
    + assert (Hbx : os b x = os a x).
      { rewrite (steq_os b _ x Hb); simpl.
        destruct (Nat.eqb y x) eqn:E; [ apply Nat.eqb_eq in E; congruence | reflexivity ]. }
      rewrite Hbx, Hxy2.
      split; [ | split; [ | split ] ]; simpl.
      * intro z; symmetry; rewrite (steq_vs b _ z Hb); reflexivity.
      * intro z; destruct (Nat.eqb y z) eqn:E.
        -- apply Nat.eqb_eq in E; subst z; reflexivity.
        -- symmetry; rewrite (steq_os b _ z Hb); simpl; now rewrite E.
      * symmetry; rewrite (steq_hn b _ Hb); reflexivity.
      * intros l f Hl; symmetry. rewrite (steq_hp b _ l f Hb); [ reflexivity | ].
        rewrite (steq_hn b _ Hb); simpl; assumption.
  - (* seq *)
    intros s1 s2 a b c H1 IH1 H2 IH2. simpl. eapply E_seq; eassumption.
  - (* if true *)
    intros e1 s1 s2 e2 a b H1 H2 IH H3. simpl. apply E_if_t; assumption.
  - (* if false *)
    intros e1 s1 s2 e2 a b H1 H2 IH H3. simpl. apply E_if_f; assumption.
  - (* loop *)
    intros e1 s1 s2 e2 a b c H1 H2 IH1 H3 IH2. simpl.
    destruct (IH2 a a IH1 (L_done G e2 (invert s1) (invert s2) e1 a a H1 (steq_refl a)))
      as [m [Hm Hloop]].
    eapply E_loop; [ eapply loopx_exit; eassumption | eassumption | assumption ].
  - (* local *)
    intros x e1 s e2 a b c Hn1 Hn2 Hs IH Hx Hc. simpl.
    eapply E_local with (b := setv a x (eval e1 a)); auto.
    + eapply exec_eq; [ eassumption | | apply steq_refl ].
      assert (Hec : eval e2 c = eval e2 b) by (eapply eval_off_v; eauto).
      split; [ | split; [ | split ] ]; simpl.
      * intro y; destruct (Nat.eqb x y) eqn:E.
        -- apply Nat.eqb_eq in E; subst y. rewrite Hec, <- Hx; reflexivity.
        -- rewrite (steq_vs c _ y Hc); simpl; now rewrite E.
      * intro y; rewrite (steq_os c _ y Hc); reflexivity.
      * rewrite (steq_hn c _ Hc); reflexivity.
      * intros l f Hl; symmetry; apply (steq_hp c _ l f Hc).
        rewrite (steq_hn c _ Hc); simpl; assumption.
    + simpl; rewrite Nat.eqb_refl; symmetry; now apply eval_setv_notin.
    + assert (Hcx : vs c x = vs a x)
        by (rewrite (steq_vs c _ x Hc); simpl; now rewrite Nat.eqb_refl).
      rewrite Hcx.
      split; [ | split; [ | split ] ]; simpl.
      * intro y; destruct (Nat.eqb x y) eqn:E;
          [ apply Nat.eqb_eq in E; subst y | ]; reflexivity.
      * reflexivity.
      * reflexivity.
      * reflexivity.
  - (* object block *)
    intros x s a b c Hx Hs IH Hbx Hbn Hbz Hc. simpl.
    assert (Hcn : hn c = hn a).
    { rewrite (steq_hn c _ Hc); simpl; rewrite Hbn; reflexivity. }
    assert (Halloc : alloc c x == b).
    { split; [ | split; [ | split ] ]; simpl.
      - intro y; rewrite (steq_vs c _ y Hc); reflexivity.
      - intro y; destruct (Nat.eqb x y) eqn:E.
        + apply Nat.eqb_eq in E; subst y. rewrite Hcn; now rewrite Hbx.
        + rewrite (steq_os c _ y Hc); simpl; now rewrite E.
      - rewrite Hcn, Hbn; reflexivity.
      - intros l f Hl; destruct (Nat.eqb l (hn c)) eqn:E.
        + apply Nat.eqb_eq in E; subst l. rewrite Hcn; symmetry; apply Hbz.
        + rewrite (steq_hp c _ l f Hc).
          * reflexivity.
          * rewrite (steq_hn c _ Hc); simpl. rewrite Hbn; simpl.
            apply Nat.eqb_neq in E. rewrite Hcn in E. simpl in Hl. lia. }
    eapply E_obj with (b := alloc a x).
    + rewrite (steq_os c _ x Hc); simpl; now rewrite Nat.eqb_refl.
    + eapply exec_eq; [ eassumption | now apply steq_sym | apply steq_refl ].
    + simpl; rewrite Nat.eqb_refl, Hcn; reflexivity.
    + simpl; rewrite Hcn; reflexivity.
    + intro f; simpl; rewrite Hcn, Nat.eqb_refl; reflexivity.
    + apply steq_sym; now apply dealloc_alloc.
  - (* call *)
    intros m s a b Hm Hs IH. simpl. eapply E_uncall; eassumption.
  - (* uncall *)
    intros m s a b Hm Hs IH. simpl. eapply E_call; [ eassumption | ].
    rewrite invert_invert in IH; assumption.
  - (* loop tail: done *)
    intros e1 s1 s2 e2 a b H1 Hab t u Ht Hu.
    exists t; split; [ | assumption ].
    eapply exec_eq; [ eassumption | assumption | apply steq_refl ].
  - (* loop tail: step *)
    intros e1 s1 s2 e2 a b c d H1 H2 IH1 H3 H4 IH2 H5 IH3 t u Ht Hu.
    apply (IH3 b u IH2). eapply L_step; try eassumption.
Qed.

Definition exec_invert G := proj1 (exec_loopx_invert G).

Theorem exec_iff : forall G s a b, exec G s a b <-> exec G (invert s) b a.
Proof.
  intros G s a b; split; intro H.
  - now apply exec_invert.
  - apply exec_invert in H. now rewrite invert_invert in H.
Qed.

(* ------------------------------------------------------------------ *)
(** * Determinism and reversibility                                    *)
(* ------------------------------------------------------------------ *)

Theorem exec_loopx_det : forall G,
  (forall s a b, exec G s a b -> forall b', exec G s a b' -> b == b')
  /\ (forall e1 s1 s2 e2 a b, loopx G e1 s1 s2 e2 a b ->
        forall b', loopx G e1 s1 s2 e2 a b' -> b == b').
Proof.
  intro G; apply exec_loopx_min.
  - (* skip *)
    intros a b Hab b' H; inversion H; subst; eauto.
  - (* assign *)
    intros x o e a b Hn Hb b' H; inversion H; subst; eauto.
  - (* field assign *)
    intros x f o e a b l Hl Hlt Hb He b' H; inversion H; subst.
    assert (l0 = l) by congruence; subst l0; eauto.
  - (* int swap *)
    intros x y a b Hb b' H; inversion H; subst; eauto.
  - (* object swap *)
    intros x y a b Hb b' H; inversion H; subst; eauto.
  - (* copy *)
    intros x y a b Hxy Hy Hb b' H; inversion H; subst; eauto.
  - (* uncopy *)
    intros x y a b Hxy Hxy2 Hb b' H; inversion H; subst; eauto.
  - (* seq *)
    intros s1 s2 a b c H1 IH1 H2 IH2 c' H; inversion H; subst.
    apply IH2. eapply exec_eq; [ eassumption | | apply steq_refl ].
    apply steq_sym; now apply IH1.
  - (* if true *)
    intros e1 s1 s2 e2 a b H1 H2 IH H3 b' H; inversion H; subst.
    + now apply IH.
    + contradiction.
  - (* if false *)
    intros e1 s1 s2 e2 a b H1 H2 IH H3 b' H; inversion H; subst.
    + contradiction.
    + now apply IH.
  - (* loop *)
    intros e1 s1 s2 e2 a b c H1 H2 IH1 H3 IH2 c' H; inversion H; subst.
    apply IH2. eapply loopx_eq; [ eassumption | | apply steq_refl ].
    apply steq_sym; now apply IH1.
  - (* local *)
    intros x e1 s e2 a b c Hn1 Hn2 Hs IH Hx Hc c' H; inversion H; subst.
    eapply steq_trans; [ eassumption | ].
    eapply steq_trans; [ | apply steq_sym; eassumption ].
    apply setv_steq. now apply IH.
  - (* object block *)
    intros x s a b c Hx Hs IH Hbx Hbn Hbz Hc c' H; inversion H; subst.
    eapply steq_trans; [ eassumption | ].
    eapply steq_trans; [ | apply steq_sym; eassumption ].
    apply dealloc_steq. now apply IH.
  - (* call *)
    intros m s a b Hm Hs IH b' H; inversion H; subst.
    match goal with
    | [ HG : G m = Some ?s0 |- _ ] => rewrite Hm in HG; injection HG as ->
    end.
    now apply IH.
  - (* uncall *)
    intros m s a b Hm Hs IH b' H; inversion H; subst.
    match goal with
    | [ HG : G m = Some ?s0 |- _ ] => rewrite Hm in HG; injection HG as ->
    end.
    now apply IH.
  - (* loop tail: done *)
    intros e1 s1 s2 e2 a b H1 Hab b' H; inversion H; subst.
    + eauto.
    + contradiction.
  - (* loop tail: step *)
    intros e1 s1 s2 e2 a b c d H1 H2 IH1 H3 H4 IH2 H5 IH3 d' H; inversion H; subst.
    + contradiction.
    + apply IH3. eapply loopx_eq; [ eassumption | | apply steq_refl ].
      apply steq_sym; apply IH2.
      eapply exec_eq; [ eassumption | | apply steq_refl ].
      apply steq_sym; now apply IH1.
Qed.

Definition exec_det G := proj1 (exec_loopx_det G).

(** **Reversibility**: the final state determines the initial state, i.e.
    every ROOPL++ statement denotes an injective partial function on states. *)
Theorem exec_inj : forall G s a1 a2 b,
  exec G s a1 b -> exec G s a2 b -> a1 == a2.
Proof.
  intros G s a1 a2 b H1 H2.
  apply exec_invert in H1; apply exec_invert in H2.
  eapply exec_det; eassumption.
Qed.

Corollary exec_round_trip : forall G s a b c,
  exec G s a b -> exec G (invert s) b c -> a == c.
Proof.
  intros G s a b c H1 H2.
  apply exec_invert in H1. eapply exec_det; eassumption.
Qed.

(* ------------------------------------------------------------------ *)
(** * Sanity checks: the semantics is not vacuous                      *)
(* ------------------------------------------------------------------ *)

Definition empty_env : menv := fun _ => None.
Definition zero : state := St (fun _ => 0) (fun _ => None) 0%nat (fun _ _ => 0).
Definition X : id := 0%nat.
Definition Y : id := 1%nat.
Definition T : id := 2%nat.
Definition O : id := 3%nat.
Definition F0 : field := 0%nat.

(** X += 3 ; X <=> Y   leaves X = 0 and Y = 3 *)
Example ex_swap :
  exists b, exec empty_env (Sseq (Sassign X MAdd (Cst 3)) (Sswap X Y)) zero b
            /\ vs b X = 0 /\ vs b Y = 3.
Proof.
  eexists. split.
  - eapply E_seq.
    + apply E_assign; [ simpl; tauto | apply steq_refl ].
    + apply E_swap. apply steq_refl.
  - split; reflexivity.
Qed.

(** from X = 0 loop X += 1 until X = 2 *)
Definition count2 : stm :=
  Sloop (Bop Oeq (Var X) (Cst 0)) Sskip (Sassign X MAdd (Cst 1))
        (Bop Oeq (Var X) (Cst 2)).

Example ex_loop : exists b, exec empty_env count2 zero b /\ vs b X = 2.
Proof.
  eexists. split.
  - eapply E_loop; [ simpl; discriminate | apply E_skip; apply steq_refl | ].
    eapply L_step; [ reflexivity | apply E_assign; [ simpl; tauto | apply steq_refl ]
                   | reflexivity | apply E_skip; apply steq_refl | ].
    eapply L_step; [ reflexivity | apply E_assign; [ simpl; tauto | apply steq_refl ]
                   | reflexivity | apply E_skip; apply steq_refl | ].
    apply L_done; [ simpl; discriminate | apply steq_refl ].
  - reflexivity.
Qed.

Example ex_loop_back : exists b, exec empty_env (invert count2) b zero.
Proof.
  destruct ex_loop as [b [Hb _]]. exists b. now apply exec_invert.
Qed.

(** local t = 3  X += t  delocal t = 3 *)
Example ex_local :
  exists b,
    exec empty_env (Slocal T (Cst 3) (Sassign X MAdd (Var T)) (Cst 3)) zero b
    /\ vs b X = 3 /\ vs b T = 0.
Proof.
  eexists. split.
  - eapply E_local; [ simpl; tauto | simpl; tauto | | | apply steq_refl ].
    + apply E_assign; [ unfold X, T; simpl; intuition discriminate | apply steq_refl ].
    + reflexivity.
  - split; reflexivity.
Qed.

(** An object block that writes a field, reads it back and clears it again.
    The heap grows during the block and is popped by `destruct`. *)
Definition objprog : stm :=
  Sobj O (Sseq (Sfassign O F0 MAdd (Cst 3))
               (Sseq (Sassign X MAdd (Fld O F0))
                     (Sfassign O F0 MSub (Cst 3)))).

Example ex_object :
  exists b, exec empty_env objprog zero b
            /\ vs b X = 3 /\ hn b = 0%nat /\ os b O = None.
Proof.
  eexists. split.
  - eapply E_obj.
    + reflexivity.
    + eapply E_seq.
      * eapply E_fassign with (l := 0%nat);
          [ reflexivity | simpl; lia | apply steq_refl | reflexivity ].
      * eapply E_seq.
        -- apply E_assign; [ simpl; tauto | apply steq_refl ].
        -- eapply E_fassign with (l := 0%nat);
             [ reflexivity | simpl; lia | apply steq_refl | reflexivity ].
    + reflexivity.
    + reflexivity.
    + intro f; simpl; destruct f; reflexivity.
    + apply steq_refl.
  - split; [ reflexivity | split; reflexivity ].
Qed.

(** copy C x y then uncopy C x y restores the state. *)
Example ex_copy_uncopy :
  exists b c, exec empty_env (Sobj O (Sseq (Scopy O Y) (Suncopy O Y))) zero b
              /\ b == zero /\ c == zero.
Proof.
  eexists. exists zero. split.
  - eapply E_obj.
    + reflexivity.
    + eapply E_seq.
      * apply E_copy; [ unfold O, Y; discriminate | reflexivity | apply steq_refl ].
      * apply E_uncopy; [ unfold O, Y; discriminate | reflexivity | apply steq_refl ].
    + reflexivity.
    + reflexivity.
    + intro f; reflexivity.
    + apply steq_refl.
  - split; [ | apply steq_refl ].
    split; [ | split; [ | split ] ]; simpl.
    + intro y0; reflexivity.
    + intro y0; destruct y0 as [ | [ | [ | [ | y0 ] ] ] ]; reflexivity.
    + reflexivity.
    + intros l f Hl; lia.
Qed.

(** The side condition on assignment bites: `X += X` has no derivation. *)
Example ex_self_assign_stuck :
  forall a b, ~ exec empty_env (Sassign X MAdd (Var X)) a b.
Proof.
  intros a b H; inversion H; subst. simpl in *. intuition.
Qed.

(* ------------------------------------------------------------------ *)
(** * Axiom check                                                      *)
(* ------------------------------------------------------------------ *)

Print Assumptions invert_invert.
Print Assumptions exec_invert.
Print Assumptions exec_iff.
Print Assumptions exec_det.
Print Assumptions exec_inj.
Print Assumptions exec_round_trip.
