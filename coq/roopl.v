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

(* Plain Require so the file compiles both on Rocq 9.x and on Coq 8.x
   (CI uses a Coq 8.20 image; `From Stdlib` would be 9.x-only). *)
Require Import ZArith List Bool Arith Lia.
Import ListNotations.
Open Scope Z_scope.

(* ------------------------------------------------------------------ *)
(** * States                                                           *)
(* ------------------------------------------------------------------ *)

Definition id := nat.
Definition mid := nat.
Definition loc := nat.
Definition field := nat.
Definition cid := nat.   (**r class name *)

(** [vs]: integer variables, [os]: object variables (None = nil),
    [hn]: heap height, [hp]: the fields of every allocated object. *)
Record state := St {
  vs : id -> Z;
  os : id -> option loc;
  hn : nat;
  hp : loc -> field -> Z;
  hc : loc -> cid            (**r the run-time class of each allocated object *)
}.

Definition setv (a : state) (x : id) (v : Z) : state :=
  St (fun y => if Nat.eqb x y then v else vs a y) (os a) (hn a) (hp a) (hc a).

Definition seto (a : state) (x : id) (r : option loc) : state :=
  St (vs a) (fun y => if Nat.eqb x y then r else os a y) (hn a) (hp a) (hc a).

Definition setf (a : state) (l : loc) (f : field) (v : Z) : state :=
  St (vs a) (os a) (hn a)
     (fun l' f' => if andb (Nat.eqb l l') (Nat.eqb f f') then v else hp a l' f')
     (hc a).

(** Allocation: the fresh location is the current height, its fields are
    zeroed, and the object variable [x] is bound to it. *)
(** [alloc a c x]: allocate a fresh object of class [c] and bind [x] to it. *)
Definition alloc (a : state) (c : cid) (x : id) : state :=
  St (vs a)
     (fun y => if Nat.eqb x y then Some (hn a) else os a y)
     (S (hn a))
     (fun l f => if Nat.eqb l (hn a) then 0 else hp a l f)
     (fun l => if Nat.eqb l (hn a) then c else hc a l).

(** Deallocation: pop the top object and set [x] back to nil. *)
Definition dealloc (a : state) (x : id) : state :=
  St (vs a)
     (fun y => if Nat.eqb x y then None else os a y)
     (pred (hn a))
     (hp a)
     (hc a).

(** Pointwise equality of states, up to the live prefix of the heap. *)
Definition steq (a b : state) : Prop :=
  (forall x, vs a x = vs b x)
  /\ (forall x, os a x = os b x)
  /\ hn a = hn b
  /\ (forall l f, (l < hn a)%nat -> hp a l f = hp b l f)
  /\ (forall l, (l < hn a)%nat -> hc a l = hc b l).
Infix "==" := steq (at level 70, no associativity).

Lemma steq_refl : forall a, a == a.
Proof. intro a; repeat split; auto. Qed.

(* 以降、状態は 5 成分（vs / os / hn / hp / hc）なので分解はこの形になる *)
Ltac steq_split := split; [ | split; [ | split; [ | split ] ] ].

Lemma steq_sym : forall a b, a == b -> b == a.
Proof.
  intros a b (Hv & Ho & Hn & Hh & Hc); steq_split.
  - intro x; symmetry; apply Hv.
  - intro x; symmetry; apply Ho.
  - symmetry; apply Hn.
  - intros l f Hl; symmetry; apply Hh; lia.
  - intros l Hl; symmetry; apply Hc; lia.
Qed.

Lemma steq_trans : forall a b c, a == b -> b == c -> a == c.
Proof.
  intros a b c (Hv1 & Ho1 & Hn1 & Hh1 & Hc1) (Hv2 & Ho2 & Hn2 & Hh2 & Hc2); steq_split.
  - intro x; rewrite Hv1; apply Hv2.
  - intro x; rewrite Ho1; apply Ho2.
  - lia.
  - intros l f Hl; rewrite Hh1 by lia; apply Hh2; lia.
  - intros l Hl; rewrite Hc1 by lia; apply Hc2; lia.
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
Proof. intros a b l f (_ & _ & _ & H & _); apply H. Qed.
Lemma steq_hc : forall a b l, a == b -> (l < hn a)%nat -> hc a l = hc b l.
Proof. intros a b l (_ & _ & _ & _ & H); apply H. Qed.

(** hc 成分（実行時クラス）は状態を作るどの操作でも変わらないので、
    証明中はいつも同じ形になる。まとめてタクティクにしておく。 *)
Ltac hc_auto :=
  intros ?loc ?Hloc; simpl in *;
  first
    [ reflexivity
    | match goal with
      | [ H : ?B == _ |- hc _ ?L = hc ?B ?L ] =>
          symmetry; apply (steq_hc B _ L H);
          try (rewrite (steq_hn B _ H); simpl); try assumption; try lia
      end
    | match goal with
      | [ H : ?A == _ |- hc ?A ?L = hc _ ?L ] =>
          apply (steq_hc A _ L H); try assumption; try lia
      end ].


(* ------------------------------------------------------------------ *)
(** * Expressions                                                      *)
(* ------------------------------------------------------------------ *)

Inductive binop := Oadd | Osub | Omul | Oeq | Olt.

Inductive exp :=
| Cst (z : Z)
| Var (x : id)                (**r integer variable *)
| Fld (x : id) (f : field)    (**r x.f *)
| Idx (x : id) (e : exp)      (**r x[e]: an array is an object with a dynamic index *)
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
  | Idx x e => rdf a x (Z.to_nat (eval e a))
  | Bop o e1 e2 => eval_binop o (eval e1 a) (eval e2 a)
  end.

(** The integer variables of an expression (object variables live in a
    different namespace, so an integer assignment cannot disturb a field). *)
Fixpoint fv (e : exp) : list id :=
  match e with
  | Cst _ => []
  | Var x => [x]
  | Fld _ _ => []
  | Idx _ e => fv e
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
  - rewrite (IHe a b H); now apply rdf_steq.
  - now rewrite (IHe1 a b H), (IHe2 a b H).
Qed.

(** An integer assignment to [x] leaves an expression without [x] alone. *)
Lemma eval_setv_notin :
  forall e a w v, ~ In w (fv e) -> eval e (setv a w v) = eval e a.
Proof.
  induction e; intros a w v H; simpl in *; auto.
  - unfold setv; simpl. destruct (Nat.eqb w x) eqn:E; auto.
    apply Nat.eqb_eq in E; subst; exfalso; apply H; now left.
  - now rewrite IHe.
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

(** メソッドの実引数。変数は参照渡し（本体の中の書き換えが呼出し側に見える）、
    式は値渡し（本体が終わったとき同じ値に戻っていることを表明として要求する）。 *)
Inductive arg :=
| Aref (x : id)   (**r 変数：参照渡し *)
| Aval (e : exp). (**r 式：値渡し *)

Inductive stm :=
| Sskip
| Sassign (x : id) (o : modop) (e : exp)             (**r x op= e *)
| Sfassign (x : id) (f : field) (o : modop) (e : exp)(**r x.f op= e *)
| Saassign (x : id) (ei : exp) (o : modop) (e : exp)  (**r x[ei] op= e *)
| Sswap (x y : id)                                    (**r int x <=> y *)
| Saswap (x : id) (e1 : exp) (y : id) (e2 : exp)      (**r x[e1] <=> y[e2] *)
| Soswap (x y : id)                                   (**r object x <=> y *)
| Scopy (x y : id)                                    (**r copy C x y *)
| Suncopy (x y : id)                                  (**r uncopy C x y *)
| Sseq (s1 s2 : stm)
| Sif (e1 : exp) (s1 s2 : stm) (e2 : exp)
| Sloop (e1 : exp) (s1 s2 : stm) (e2 : exp)
| Slocal (x : id) (e1 : exp) (s : stm) (e2 : exp)
| Sshow (e : exp)                                     (**r show(e) / print("...") *)
| Sobj (cl : cid) (x : id) (s : stm)                  (**r construct C x s destruct x *)
| Snew (cl : cid) (x : id)                            (**r new C x *)
| Sdelete (cl : cid) (x : id)                         (**r delete C x *)
| Scall (m : mid) (args : list arg)
| Suncall (m : mid) (args : list arg)
| Socall (x : id) (m : mid) (args : list arg)    (**r call x::m(args) *)
| Souncall (x : id) (m : mid) (args : list arg). (**r uncall x::m(args) *)

Fixpoint invert (s : stm) : stm :=
  match s with
  | Sskip => Sskip
  | Sassign x o e => Sassign x (minv o) e
  | Sfassign x f o e => Sfassign x f (minv o) e
  | Saassign x ei o e => Saassign x ei (minv o) e
  | Sswap x y => Sswap x y
  | Saswap x e1 y e2 => Saswap x e1 y e2
  | Soswap x y => Soswap x y
  | Scopy x y => Suncopy x y
  | Suncopy x y => Scopy x y
  | Sseq s1 s2 => Sseq (invert s2) (invert s1)
  | Sif e1 s1 s2 e2 => Sif e2 (invert s1) (invert s2) e1
  | Sloop e1 s1 s2 e2 => Sloop e2 (invert s1) (invert s2) e1
  | Slocal x e1 s e2 => Slocal x e2 (invert s) e1
  | Sshow e => Sshow e
  | Sobj c x s => Sobj c x (invert s)
  | Snew c x => Sdelete c x
  | Sdelete c x => Snew c x
  | Scall m args => Suncall m args
  | Suncall m args => Scall m args
  | Socall x m args => Souncall x m args
  | Souncall x m args => Socall x m args
  end.

(** 参照渡しの実引数への改名。仮引数でない名前はそのまま。値渡しの仮引数も
    改名しない（下の [wrap_vals] が局所ブロックで束ねる）。 *)
Fixpoint ren_args (ps : list id) (args : list arg) (x : id) : id :=
  match ps, args with
  | p :: ps', Aref y :: as' => if Nat.eqb p x then y else ren_args ps' as' x
  | _ :: ps', Aval _ :: as' => ren_args ps' as' x
  | _, _ => x
  end.

Fixpoint rename_exp (r : id -> id) (e : exp) : exp :=
  match e with
  | Cst z => Cst z
  | Var x => Var (r x)
  | Fld x f => Fld (r x) f
  | Idx x e => Idx (r x) (rename_exp r e)
  | Bop o e1 e2 => Bop o (rename_exp r e1) (rename_exp r e2)
  end.

Definition rename_arg (r : id -> id) (a : arg) : arg :=
  match a with
  | Aref x => Aref (r x)
  | Aval e => Aval (rename_exp r e)
  end.

Fixpoint rename (r : id -> id) (s : stm) : stm :=
  match s with
  | Sskip => Sskip
  | Sassign x o e => Sassign (r x) o (rename_exp r e)
  | Sfassign x f o e => Sfassign (r x) f o (rename_exp r e)
  | Saassign x ei o e => Saassign (r x) (rename_exp r ei) o (rename_exp r e)
  | Sswap x y => Sswap (r x) (r y)
  | Saswap x e1 y e2 => Saswap (r x) (rename_exp r e1) (r y) (rename_exp r e2)
  | Soswap x y => Soswap (r x) (r y)
  | Scopy x y => Scopy (r x) (r y)
  | Suncopy x y => Suncopy (r x) (r y)
  | Sseq s1 s2 => Sseq (rename r s1) (rename r s2)
  | Sif e1 s1 s2 e2 =>
      Sif (rename_exp r e1) (rename r s1) (rename r s2) (rename_exp r e2)
  | Sloop e1 s1 s2 e2 =>
      Sloop (rename_exp r e1) (rename r s1) (rename r s2) (rename_exp r e2)
  | Slocal x e1 s' e2 =>
      Slocal (r x) (rename_exp r e1) (rename r s') (rename_exp r e2)
  | Sshow e => Sshow (rename_exp r e)
  | Sobj c x s' => Sobj c (r x) (rename r s')
  | Snew c x => Snew c (r x)
  | Sdelete c x => Sdelete c (r x)
  | Scall m args => Scall m (map (rename_arg r) args)
  | Suncall m args => Suncall m (map (rename_arg r) args)
  | Socall x m args => Socall (r x) m (map (rename_arg r) args)
  | Souncall x m args => Souncall (r x) m (map (rename_arg r) args)
  end.

(** 値渡しの仮引数は局所ブロックで束ねる。

    [call m(e)]（仮引数 p）は
      local p = e   本体   delocal p = e
    と同じ意味で、**出口で値が戻っていることの表明**がそのまま
    「値引数は書き換えてはならない」という可逆性の副条件になる。局所ブロックの
    規則をそのまま使うので、可逆性・決定性は既に証明済みのものが効く。 *)
Fixpoint wrap_vals (ps : list id) (args : list arg) (s : stm) : stm :=
  match ps, args with
  | p :: ps', Aval e :: as' => Slocal p e (wrap_vals ps' as' s) e
  | _ :: ps', _ :: as' => wrap_vals ps' as' s
  | _, _ => s
  end.

(** 実引数を仮引数に束ねた本体。参照渡しは改名、値渡しは局所ブロック。 *)
Definition bind_args (ps : list id) (args : list arg) (body : stm) : stm :=
  wrap_vals ps args (rename (ren_args ps args) body).

(** Renaming commutes with inversion. *)
Lemma invert_rename : forall r s, invert (rename r s) = rename r (invert s).
Proof.
  intros r s; induction s; simpl; try reflexivity;
    try (rewrite IHs1, IHs2; reflexivity);
    try (rewrite IHs; reflexivity).
Qed.

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
  intros a b x v (Hv & Ho & Hn & Hh & Hc); steq_split; simpl.
  - intro y; destruct (Nat.eqb x y); [ reflexivity | apply Hv ].
  - apply Ho.
  - apply Hn.
  - intros l f Hl; apply Hh; exact Hl.
  - intros l Hl; apply Hc; exact Hl.
Qed.

Lemma seto_steq : forall a b x r, a == b -> seto a x r == seto b x r.
Proof.
  intros a b x r (Hv & Ho & Hn & Hh & Hc); steq_split; simpl.
  - apply Hv.
  - intro y; destruct (Nat.eqb x y); [ reflexivity | apply Ho ].
  - apply Hn.
  - intros l f Hl; apply Hh; exact Hl.
  - intros l Hl; apply Hc; exact Hl.
Qed.

Lemma setf_steq : forall a b l f v, a == b -> setf a l f v == setf b l f v.
Proof.
  intros a b l f v (Hv & Ho & Hn & Hh & Hc); steq_split; simpl.
  - apply Hv.
  - apply Ho.
  - apply Hn.
  - intros l' f' Hl'.
    destruct (Nat.eqb l l' && Nat.eqb f f')%bool; [ reflexivity | apply Hh; exact Hl' ].
  - intros l' Hl'; apply Hc; exact Hl'.
Qed.

Lemma alloc_steq : forall a b c x, a == b -> alloc a c x == alloc b c x.
Proof.
  intros a b c x (Hv & Ho & Hn & Hh & Hcl); steq_split; simpl.
  - apply Hv.
  - intro y; destruct (Nat.eqb x y); [ rewrite Hn; reflexivity | apply Ho ].
  - rewrite Hn; reflexivity.
  - intros l f Hl; simpl in Hl; rewrite <- Hn.
    destruct (Nat.eqb l (hn a)) eqn:E; [ reflexivity | ].
    apply Hh; apply Nat.eqb_neq in E; lia.
  - intros l Hl; simpl in Hl; rewrite <- Hn.
    destruct (Nat.eqb l (hn a)) eqn:E; [ reflexivity | ].
    apply Hcl; apply Nat.eqb_neq in E; lia.
Qed.

Lemma dealloc_steq : forall a b x, a == b -> dealloc a x == dealloc b x.
Proof.
  intros a b x (Hv & Ho & Hn & Hh & Hc); steq_split; simpl.
  - apply Hv.
  - intro y; destruct (Nat.eqb x y); [ reflexivity | apply Ho ].
  - rewrite Hn; reflexivity.
  - intros l f Hl; apply Hh; simpl in Hl; lia.
  - intros l Hl; apply Hc; simpl in Hl; lia.
Qed.

(** Allocating then deallocating is the identity (the fresh cell is above
    the live prefix, so nothing observable changes). *)
Lemma dealloc_alloc : forall a c x, os a x = None -> dealloc (alloc a c x) x == a.
Proof.
  intros a c x Hx; steq_split; simpl.
  - reflexivity.
  - intro y; destruct (Nat.eqb x y) eqn:E; [ | reflexivity ].
    apply Nat.eqb_eq in E; subst; now rewrite Hx.
  - reflexivity.
  - intros l f Hl; destruct (Nat.eqb l (hn a)) eqn:E; [ | reflexivity ].
    apply Nat.eqb_eq in E; subst; lia.
  - intros l Hl; destruct (Nat.eqb l (hn a)) eqn:E; [ | reflexivity ].
    apply Nat.eqb_eq in E; subst; lia.
Qed.

(** Swapping two heap cells is an involution (the cells may coincide). *)
Lemma aswap_invol : forall a b l1 i1 l2 i2,
  (l1 < hn a)%nat -> (l2 < hn a)%nat ->
  b == setf (setf a l1 i1 (hp a l2 i2)) l2 i2 (hp a l1 i1) ->
  a == setf (setf b l1 i1 (hp b l2 i2)) l2 i2 (hp b l1 i1).
Proof.
  intros a b l1 i1 l2 i2 H1 H2 Hb.
  assert (Hhn : hn b = hn a) by (rewrite (steq_hn b _ Hb); reflexivity).
  assert (Hcell : forall l f, (l < hn a)%nat ->
    hp b l f = if (Nat.eqb l2 l && Nat.eqb i2 f)%bool then hp a l1 i1
               else if (Nat.eqb l1 l && Nat.eqb i1 f)%bool then hp a l2 i2
               else hp a l f).
  { intros l f Hl. rewrite (steq_hp b _ l f Hb) by (rewrite Hhn; assumption).
    simpl. reflexivity. }
  steq_split.
  - intro y; simpl; symmetry; rewrite (steq_vs b _ y Hb); reflexivity.
  - intro y; simpl; symmetry; rewrite (steq_os b _ y Hb); reflexivity.
  - simpl; symmetry; assumption.
  - intros l f Hl; simpl.
    assert (Hb2 : hp b l2 i2 = hp a l1 i1).
    { rewrite (Hcell l2 i2 H2); now rewrite Nat.eqb_refl, Nat.eqb_refl. }
    destruct (Nat.eqb l2 l && Nat.eqb i2 f)%bool eqn:E2.
    + apply andb_true_iff in E2 as [A B]; apply Nat.eqb_eq in A;
        apply Nat.eqb_eq in B; subst l f.
      rewrite (Hcell l1 i1 H1).
      destruct (Nat.eqb l2 l1 && Nat.eqb i2 i1)%bool eqn:E12.
      * apply andb_true_iff in E12 as [A B]; apply Nat.eqb_eq in A;
          apply Nat.eqb_eq in B; subst. reflexivity.
      * rewrite Nat.eqb_refl, Nat.eqb_refl; reflexivity.
    + destruct (Nat.eqb l1 l && Nat.eqb i1 f)%bool eqn:E1.
      * apply andb_true_iff in E1 as [A B]; apply Nat.eqb_eq in A;
          apply Nat.eqb_eq in B; subst l f. now rewrite Hb2.
      * rewrite (Hcell l f Hl), E2, E1. reflexivity.
  - intros l Hl; simpl; symmetry; apply (steq_hc b _ l Hb).
    rewrite Hhn; assumption.
Qed.

(* ------------------------------------------------------------------ *)
(** * Big-step operational semantics                                   *)
(* ------------------------------------------------------------------ *)

(** A method is a parameter list and a body.  For a method that is called on
    an object, the *first* formal parameter is its receiver (`this`), so
    dispatching is just the renaming that binds it to the actual object. *)
Inductive mdecl := MDecl (ps : list id) (body : stm).

(** A class: its superclass (if any) and its own methods. *)
Inductive cdecl := CDecl (parent : option cid) (methods : mid -> option mdecl).

Definition ctable := cid -> option cdecl.

(** Dynamic dispatch: look the method up in the object's *run-time* class and
    walk up the inheritance chain.  This is subtype polymorphism: the class
    that is searched is the one the object was constructed with, not the
    declared type of the variable. *)
Inductive dispatch (T : ctable) : cid -> mid -> mdecl -> Prop :=
| D_here : forall c p ms m d,
    T c = Some (CDecl p ms) -> ms m = Some d -> dispatch T c m d
| D_up : forall c q ms m d,
    T c = Some (CDecl (Some q) ms) -> ms m = None -> dispatch T q m d ->
    dispatch T c m d.

Lemma dispatch_det : forall T c m d1 d2,
  dispatch T c m d1 -> dispatch T c m d2 -> d1 = d2.
Proof.
  intros T c m d1 d2 H1; revert d2; induction H1; intros d2 H2; inversion H2; subst;
    match goal with
    | [ HA : T ?c = Some _, HB : T ?c = Some _ |- _ ] =>
        rewrite HA in HB; injection HB as E1 E2; subst
    end.
  - congruence.
  - congruence.
  - congruence.
  - now apply IHdispatch.
Qed.

(** 呼出し先の本体：受け手 x と実引数を仮引数へ束縛する（参照渡し）。 *)
Definition call_body (d : mdecl) (x : id) (args : list arg) : stm :=
  match d with MDecl ps body => bind_args ps (Aref x :: args) body end.

(** The environment a program runs in: free-standing methods and the class
    table. *)
(** 実行環境。[cells c] はクラス [c] の対象が持つセルの数——オブジェクトなら
    フィールド数、配列なら長さ。状態ではなく**クラス表から引く**ので、状態の
    形（と [==] の合同）を変えずに範囲検査を入れられる。 *)
Record menv := MEnv { procs : mid -> option mdecl;
                      classes : ctable;
                      cells : cid -> nat }.

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
| E_aassign : forall x ei o e a b l,
    os a x = Some l -> (l < hn a)%nat ->
    (* 添字は確保したセル数の範囲になければならない *)
    (Z.to_nat (eval ei a) < cells G (hc a l))%nat ->
    b == setf a l (Z.to_nat (eval ei a))
              (mapp o (hp a l (Z.to_nat (eval ei a))) (eval e a)) ->
    eval ei b = eval ei a ->
    eval e b = eval e a ->
    exec G (Saassign x ei o e) a b
| E_swap : forall x y a b,
    b == setv (setv a x (vs a y)) y (vs a x) ->
    exec G (Sswap x y) a b
| E_aswap : forall x e1 y e2 a b l1 l2,
    os a x = Some l1 -> (l1 < hn a)%nat ->
    (Z.to_nat (eval e1 a) < cells G (hc a l1))%nat ->
    os a y = Some l2 -> (l2 < hn a)%nat ->
    (Z.to_nat (eval e2 a) < cells G (hc a l2))%nat ->
    b == setf (setf a l1 (Z.to_nat (eval e1 a)) (hp a l2 (Z.to_nat (eval e2 a))))
              l2 (Z.to_nat (eval e2 a)) (hp a l1 (Z.to_nat (eval e1 a))) ->
    eval e1 b = eval e1 a ->
    eval e2 b = eval e2 a ->
    exec G (Saswap x e1 y e2) a b
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
(* show / print は状態を変えないので、状態変換としては恒等（それ自身が逆）。 *)
| E_show : forall e a b, a == b -> exec G (Sshow e) a b
| E_obj : forall cl x s a b c,
    os a x = None ->
    exec G s (alloc a cl x) b ->
    os b x = Some (hn a) ->
    hn b = S (hn a) ->
    (forall f, hp b (hn a) f = 0) ->
    hc b (hn a) = cl ->
    c == dealloc b x ->
    exec G (Sobj cl x s) a c
(* ブロックにしない [new] / [delete]。オブジェクトブロックの前半・後半を
   そのまま切り出したもの。ヒープはスタックなので [delete] は必ず一番上の
   対象を解放する（それを [os a x = Some (pred (hn a))] と [0 < hn a] が
   要求している）。ゼロクリア検査は [E_obj] と同じ。 *)
| E_new : forall cl x a b,
    os a x = None ->
    b == alloc a cl x ->
    exec G (Snew cl x) a b
| E_delete : forall cl x a b,
    os a x = Some (pred (hn a)) ->
    (0 < hn a)%nat ->
    (forall f, hp a (pred (hn a)) f = 0) ->
    hc a (pred (hn a)) = cl ->
    b == dealloc a x ->
    exec G (Sdelete cl x) a b
| E_call : forall m ps body args a b,
    procs G m = Some (MDecl ps body) ->
    length ps = length args ->
    exec G (bind_args ps args body) a b ->
    exec G (Scall m args) a b
| E_uncall : forall m ps body args a b,
    procs G m = Some (MDecl ps body) ->
    length ps = length args ->
    exec G (invert (bind_args ps args body)) a b ->
    exec G (Suncall m args) a b

(* 動的束縛つきのメソッド呼出し。受け手が呼出し中に動かないこと
   （ROOPL++ では this は代入できない）とヒープ高さが釣り合うことは，
   言語の構文が保証している性質をここでは意味論の側で述べている。 *)
| E_ocall : forall x m args a b l d,
    os a x = Some l -> (l < hn a)%nat ->
    dispatch (classes G) (hc a l) m d ->
    exec G (call_body d x args) a b ->
    os b x = Some l -> hc b l = hc a l -> hn b = hn a ->
    exec G (Socall x m args) a b
| E_ouncall : forall x m args a b l d,
    os a x = Some l -> (l < hn a)%nat ->
    dispatch (classes G) (hc a l) m d ->
    exec G (invert (call_body d x args)) a b ->
    os b x = Some l -> hc b l = hc a l -> hn b = hn a ->
    exec G (Souncall x m args) a b

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
  - (* array assign *)
    intros x ei o e a b l Hl Hlt Hbnd Hb Hei He a' b' Ha Hb'.
    eapply E_aassign with (l := l).
    + rewrite <- (steq_os a a' x Ha); assumption.
    + rewrite <- (steq_hn a a' Ha); assumption.
    + rewrite <- (eval_steq ei a a' Ha), <- (steq_hc a a' l Ha Hlt); assumption.
    + rewrite <- (eval_steq ei a a' Ha), <- (eval_steq e a a' Ha),
              <- (steq_hp a a' l (Z.to_nat (eval ei a)) Ha Hlt).
      eapply steq_rewrite
        with (b := b)
             (c := setf a l (Z.to_nat (eval ei a))
                        (mapp o (hp a l (Z.to_nat (eval ei a))) (eval e a)));
        [ assumption | apply setf_steq; assumption | assumption ].
    + rewrite <- (eval_steq ei b b' Hb'), <- (eval_steq ei a a' Ha); assumption.
    + rewrite <- (eval_steq e b b' Hb'), <- (eval_steq e a a' Ha); assumption.
  - (* int swap *)
    intros x y a b Hb a' b' Ha Hb'.
    apply E_swap.
    rewrite <- (steq_vs a a' x Ha), <- (steq_vs a a' y Ha).
    eapply steq_rewrite with (b := b) (c := setv (setv a x (vs a y)) y (vs a x));
      [ assumption | apply setv_steq; apply setv_steq; assumption | assumption ].
  - (* array swap *)
    intros x e1 y e2 a b l1 l2 Hx Hx1 Hb1 Hy Hy1 Hb2 Hb He1 He2 a' b' Ha Hb'.
    eapply E_aswap with (l1 := l1) (l2 := l2).
    + rewrite <- (steq_os a a' x Ha); assumption.
    + rewrite <- (steq_hn a a' Ha); assumption.
    + rewrite <- (eval_steq e1 a a' Ha), <- (steq_hc a a' l1 Ha Hx1); assumption.
    + rewrite <- (steq_os a a' y Ha); assumption.
    + rewrite <- (steq_hn a a' Ha); assumption.
    + rewrite <- (eval_steq e2 a a' Ha), <- (steq_hc a a' l2 Ha Hy1); assumption.
    + rewrite <- (eval_steq e1 a a' Ha), <- (eval_steq e2 a a' Ha),
              <- (steq_hp a a' l1 (Z.to_nat (eval e1 a)) Ha Hx1),
              <- (steq_hp a a' l2 (Z.to_nat (eval e2 a)) Ha Hy1).
      eapply steq_rewrite
        with (b := b)
             (c := setf (setf a l1 (Z.to_nat (eval e1 a))
                              (hp a l2 (Z.to_nat (eval e2 a))))
                        l2 (Z.to_nat (eval e2 a))
                        (hp a l1 (Z.to_nat (eval e1 a))));
        [ assumption | apply setf_steq; apply setf_steq; assumption | assumption ].
    + rewrite <- (eval_steq e1 b b' Hb'), <- (eval_steq e1 a a' Ha); assumption.
    + rewrite <- (eval_steq e2 b b' Hb'), <- (eval_steq e2 a a' Ha); assumption.
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
  - (* show / print *)
    intros e a b Hab a' b' Ha Hb. apply E_show; eauto.
  - (* object block *)
    intros cl x s a b c Hx Hs IH Hbx Hbn Hbz Hbc Hc a' c' Ha Hc'.
    eapply E_obj with (b := b).
    + rewrite <- (steq_os a a' x Ha); assumption.
    + apply IH; [ now apply alloc_steq | apply steq_refl ].
    + rewrite <- (steq_hn a a' Ha); assumption.
    + rewrite <- (steq_hn a a' Ha); assumption.
    + rewrite <- (steq_hn a a' Ha); assumption.
    + rewrite <- (steq_hn a a' Ha); assumption.
    + eapply steq_rewrite with (b := c) (c := dealloc b x);
        [ assumption | apply steq_refl | assumption ].
  - (* new *)
    intros cl x a b Hx Hb a' b' Ha Hb'.
    apply E_new.
    + rewrite <- (steq_os a a' x Ha); assumption.
    + apply (steq_trans b' b (alloc a' cl x));
        [ now apply steq_sym | ].
      apply (steq_trans b (alloc a cl x) (alloc a' cl x));
        [ assumption | now apply alloc_steq ].
  - (* delete *)
    intros cl x a b Hx Hpos Hz Hcc Hb a' b' Ha Hb'.
    assert (Hn : hn a = hn a') by (apply (steq_hn a a' Ha)).
    assert (Hlt : (pred (hn a) < hn a)%nat) by lia.
    apply E_delete.
    + rewrite <- (steq_os a a' x Ha), <- Hn; assumption.
    + rewrite <- Hn; assumption.
    + intro f; rewrite <- Hn, <- (steq_hp a a' (pred (hn a)) f Ha Hlt); apply Hz.
    + rewrite <- Hn, <- (steq_hc a a' (pred (hn a)) Ha Hlt); assumption.
    + apply (steq_trans b' b (dealloc a' x));
        [ now apply steq_sym | ].
      apply (steq_trans b (dealloc a x) (dealloc a' x));
        [ assumption | now apply dealloc_steq ].
  - (* call *)
    intros m ps body args a b Hm Hlen Hs IH a' b' Ha Hb. eapply E_call; eauto.
  - (* uncall *)
    intros m ps body args a b Hm Hlen Hs IH a' b' Ha Hb. eapply E_uncall; eauto.
  - (* object call *)
    intros x m args a b l d H1 H2 H3 H4 IH H5 H6 H7 a' b' Ha Hb.
    assert (Hlb : (l < hn b)%nat) by (rewrite H7; assumption).
    eapply E_ocall with (l := l) (d := d).
    + rewrite <- (steq_os a a' x Ha); assumption.
    + rewrite <- (steq_hn a a' Ha); assumption.
    + rewrite <- (steq_hc a a' l Ha H2); assumption.
    + apply IH; assumption.
    + rewrite <- (steq_os b b' x Hb); assumption.
    + rewrite <- (steq_hc b b' l Hb Hlb), <- (steq_hc a a' l Ha H2); assumption.
    + rewrite <- (steq_hn b b' Hb), <- (steq_hn a a' Ha); assumption.
  - (* object uncall *)
    intros x m args a b l d H1 H2 H3 H4 IH H5 H6 H7 a' b' Ha Hb.
    assert (Hlb : (l < hn b)%nat) by (rewrite H7; assumption).
    eapply E_ouncall with (l := l) (d := d).
    + rewrite <- (steq_os a a' x Ha); assumption.
    + rewrite <- (steq_hn a a' Ha); assumption.
    + rewrite <- (steq_hc a a' l Ha H2); assumption.
    + apply IH; assumption.
    + rewrite <- (steq_os b b' x Hb); assumption.
    + rewrite <- (steq_hc b b' l Hb Hlb), <- (steq_hc a a' l Ha H2); assumption.
    + rewrite <- (steq_hn b b' Hb), <- (steq_hn a a' Ha); assumption.
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
    steq_split; simpl.
    + intro y. destruct (Nat.eqb x y) eqn:E.
      * apply Nat.eqb_eq in E; subst y; reflexivity.
      * rewrite (steq_vs b _ y Hb); simpl; now rewrite E.
    + intro y; symmetry; rewrite (steq_os b _ y Hb); reflexivity.
    + symmetry; rewrite (steq_hn b _ Hb); reflexivity.
    + intros l f Hl; symmetry.
      rewrite (steq_hp b _ l f Hb); [ reflexivity | ].
      rewrite (steq_hn b _ Hb); simpl; assumption.
    + hc_auto.
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
      steq_split; simpl.
      * intro y; symmetry; rewrite (steq_vs b _ y Hb); reflexivity.
      * intro y; symmetry; rewrite (steq_os b _ y Hb); reflexivity.
      * symmetry; assumption.
      * intros l' f' Hl'; destruct (Nat.eqb l l' && Nat.eqb f f')%bool eqn:E.
        -- apply andb_true_iff in E as [E1 E2].
           apply Nat.eqb_eq in E1; apply Nat.eqb_eq in E2; subst; reflexivity.
        -- symmetry. rewrite (steq_hp b _ l' f' Hb) by (rewrite Hhn; assumption).
           simpl. now rewrite E.
      * hc_auto.
  - (* array assign *)
    intros x ei o e a b l Hl Hlt Hbnd Hb Hei He.
    assert (Hhn : hn b = hn a) by (rewrite (steq_hn b _ Hb); reflexivity).
    assert (Hhc : hc b l = hc a l)
      by (rewrite (steq_hc b _ l Hb) by (rewrite Hhn; assumption); reflexivity).
    assert (Hidx : Z.to_nat (eval ei b) = Z.to_nat (eval ei a)) by (now rewrite Hei).
    assert (Hf : hp b l (Z.to_nat (eval ei a))
                 = mapp o (hp a l (Z.to_nat (eval ei a))) (eval e a)).
    { rewrite (steq_hp b _ l (Z.to_nat (eval ei a)) Hb) by (rewrite Hhn; assumption).
      simpl. now rewrite Nat.eqb_refl, Nat.eqb_refl. }
    eapply E_aassign with (l := l).
    + rewrite (steq_os b _ x Hb); simpl; assumption.
    + rewrite Hhn; assumption.
    + rewrite Hidx, Hhc; assumption.
    + rewrite Hidx, Hf, He, mapp_minv.
      steq_split; simpl.
      * intro y; symmetry; rewrite (steq_vs b _ y Hb); reflexivity.
      * intro y; symmetry; rewrite (steq_os b _ y Hb); reflexivity.
      * symmetry; assumption.
      * intros l' f' Hl';
          destruct (Nat.eqb l l' && Nat.eqb (Z.to_nat (eval ei a)) f')%bool eqn:E.
        -- apply andb_true_iff in E as [E1 E2]; apply Nat.eqb_eq in E1;
             apply Nat.eqb_eq in E2; subst; reflexivity.
        -- symmetry. rewrite (steq_hp b _ l' f' Hb) by (rewrite Hhn; assumption).
           simpl. now rewrite E.
      * hc_auto.
    + now rewrite Hei.
    + now rewrite He.
  - (* int swap *)
    intros x y a b Hb. apply E_swap.
    assert (Hbx : vs b x = if Nat.eqb y x then vs a x else vs a y).
    { rewrite (steq_vs b _ x Hb); simpl; rewrite Nat.eqb_refl.
      destruct (Nat.eqb y x); reflexivity. }
    assert (Hby : vs b y = vs a x)
      by (rewrite (steq_vs b _ y Hb); simpl; now rewrite Nat.eqb_refl).
    rewrite Hbx, Hby.
    steq_split; simpl.
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
    + hc_auto.
  - (* array swap *)
    intros x e1 y e2 a b l1 l2 Hx Hx1 Hb1 Hy Hy1 Hb2 Hb He1 He2.
    assert (Hhn : hn b = hn a) by (rewrite (steq_hn b _ Hb); reflexivity).
    assert (Hc1 : hc b l1 = hc a l1)
      by (rewrite (steq_hc b _ l1 Hb) by (rewrite Hhn; assumption); reflexivity).
    assert (Hc2 : hc b l2 = hc a l2)
      by (rewrite (steq_hc b _ l2 Hb) by (rewrite Hhn; assumption); reflexivity).
    eapply E_aswap with (l1 := l1) (l2 := l2).
    + rewrite (steq_os b _ x Hb); simpl; assumption.
    + rewrite Hhn; assumption.
    + rewrite He1, Hc1; assumption.
    + rewrite (steq_os b _ y Hb); simpl; assumption.
    + rewrite Hhn; assumption.
    + rewrite He2, Hc2; assumption.
    + rewrite He1, He2. apply aswap_invol; assumption.
    + now rewrite He1.
    + now rewrite He2.
  - (* object swap *)
    intros x y a b Hb. apply E_oswap.
    assert (Hbx : os b x = if Nat.eqb y x then os a x else os a y).
    { rewrite (steq_os b _ x Hb); simpl; rewrite Nat.eqb_refl.
      destruct (Nat.eqb y x); reflexivity. }
    assert (Hby : os b y = os a x)
      by (rewrite (steq_os b _ y Hb); simpl; now rewrite Nat.eqb_refl).
    rewrite Hbx, Hby.
    steq_split; simpl.
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
    + hc_auto.
  - (* copy *)
    intros x y a b Hxy Hy Hb. apply E_uncopy; auto.
    + assert (Hbx : os b x = os a x).
      { rewrite (steq_os b _ x Hb); simpl.
        destruct (Nat.eqb y x) eqn:E; [ apply Nat.eqb_eq in E; congruence | reflexivity ]. }
      assert (Hby : os b y = os a x)
        by (rewrite (steq_os b _ y Hb); simpl; now rewrite Nat.eqb_refl).
      now rewrite Hbx, Hby.
    + steq_split; simpl.
      * intro z; symmetry; rewrite (steq_vs b _ z Hb); reflexivity.
      * intro z; destruct (Nat.eqb y z) eqn:E.
        -- apply Nat.eqb_eq in E; subst z; now rewrite Hy.
        -- symmetry; rewrite (steq_os b _ z Hb); simpl; now rewrite E.
      * symmetry; rewrite (steq_hn b _ Hb); reflexivity.
      * intros l f Hl; symmetry. rewrite (steq_hp b _ l f Hb); [ reflexivity | ].
        rewrite (steq_hn b _ Hb); simpl; assumption.
      * hc_auto.
  - (* uncopy *)
    intros x y a b Hxy Hxy2 Hb. apply E_copy; auto.
    + rewrite (steq_os b _ y Hb); simpl; now rewrite Nat.eqb_refl.
    + assert (Hbx : os b x = os a x).
      { rewrite (steq_os b _ x Hb); simpl.
        destruct (Nat.eqb y x) eqn:E; [ apply Nat.eqb_eq in E; congruence | reflexivity ]. }
      rewrite Hbx, Hxy2.
      steq_split; simpl.
      * intro z; symmetry; rewrite (steq_vs b _ z Hb); reflexivity.
      * intro z; destruct (Nat.eqb y z) eqn:E.
        -- apply Nat.eqb_eq in E; subst z; reflexivity.
        -- symmetry; rewrite (steq_os b _ z Hb); simpl; now rewrite E.
      * symmetry; rewrite (steq_hn b _ Hb); reflexivity.
      * intros l f Hl; symmetry. rewrite (steq_hp b _ l f Hb); [ reflexivity | ].
        rewrite (steq_hn b _ Hb); simpl; assumption.
      * hc_auto.
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
      steq_split; simpl.
      * intro y; destruct (Nat.eqb x y) eqn:E.
        -- apply Nat.eqb_eq in E; subst y. rewrite Hec, <- Hx; reflexivity.
        -- rewrite (steq_vs c _ y Hc); simpl; now rewrite E.
      * intro y; rewrite (steq_os c _ y Hc); reflexivity.
      * rewrite (steq_hn c _ Hc); reflexivity.
      * intros l f Hl; symmetry; apply (steq_hp c _ l f Hc).
        rewrite (steq_hn c _ Hc); simpl; assumption.
      * hc_auto.
    + simpl; rewrite Nat.eqb_refl; symmetry; now apply eval_setv_notin.
    + assert (Hcx : vs c x = vs a x)
        by (rewrite (steq_vs c _ x Hc); simpl; now rewrite Nat.eqb_refl).
      rewrite Hcx.
      steq_split; simpl.
      * intro y; destruct (Nat.eqb x y) eqn:E;
          [ apply Nat.eqb_eq in E; subst y | ]; reflexivity.
      * reflexivity.
      * reflexivity.
      * reflexivity.
      * hc_auto.
  - (* show / print *)
    intros e a b Hab. apply E_show. now apply steq_sym.
  - (* object block *)
    intros cl x s a b c Hx Hs IH Hbx Hbn Hbz Hbc Hc. simpl.
    assert (Hcn : hn c = hn a).
    { rewrite (steq_hn c _ Hc); simpl; rewrite Hbn; reflexivity. }
    assert (Halloc : alloc c cl x == b).
    { steq_split; simpl.
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
            apply Nat.eqb_neq in E. rewrite Hcn in E. simpl in Hl. lia.
      - intros l Hl; destruct (Nat.eqb l (hn c)) eqn:E.
        + apply Nat.eqb_eq in E; subst l. rewrite Hcn; symmetry; apply Hbc.
        + rewrite (steq_hc c _ l Hc).
          * reflexivity.
          * rewrite (steq_hn c _ Hc); simpl. rewrite Hbn; simpl.
            apply Nat.eqb_neq in E. rewrite Hcn in E. simpl in Hl. lia. }
    eapply E_obj with (b := alloc a cl x).
    + rewrite (steq_os c _ x Hc); simpl; now rewrite Nat.eqb_refl.
    + eapply exec_eq; [ eassumption | now apply steq_sym | apply steq_refl ].
    + simpl; rewrite Nat.eqb_refl, Hcn; reflexivity.
    + simpl; rewrite Hcn; reflexivity.
    + intro f; simpl; rewrite Hcn, Nat.eqb_refl; reflexivity.
    + simpl; rewrite Hcn, Nat.eqb_refl; reflexivity.
    + apply steq_sym; now apply dealloc_alloc.
  - (* new の逆は delete *)
    intros cl x a b Hx Hb. simpl.
    assert (Hbn : hn b = S (hn a)) by (rewrite (steq_hn b _ Hb); reflexivity).
    apply E_delete.
    + rewrite Hbn; simpl.
      rewrite (steq_os b _ x Hb); simpl; now rewrite Nat.eqb_refl.
    + rewrite Hbn; lia.
    + intro f; rewrite Hbn; simpl.
      rewrite (steq_hp b _ (hn a) f Hb); [ simpl; now rewrite Nat.eqb_refl | ].
      rewrite Hbn; lia.
    + rewrite Hbn; simpl.
      rewrite (steq_hc b _ (hn a) Hb); [ simpl; now rewrite Nat.eqb_refl | ].
      rewrite Hbn; lia.
    + apply steq_sym.
      apply (steq_trans (dealloc b x) (dealloc (alloc a cl x) x) a);
        [ now apply dealloc_steq | now apply dealloc_alloc ].
  - (* delete の逆は new。解放した対象を同じ位置に取り直せることが要 *)
    intros cl x a b Hx Hpos Hz Hcc Hb. simpl.
    assert (Hbn : hn b = pred (hn a)) by (rewrite (steq_hn b _ Hb); reflexivity).
    assert (Halloc : alloc b cl x == a).
    { steq_split; simpl.
      - intro y; rewrite (steq_vs b _ y Hb); reflexivity.
      - intro y; destruct (Nat.eqb x y) eqn:E.
        + apply Nat.eqb_eq in E; subst y. rewrite Hbn; symmetry; assumption.
        + rewrite (steq_os b _ y Hb); simpl; now rewrite E.
      - rewrite Hbn; lia.
      - intros l f Hl; destruct (Nat.eqb l (hn b)) eqn:E.
        + apply Nat.eqb_eq in E; subst l. rewrite Hbn; symmetry; apply Hz.
        + rewrite (steq_hp b _ l f Hb); [ reflexivity | ].
          apply Nat.eqb_neq in E; simpl in Hl; lia.
      - intros l Hl; destruct (Nat.eqb l (hn b)) eqn:E.
        + apply Nat.eqb_eq in E; subst l. rewrite Hbn; symmetry; apply Hcc.
        + rewrite (steq_hc b _ l Hb); [ reflexivity | ].
          apply Nat.eqb_neq in E; simpl in Hl; lia. }
    apply E_new.
    + rewrite (steq_os b _ x Hb); simpl; now rewrite Nat.eqb_refl.
    + now apply steq_sym.
  - (* call *)
    intros m ps body args a b Hm Hlen Hs IH. simpl. eapply E_uncall; eassumption.
  - (* uncall *)
    intros m ps body args a b Hm Hlen Hs IH. simpl.
    eapply E_call; [ eassumption | eassumption | ].
    rewrite invert_invert in IH; assumption.
  - (* object call *)
    intros x m args a b l d H1 H2 H3 H4 IH H5 H6 H7. simpl.
    eapply E_ouncall with (l := l) (d := d); try assumption.
    + rewrite H7; assumption.
    + rewrite H6; assumption.
    + now symmetry.
    + now symmetry.
  - (* object uncall *)
    intros x m args a b l d H1 H2 H3 H4 IH H5 H6 H7. simpl.
    eapply E_ocall with (l := l) (d := d); try assumption.
    + rewrite H7; assumption.
    + rewrite H6; assumption.
    + rewrite invert_invert in IH; assumption.
    + now symmetry.
    + now symmetry.
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
  - (* array assign *)
    intros x ei o e a b l Hl Hlt Hbnd Hb Hei He b' H; inversion H; subst.
    assert (l0 = l) by congruence; subst l0; eauto.
  - (* int swap *)
    intros x y a b Hb b' H; inversion H; subst; eauto.
  - (* array swap *)
    intros x e1 y e2 a b l1 l2 Hx Hx1 Hb1 Hy Hy1 Hb2 Hb He1 He2 b' H;
      inversion H; subst.
    assert (l0 = l1) by congruence; assert (l3 = l2) by congruence; subst; eauto.
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
  - (* show / print *)
    intros e a b Hab b' H; inversion H; subst; eauto.
  - (* object block *)
    intros cl x s a b c Hx Hs IH Hbx Hbn Hbz Hbc Hc c' H; inversion H; subst.
    eapply steq_trans; [ eassumption | ].
    eapply steq_trans; [ | apply steq_sym; eassumption ].
    apply dealloc_steq. now apply IH.
  - (* new *)
    intros cl x a b Hx Hb b' H; inversion H; subst.
    eapply steq_trans; [ eassumption | now apply steq_sym ].
  - (* delete *)
    intros cl x a b Hx Hpos Hz Hcc Hb b' H; inversion H; subst.
    eapply steq_trans; [ eassumption | now apply steq_sym ].
  - (* call *)
    intros m ps body args a b Hm Hlen Hs IH b' H; inversion H; subst.
    match goal with
    | [ HG : procs G m = Some (MDecl _ _) |- _ ] =>
        rewrite Hm in HG; injection HG as E1 E2; subst
    end.
    now apply IH.
  - (* uncall *)
    intros m ps body args a b Hm Hlen Hs IH b' H; inversion H; subst.
    match goal with
    | [ HG : procs G m = Some (MDecl _ _) |- _ ] =>
        rewrite Hm in HG; injection HG as E1 E2; subst
    end.
    now apply IH.
  - (* object call *)
    intros x m args a b l d H1 H2 H3 H4 IH H5 H6 H7 b' H; inversion H; subst.
    match goal with
    | [ HA : os a x = Some ?l0 |- _ ] => assert (l0 = l) by congruence
    end.
    subst.
    match goal with
    | [ HD : dispatch _ _ m ?d0 |- _ ] => assert (d0 = d) by (eapply dispatch_det; eassumption)
    end.
    subst. now apply IH.
  - (* object uncall *)
    intros x m args a b l d H1 H2 H3 H4 IH H5 H6 H7 b' H; inversion H; subst.
    match goal with
    | [ HA : os a x = Some ?l0 |- _ ] => assert (l0 = l) by congruence
    end.
    subst.
    match goal with
    | [ HD : dispatch _ _ m ?d0 |- _ ] => assert (d0 = d) by (eapply dispatch_det; eassumption)
    end.
    subst. now apply IH.
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
(** * An executable interpreter, proved sound against the semantics    *)
(* ------------------------------------------------------------------ *)

(** [exec] is a relation, so it cannot be run.  [run] is an executable
    interpreter -- extracted to OCaml by coq/extract.v -- and [run_sound]
    says every state it produces is one the semantics allows.  Reversibility
    then transfers to it: whatever [run] computes is backed by an [exec]
    derivation, hence injective, and its inverse runs backwards.

    Fuel bounds the recursion so the definition is total.  [None] means
    either "out of fuel" or "this program has no derivation", which for a
    reversible language is the normal way to reject a program.

    Object blocks, field/array updates and dynamic dispatch return [None]:
    the destruct rule quantifies over *all* fields, which is not decidable
    in this model.  Giving each object a cell count would make it decidable
    -- see coq/README.md. *)

(** 動的束縛の計算版。関係 [dispatch] は継承の鎖をさかのぼるので、
    鎖の長さぶんの燃料で打ち切る。 *)
Fixpoint dispatch_fn (fuel : nat) (T : ctable) (c : cid) (m : mid)
  : option mdecl :=
  match fuel with
  | O => None
  | S k =>
      match T c with
      | Some (CDecl p ms) =>
          match ms m with
          | Some d => Some d
          | None =>
              match p with
              | Some q => dispatch_fn k T q m
              | None => None
              end
          end
      | None => None
      end
  end.

Lemma dispatch_fn_sound : forall fuel T c m d,
  dispatch_fn fuel T c m = Some d -> dispatch T c m d.
Proof.
  induction fuel as [ | k IH ]; intros T c m d H; simpl in H; [ discriminate | ].
  destruct (T c) as [ [ p ms ] | ] eqn:Hc; [ | discriminate ].
  destruct (ms m) as [ d0 | ] eqn:Hm.
  - injection H as Heq; subst d0. eapply D_here; eassumption.
  - destruct p as [ q | ]; [ | discriminate ].
    eapply D_up; [ eassumption | eassumption | now apply IH ].
Qed.

(** オブジェクト参照の等価判定（[copy]/[uncopy] の副条件に要る）。 *)
Definition oloc_eqb (r1 r2 : option loc) : bool :=
  match r1, r2 with
  | None, None => true
  | Some l1, Some l2 => Nat.eqb l1 l2
  | _, _ => false
  end.

Lemma oloc_eqb_eq : forall r1 r2, oloc_eqb r1 r2 = true -> r1 = r2.
Proof.
  intros [ l1 | ] [ l2 | ] H; simpl in H; try discriminate; [ | reflexivity ].
  apply Nat.eqb_eq in H; now subst.
Qed.

(** 実行中に書き込みうるフィールド番号の上限。オブジェクトブロックの出口で
    「確保した対象の全フィールドがゼロ」を確かめる必要があるが、フィールドは
    自然数なので [forall f, hp b l f = 0] はそのままでは決定できない。そこで
    **上限 [nf] を実行と一緒に持ち回り**、[nf] 未満だけを実際に調べる
    （[nf] 以上は確保時の 0 のまま、という不変条件 [above] が保証する）。 *)
Definition above (nf : nat) (a : state) : Prop :=
  forall l f, (nf <= f)%nat -> hp a l f = 0.

Lemma above_setv : forall nf a x v, above nf a -> above nf (setv a x v).
Proof. intros nf a x v H l f Hf; apply H; assumption. Qed.

Lemma above_seto : forall nf a x r, above nf a -> above nf (seto a x r).
Proof. intros nf a x r H l f Hf; apply H; assumption. Qed.

Lemma above_setf : forall nf a l f v,
  above nf a -> above (Nat.max (S f) nf) (setf a l f v).
Proof.
  intros nf a l0 f0 v H l f Hf; simpl.
  destruct (Nat.eqb l0 l) eqn:El; destruct (Nat.eqb f0 f) eqn:Ef; simpl;
    try (apply H; lia).
  apply Nat.eqb_eq in Ef; subst f0; lia.
Qed.

Lemma above_mono : forall nf nf' a,
  (nf <= nf')%nat -> above nf a -> above nf' a.
Proof. intros nf nf' a Hle H l f Hf; apply H; lia. Qed.

Lemma above_alloc : forall nf a cl x, above nf a -> above nf (alloc a cl x).
Proof.
  intros nf a cl x H l f Hf; simpl.
  destruct (Nat.eqb l (hn a)); [ reflexivity | apply H; assumption ].
Qed.

Lemma above_dealloc : forall nf a x, above nf a -> above nf (dealloc a x).
Proof. intros nf a x H l f Hf; apply H; assumption. Qed.

(** 式の中の読み出しが範囲内か（決定可能）。

    意味論 [exec] はこれを要求しない。範囲外の読み出しは全域なヒープから値が
    読めるだけで、決定性も可逆性も壊さないからである（壊すのは書き込み側で、
    そちらは [E_aassign] / [E_aswap] の前提で塞いである）。一方で実装
    (lib/eval.ml) は読み出しも検査して落ちるので、**実行可能インタプリタの側で
    同じ検査を入れて実装に合わせる**。[run_sound] は「run が返す状態は必ず
    exec が許す」という片側の含意なので、run を厳しくしても成り立つ。 *)
Fixpoint inb (G : menv) (a : state) (e : exp) : bool :=
  match e with
  | Cst _ => true
  | Var _ => true
  | Fld x f =>
      match os a x with
      | Some l => andb (Nat.ltb l (hn a)) (Nat.ltb f (cells G (hc a l)))
      | None => false
      end
  | Idx x e' =>
      andb (inb G a e')
           (match os a x with
            | Some l => andb (Nat.ltb l (hn a))
                             (Nat.ltb (Z.to_nat (eval e' a)) (cells G (hc a l)))
            | None => false
            end)
  | Bop _ e1 e2 => andb (inb G a e1) (inb G a e2)
  end.

(** 複数の式をまとめて検査する。 *)
Definition inb2 (G : menv) (a : state) (e1 e2 : exp) : bool :=
  andb (inb G a e1) (inb G a e2).

(** 実行可能インタプリタ。状態と一緒にフィールド番号の上限を返す。 *)
Fixpoint run (fuel : nat) (G : menv) (s : stm) (a : state) (nf : nat)
              {struct fuel} : option (state * nat) :=
  match fuel with
  | O => None
  | S k =>
    match s with
    | Sskip => Some (a, nf)
    | Sshow _ => Some (a, nf)
    | Sassign x o e =>
        if in_dec Nat.eq_dec x (fv e) then None
        else if negb (inb G a e) then None
        else Some (setv a x (mapp o (vs a x) (eval e a)), nf)
    | Sswap x y => Some (setv (setv a x (vs a y)) y (vs a x), nf)
    (* フィールド・配列・オブジェクト参照。副条件はどれも決定可能で、
       規則が [b == …] の形で許す状態のうち、右辺そのものを返せばよい。 *)
    | Sfassign x f o e =>
        if negb (inb G a e) then None else
        match os a x with
        | Some l =>
            if andb (Nat.ltb l (hn a)) (Nat.ltb f (cells G (hc a l))) then
              let b := setf a l f (mapp o (hp a l f) (eval e a)) in
              if Z.eqb (eval e b) (eval e a)
              then Some (b, Nat.max (S f) nf) else None
            else None
        | None => None
        end
    | Saassign x ei o e =>
        if negb (inb2 G a ei e) then None else
        match os a x with
        | Some l =>
            let i := Z.to_nat (eval ei a) in
            if andb (Nat.ltb l (hn a)) (Nat.ltb i (cells G (hc a l))) then
              let b := setf a l i (mapp o (hp a l i) (eval e a)) in
              if andb (Z.eqb (eval ei b) (eval ei a)) (Z.eqb (eval e b) (eval e a))
              then Some (b, Nat.max (S i) nf) else None
            else None
        | None => None
        end
    | Saswap x e1 y e2 =>
        if negb (inb2 G a e1 e2) then None else
        match os a x, os a y with
        | Some l1, Some l2 =>
            let i1 := Z.to_nat (eval e1 a) in
            let i2 := Z.to_nat (eval e2 a) in
            if andb (andb (Nat.ltb l1 (hn a)) (Nat.ltb i1 (cells G (hc a l1))))
                    (andb (Nat.ltb l2 (hn a)) (Nat.ltb i2 (cells G (hc a l2)))) then
              let b := setf (setf a l1 i1 (hp a l2 i2)) l2 i2 (hp a l1 i1) in
              if andb (Z.eqb (eval e1 b) (eval e1 a)) (Z.eqb (eval e2 b) (eval e2 a))
              then Some (b, Nat.max (S i2) (Nat.max (S i1) nf)) else None
            else None
        | _, _ => None
        end
    | Soswap x y => Some (seto (seto a x (os a y)) y (os a x), nf)
    | Scopy x y =>
        if Nat.eqb x y then None
        else match os a y with
             | None => Some (seto a y (os a x), nf)
             | Some _ => None
             end
    | Suncopy x y =>
        if Nat.eqb x y then None
        else if oloc_eqb (os a x) (os a y) then Some (seto a y None, nf) else None
    | Sseq s1 s2 =>
        match run k G s1 a nf with
        | Some (b, nf1) => run k G s2 b nf1
        | None => None
        end
    | Sif e1 s1 s2 e2 =>
        if negb (inb G a e1) then None else
        if Z.eqb (eval e1 a) 0
        then match run k G s2 a nf with
             | Some (b, nf1) => if Z.eqb (eval e2 b) 0 then Some (b, nf1) else None
             | None => None
             end
        else match run k G s1 a nf with
             | Some (b, nf1) => if Z.eqb (eval e2 b) 0 then None else Some (b, nf1)
             | None => None
             end
    | Sloop e1 s1 s2 e2 =>
        if negb (inb G a e1) then None else
        if Z.eqb (eval e1 a) 0 then None
        else match run k G s1 a nf with
             | Some (b, nf1) => run_loop k G e1 s1 s2 e2 b nf1
             | None => None
             end
    | Slocal x e1 s' e2 =>
        if in_dec Nat.eq_dec x (fv e1) then None
        else if in_dec Nat.eq_dec x (fv e2) then None
        else if negb (inb G a e1) then None
        else match run k G s' (setv a x (eval e1 a)) nf with
             | Some (b, nf1) =>
                 if Z.eqb (vs b x) (eval e2 b)
                 then Some (setv b x (vs a x), nf1)
                 else None
             | None => None
             end
    (* オブジェクトブロック。出口の「全フィールドがゼロ」は、持ち回った上限
       [nf1] 未満を実際に調べれば足りる（[above] より [nf1] 以上は 0）。 *)
    | Sobj cl x s' =>
        match os a x with
        | None =>
            match run k G s' (alloc a cl x) nf with
            | Some (b, nf1) =>
                if andb (oloc_eqb (os b x) (Some (hn a)))
                   (andb (Nat.eqb (hn b) (S (hn a)))
                   (andb (Nat.eqb (hc b (hn a)) cl)
                         (forallb (fun f => Z.eqb (hp b (hn a) f) 0) (seq 0 nf1))))
                then Some (dealloc b x, nf1) else None
            | None => None
            end
        | Some _ => None
        end
    (* ブロックにしない new / delete。オブジェクトブロックの前半・後半と同じ *)
    | Snew cl x =>
        match os a x with
        | None => Some (alloc a cl x, nf)
        | Some _ => None
        end
    | Sdelete cl x =>
        match os a x with
        | Some l =>
            if andb (Nat.ltb 0 (hn a))
               (andb (Nat.eqb l (pred (hn a)))
               (andb (Nat.eqb (hc a (pred (hn a))) cl)
                     (forallb (fun f => Z.eqb (hp a (pred (hn a)) f) 0)
                              (seq 0 nf))))
            then Some (dealloc a x, nf) else None
        | None => None
        end
    | Scall m args =>
        match procs G m with
        | Some (MDecl ps body) =>
            if Nat.eqb (length ps) (length args)
            then run k G (bind_args ps args body) a nf
            else None
        | None => None
        end
    | Suncall m args =>
        match procs G m with
        | Some (MDecl ps body) =>
            if Nat.eqb (length ps) (length args)
            then run k G (invert (bind_args ps args body)) a nf
            else None
        | None => None
        end
    (* 動的束縛つきのメソッド呼出し。受け手が呼出し中に動かないことと
       ヒープの高さが釣り合うことを、出口で確かめる。 *)
    | Socall x m args =>
        match os a x with
        | Some l =>
            if Nat.ltb l (hn a) then
              match dispatch_fn k (classes G) (hc a l) m with
              | Some d =>
                  match run k G (call_body d x args) a nf with
                  | Some (b, nf1) =>
                      if andb (oloc_eqb (os b x) (Some l))
                              (andb (Nat.eqb (hc b l) (hc a l))
                                    (Nat.eqb (hn b) (hn a)))
                      then Some (b, nf1) else None
                  | None => None
                  end
              | None => None
              end
            else None
        | None => None
        end
    | Souncall x m args =>
        match os a x with
        | Some l =>
            if Nat.ltb l (hn a) then
              match dispatch_fn k (classes G) (hc a l) m with
              | Some d =>
                  match run k G (invert (call_body d x args)) a nf with
                  | Some (b, nf1) =>
                      if andb (oloc_eqb (os b x) (Some l))
                              (andb (Nat.eqb (hc b l) (hc a l))
                                    (Nat.eqb (hn b) (hn a)))
                      then Some (b, nf1) else None
                  | None => None
                  end
              | None => None
              end
            else None
        | None => None
        end
    end
  end

with run_loop (fuel : nat) (G : menv) (e1 : exp) (s1 s2 : stm) (e2 : exp)
              (a : state) (nf : nat) {struct fuel} : option (state * nat) :=
  match fuel with
  | O => None
  | S k =>
    if Z.eqb (eval e2 a) 0
    then match run k G s2 a nf with
         | Some (b, nf1) =>
             if Z.eqb (eval e1 b) 0
             then match run k G s1 b nf1 with
                  | Some (c, nf2) => run_loop k G e1 s1 s2 e2 c nf2
                  | None => None
                  end
             else None
         | None => None
         end
    else Some (a, nf)
  end.

(** 持ち回った上限は本物：実行後の状態でも、上限以上のフィールドは 0 のまま。 *)
Lemma run_above : forall fuel G,
  (forall s a nf b nf', run fuel G s a nf = Some (b, nf') ->
     above nf a -> above nf' b)
  /\ (forall e1 s1 s2 e2 a nf b nf',
        run_loop fuel G e1 s1 s2 e2 a nf = Some (b, nf') ->
        above nf a -> above nf' b).
Proof.
  induction fuel as [ | k IH ]; intro G.
  - split; intros; discriminate.
  - destruct (IH G) as [ IHrun IHloop ]. split.
    + intros s a nf b nf' H Ha; destruct s; simpl in H;
        (* 状態を作らない文・オブジェクト参照だけを動かす文 *)
        try (injection H as Heq Hn; subst;
             solve [ assumption
                   | now apply above_setv
                   | now repeat apply above_setv
                   | now apply above_seto
                   | now repeat apply above_seto ]).
      * (* assign *)
        destruct (in_dec Nat.eq_dec x (fv e)); [ discriminate | ].
        repeat (match type of H with
                | context [ if ?c then _ else _ ] => destruct c eqn:?; try discriminate
                end).
        injection H as Heq Hn; subst; now apply above_setv.
      * (* field assignment *)
        destruct (negb (inb G a e)) eqn:Hin; try discriminate.
        destruct (os a x) as [ l | ] eqn:Hx; try discriminate.
        repeat (match type of H with
                | context [ if ?c then _ else _ ] => destruct c eqn:?; try discriminate
                end).
        injection H as Heq Hn; subst; now apply above_setf.
      * (* array assignment *)
        destruct (negb (inb2 G a ei e)) eqn:Hin; try discriminate.
        destruct (os a x) as [ l | ] eqn:Hx; try discriminate.
        repeat (match type of H with
                | context [ if ?c then _ else _ ] => destruct c eqn:?; try discriminate
                end).
        injection H as Heq Hn; subst; now apply above_setf.
      * (* array element swap *)
        destruct (negb (inb2 G a e1 e2)) eqn:Hin; try discriminate.
        destruct (os a x) as [ l1 | ] eqn:Hx; try discriminate.
        destruct (os a y) as [ l2 | ] eqn:Hy; try discriminate.
        repeat (match type of H with
                | context [ if ?c then _ else _ ] => destruct c eqn:?; try discriminate
                end).
        injection H as Heq Hn; subst.
        (* 上限は setf の適用順（外側が e2 の添字）に合わせてある *)
        apply above_setf, above_setf; assumption.
      * (* copy *)
        destruct (Nat.eqb x y); try discriminate.
        destruct (os a y) as [ l | ] eqn:Hy; try discriminate.
        injection H as Heq Hn; subst; now apply above_seto.
      * (* uncopy *)
        destruct (Nat.eqb x y); try discriminate.
        destruct (oloc_eqb (os a x) (os a y)); try discriminate.
        injection H as Heq Hn; subst; now apply above_seto.
      * (* seq *)
        destruct (run k G s1 a nf) as [ [ m n1 ] | ] eqn:R1; try discriminate.
        eapply IHrun; [ eassumption | ]. eapply IHrun; eassumption.
      * (* if *)
        destruct (negb (inb G a e1)) eqn:Hin; try discriminate.
        destruct (Z.eqb (eval e1 a) 0) eqn:E1.
        -- destruct (run k G s2 a nf) as [ [ m n1 ] | ] eqn:R; try discriminate.
           destruct (Z.eqb (eval e2 m) 0); try discriminate.
           injection H as Heq Hn; subst. eapply IHrun; eassumption.
        -- destruct (run k G s1 a nf) as [ [ m n1 ] | ] eqn:R; try discriminate.
           destruct (Z.eqb (eval e2 m) 0); try discriminate.
           injection H as Heq Hn; subst. eapply IHrun; eassumption.
      * (* loop *)
        destruct (negb (inb G a e1)) eqn:Hin; try discriminate.
        destruct (Z.eqb (eval e1 a) 0); try discriminate.
        destruct (run k G s1 a nf) as [ [ m n1 ] | ] eqn:R; try discriminate.
        eapply IHloop; [ eassumption | ]. eapply IHrun; eassumption.
      * (* local *)
        destruct (in_dec Nat.eq_dec x (fv e1)); try discriminate.
        destruct (in_dec Nat.eq_dec x (fv e2)); try discriminate.
        destruct (negb (inb G a e1)) eqn:Hin; try discriminate.
        destruct (run k G s (setv a x (eval e1 a)) nf) as [ [ m n1 ] | ] eqn:R;
          try discriminate.
        destruct (Z.eqb (vs m x) (eval e2 m)); try discriminate.
        injection H as Heq Hn; subst.
        apply above_setv. eapply IHrun; [ eassumption | now apply above_setv ].
      * (* object block *)
        destruct (os a x) as [ l | ] eqn:Hx; try discriminate.
        destruct (run k G s (alloc a _ x) nf) as [ [ m n1 ] | ] eqn:R;
          try discriminate.
        destruct (andb _ _) eqn:Hchk; try discriminate.
        injection H as Heq Hn; subst.
        apply above_dealloc.
        eapply IHrun; [ eassumption | now apply above_alloc ].
      * (* new *)
        destruct (os a x) as [ l | ] eqn:Hx; try discriminate.
        injection H as Heq Hn; subst; now apply above_alloc.
      * (* delete *)
        destruct (os a x) as [ l | ] eqn:Hx; try discriminate.
        destruct (andb _ _) eqn:Hchk; try discriminate.
        injection H as Heq Hn; subst; now apply above_dealloc.
      * (* call *)
        destruct (procs G m) as [ [ ps body ] | ] eqn:Hm; try discriminate.
        destruct (Nat.eqb (length ps) (length args)); try discriminate.
        eapply IHrun; eassumption.
      * (* uncall *)
        destruct (procs G m) as [ [ ps body ] | ] eqn:Hm; try discriminate.
        destruct (Nat.eqb (length ps) (length args)); try discriminate.
        eapply IHrun; eassumption.
      * (* object call *)
        destruct (os a x) as [ l | ] eqn:Hx; try discriminate.
        destruct (Nat.ltb l (hn a)); try discriminate.
        destruct (dispatch_fn k (classes G) (hc a l) m) as [ d | ];
          try discriminate.
        destruct (run k G (call_body d x args) a nf) as [ [ c n1 ] | ] eqn:R;
          try discriminate.
        destruct (andb _ _) eqn:Hchk; try discriminate.
        injection H as Heq Hn; subst. eapply IHrun; eassumption.
      * (* object uncall *)
        destruct (os a x) as [ l | ] eqn:Hx; try discriminate.
        destruct (Nat.ltb l (hn a)); try discriminate.
        destruct (dispatch_fn k (classes G) (hc a l) m) as [ d | ];
          try discriminate.
        destruct (run k G (invert (call_body d x args)) a nf) as [ [ c n1 ] | ] eqn:R;
          try discriminate.
        destruct (andb _ _) eqn:Hchk; try discriminate.
        injection H as Heq Hn; subst. eapply IHrun; eassumption.
    + intros e1 s1 s2 e2 a nf b nf' H Ha; simpl in H.
      destruct (Z.eqb (eval e2 a) 0) eqn:E2.
      * destruct (run k G s2 a nf) as [ [ x1 n1 ] | ] eqn:R2; try discriminate.
        destruct (Z.eqb (eval e1 x1) 0) eqn:E1; try discriminate.
        destruct (run k G s1 x1 n1) as [ [ x2 n2 ] | ] eqn:R1; try discriminate.
        eapply IHloop; [ eassumption | ].
        eapply IHrun; [ eassumption | ]. eapply IHrun; eassumption.
      * injection H as Heq Hn; subst; assumption.
Qed.

(** 上限までのゼロ検査を、有限な [seq] の走査として実行する。 *)
Lemma forallb_seq_zero : forall n b l,
  forallb (fun f => Z.eqb (hp b l f) 0) (seq 0 n) = true ->
  forall f, (f < n)%nat -> hp b l f = 0.
Proof.
  intros n b l H f Hf.
  rewrite forallb_forall in H.
  apply Z.eqb_eq, H, in_seq; lia.
Qed.

Lemma run_sound_aux : forall fuel G,
  (forall s a nf b nf',
     run fuel G s a nf = Some (b, nf') -> above nf a -> exec G s a b)
  /\ (forall e1 s1 s2 e2 a nf b nf',
        run_loop fuel G e1 s1 s2 e2 a nf = Some (b, nf') -> above nf a ->
        loopx G e1 s1 s2 e2 a b).
Proof.
  induction fuel as [ | k IH ]; intro G.
  - split; intros; discriminate.
  - destruct (IH G) as [ IHrun IHloop ]. split.
    + intros s a nf b nf' H Ha; destruct s; simpl in H; try discriminate.
      * (* skip *) injection H as Heq Hn; subst b; apply E_skip, steq_refl.
      * (* assign *)
        destruct (in_dec Nat.eq_dec x (fv e)); [ discriminate | ].
        destruct (negb (inb G a e)) eqn:Hin; try discriminate.
        injection H as Heq Hn; subst b.
        apply E_assign; [ assumption | apply steq_refl ].
      * (* field assignment *)
        destruct (negb (inb G a e)) eqn:Hin; try discriminate.
        destruct (os a x) as [ l | ] eqn:Hx; try discriminate.
        destruct (andb (Nat.ltb l (hn a)) _) eqn:Hl; try discriminate.
        destruct (Z.eqb _ _) eqn:He; try discriminate.
        injection H as Heq Hn; subst b.
        apply andb_true_iff in Hl as [ Hl _ ].
        apply Nat.ltb_lt in Hl. apply Z.eqb_eq in He.
        eapply E_fassign; [ eassumption | assumption | apply steq_refl | assumption ].
      * (* array assignment *)
        destruct (negb (inb2 G a ei e)) eqn:Hin; try discriminate.
        destruct (os a x) as [ l | ] eqn:Hx; try discriminate.
        destruct (andb (Nat.ltb l (hn a)) _) eqn:Hl; try discriminate.
        destruct (andb (Z.eqb _ _) _) eqn:He; try discriminate.
        injection H as Heq Hn; subst b.
        apply andb_true_iff in Hl as [ Hl Hbnd ].
        apply Nat.ltb_lt in Hl. apply Nat.ltb_lt in Hbnd.
        apply andb_true_iff in He as [ Hi Hv ].
        apply Z.eqb_eq in Hi. apply Z.eqb_eq in Hv.
        eapply E_aassign;
          [ eassumption | assumption | assumption | apply steq_refl
          | assumption | assumption ].
      * (* swap *) injection H as Heq Hn; subst b. apply E_swap, steq_refl.
      * (* array element swap *)
        destruct (negb (inb2 G a e1 e2)) eqn:Hin; try discriminate.
        destruct (os a x) as [ l1 | ] eqn:Hx; try discriminate.
        destruct (os a y) as [ l2 | ] eqn:Hy; try discriminate.
        destruct (andb (andb (Nat.ltb l1 (hn a)) _) _) eqn:Hl; try discriminate.
        destruct (andb (Z.eqb _ _) (Z.eqb _ _)) eqn:He; try discriminate.
        injection H as Heq Hn; subst b.
        apply andb_true_iff in Hl as [ Hla Hlb ].
        apply andb_true_iff in Hla as [ Hl1 Hb1 ].
        apply andb_true_iff in Hlb as [ Hl2 Hb2 ].
        apply Nat.ltb_lt in Hl1. apply Nat.ltb_lt in Hl2.
        apply Nat.ltb_lt in Hb1. apply Nat.ltb_lt in Hb2.
        apply andb_true_iff in He as [ H1 H2 ].
        apply Z.eqb_eq in H1. apply Z.eqb_eq in H2.
        eapply E_aswap;
          [ eassumption | assumption | assumption | eassumption | assumption
          | assumption | apply steq_refl | assumption | assumption ].
      * (* object swap *)
        injection H as Heq Hn; subst b. apply E_oswap, steq_refl.
      * (* copy *)
        destruct (Nat.eqb x y) eqn:Hxy; try discriminate.
        destruct (os a y) as [ l | ] eqn:Hy; try discriminate.
        injection H as Heq Hn; subst b.
        apply Nat.eqb_neq in Hxy.
        apply E_copy; [ assumption | assumption | apply steq_refl ].
      * (* uncopy *)
        destruct (Nat.eqb x y) eqn:Hxy; try discriminate.
        destruct (oloc_eqb (os a x) (os a y)) eqn:Ho; try discriminate.
        injection H as Heq Hn; subst b.
        apply Nat.eqb_neq in Hxy. apply oloc_eqb_eq in Ho.
        apply E_uncopy; [ assumption | assumption | apply steq_refl ].
      * (* seq *)
        destruct (run k G s1 a nf) as [ [ m n1 ] | ] eqn:R1; [ | discriminate ].
        eapply E_seq.
        -- eapply IHrun; eassumption.
        -- eapply IHrun; [ eassumption | ].
           eapply (proj1 (run_above k G)); eassumption.
      * (* if *)
        destruct (negb (inb G a e1)) eqn:Hin; try discriminate.
        destruct (Z.eqb (eval e1 a) 0) eqn:E1.
        -- apply Z.eqb_eq in E1.
           destruct (run k G s2 a nf) as [ [ m n1 ] | ] eqn:R; [ | discriminate ].
           destruct (Z.eqb (eval e2 m) 0) eqn:E2; [ | discriminate ].
           injection H as Heq Hn; subst b. apply Z.eqb_eq in E2.
           apply E_if_f; [ assumption | eapply IHrun; eassumption | assumption ].
        -- apply Z.eqb_neq in E1.
           destruct (run k G s1 a nf) as [ [ m n1 ] | ] eqn:R; [ | discriminate ].
           destruct (Z.eqb (eval e2 m) 0) eqn:E2; [ discriminate | ].
           injection H as Heq Hn; subst b. apply Z.eqb_neq in E2.
           apply E_if_t; [ assumption | eapply IHrun; eassumption | assumption ].
      * (* loop *)
        destruct (negb (inb G a e1)) eqn:Hin; try discriminate.
        destruct (Z.eqb (eval e1 a) 0) eqn:E1; [ discriminate | ].
        apply Z.eqb_neq in E1.
        destruct (run k G s1 a nf) as [ [ m n1 ] | ] eqn:R; [ | discriminate ].
        eapply E_loop; [ assumption | eapply IHrun; eassumption | ].
        eapply IHloop; [ eassumption | ].
        eapply (proj1 (run_above k G)); eassumption.
      * (* local *)
        destruct (in_dec Nat.eq_dec x (fv e1)); [ discriminate | ].
        destruct (in_dec Nat.eq_dec x (fv e2)); [ discriminate | ].
        destruct (negb (inb G a e1)) eqn:Hin; try discriminate.
        destruct (run k G s (setv a x (eval e1 a)) nf) as [ [ m n1 ] | ] eqn:R;
          [ | discriminate ].
        destruct (Z.eqb (vs m x) (eval e2 m)) eqn:Ex; [ | discriminate ].
        injection H as Heq Hn; subst b. apply Z.eqb_eq in Ex.
        eapply E_local; try eassumption.
        -- eapply IHrun; [ eassumption | now apply above_setv ].
        -- apply steq_refl.
      * (* show *) injection H as Heq Hn; subst b; apply E_show, steq_refl.
      * (* object block *)
        destruct (os a x) as [ l | ] eqn:Hx; try discriminate.
        destruct (run k G s (alloc a _ x) nf) as [ [ m n1 ] | ] eqn:R;
          [ | discriminate ].
        destruct (andb _ _) eqn:Hchk; [ | discriminate ].
        injection H as Heq Hn; subst b.
        apply andb_true_iff in Hchk as [ Hox Hrest ].
        apply andb_true_iff in Hrest as [ Hhn Hrest2 ].
        apply andb_true_iff in Hrest2 as [ Hhc Hzero ].
        apply oloc_eqb_eq in Hox.
        apply Nat.eqb_eq in Hhn. apply Nat.eqb_eq in Hhc.
        (* 上限 [n1] 未満は実際に調べ、それ以上は不変条件から 0 *)
        assert (Hz : forall f, hp m (hn a) f = 0).
        { intro f. destruct (Nat.ltb f n1) eqn:Hf.
          - apply Nat.ltb_lt in Hf. eapply forallb_seq_zero; eassumption.
          - apply Nat.ltb_ge in Hf.
            eapply (proj1 (run_above k G));
              [ eassumption | now apply above_alloc | assumption ]. }
        eapply E_obj;
          [ eassumption | eapply IHrun; [ eassumption | now apply above_alloc ]
          | assumption | assumption | assumption | assumption | apply steq_refl ].
      * (* new *)
        destruct (os a x) as [ l | ] eqn:Hx; [ discriminate | ].
        injection H as Heq Hn; subst b.
        apply E_new; [ assumption | apply steq_refl ].
      * (* delete *)
        destruct (os a x) as [ l | ] eqn:Hx; [ | discriminate ].
        destruct (andb _ _) eqn:Hchk; [ | discriminate ].
        injection H as Heq Hn; subst b.
        apply andb_true_iff in Hchk as [ Hpos Hrest ].
        apply andb_true_iff in Hrest as [ Htop Hrest2 ].
        apply andb_true_iff in Hrest2 as [ Hhc Hzero ].
        apply Nat.ltb_lt in Hpos. apply Nat.eqb_eq in Htop.
        apply Nat.eqb_eq in Hhc. subst l.
        (* 上限 [nf] 未満は実際に調べ、それ以上は不変条件から 0 *)
        assert (Hz : forall f, hp a (pred (hn a)) f = 0).
        { intro f. destruct (Nat.ltb f nf) eqn:Hf.
          - apply Nat.ltb_lt in Hf. eapply forallb_seq_zero; eassumption.
          - apply Nat.ltb_ge in Hf. now apply Ha. }
        apply E_delete;
          [ assumption | assumption | assumption | assumption | apply steq_refl ].
      * (* call *)
        destruct (procs G m) as [ [ ps body ] | ] eqn:Hm; [ | discriminate ].
        destruct (Nat.eqb (length ps) (length args)) eqn:Hlen; [ | discriminate ].
        apply Nat.eqb_eq in Hlen.
        eapply E_call; [ eassumption | eassumption | eapply IHrun; eassumption ].
      * (* uncall *)
        destruct (procs G m) as [ [ ps body ] | ] eqn:Hm; [ | discriminate ].
        destruct (Nat.eqb (length ps) (length args)) eqn:Hlen; [ | discriminate ].
        apply Nat.eqb_eq in Hlen.
        eapply E_uncall; [ eassumption | eassumption | eapply IHrun; eassumption ].
      * (* object call *)
        destruct (os a x) as [ l | ] eqn:Hx; try discriminate.
        destruct (Nat.ltb l (hn a)) eqn:Hl; try discriminate.
        destruct (dispatch_fn k (classes G) (hc a l) m) as [ d | ] eqn:Hd;
          try discriminate.
        destruct (run k G (call_body d x args) a nf) as [ [ c n1 ] | ] eqn:R;
          try discriminate.
        destruct (andb _ _) eqn:Hchk; try discriminate.
        injection H as Heq Hn; subst b.
        apply Nat.ltb_lt in Hl.
        apply andb_true_iff in Hchk as [ Hox Hrest ].
        apply andb_true_iff in Hrest as [ Hcc Hnn ].
        apply oloc_eqb_eq in Hox.
        apply Nat.eqb_eq in Hcc. apply Nat.eqb_eq in Hnn.
        eapply E_ocall;
          [ eassumption | assumption | eapply dispatch_fn_sound; eassumption
          | eapply IHrun; eassumption | assumption | assumption | assumption ].
      * (* object uncall *)
        destruct (os a x) as [ l | ] eqn:Hx; try discriminate.
        destruct (Nat.ltb l (hn a)) eqn:Hl; try discriminate.
        destruct (dispatch_fn k (classes G) (hc a l) m) as [ d | ] eqn:Hd;
          try discriminate.
        destruct (run k G (invert (call_body d x args)) a nf) as [ [ c n1 ] | ] eqn:R;
          try discriminate.
        destruct (andb _ _) eqn:Hchk; try discriminate.
        injection H as Heq Hn; subst b.
        apply Nat.ltb_lt in Hl.
        apply andb_true_iff in Hchk as [ Hox Hrest ].
        apply andb_true_iff in Hrest as [ Hcc Hnn ].
        apply oloc_eqb_eq in Hox.
        apply Nat.eqb_eq in Hcc. apply Nat.eqb_eq in Hnn.
        eapply E_ouncall;
          [ eassumption | assumption | eapply dispatch_fn_sound; eassumption
          | eapply IHrun; eassumption | assumption | assumption | assumption ].
    + intros e1 s1 s2 e2 a nf b nf' H Ha; simpl in H.
      destruct (Z.eqb (eval e2 a) 0) eqn:E2.
      * apply Z.eqb_eq in E2.
        destruct (run k G s2 a nf) as [ [ x1 n1 ] | ] eqn:R2; [ | discriminate ].
        destruct (Z.eqb (eval e1 x1) 0) eqn:E1; [ | discriminate ].
        apply Z.eqb_eq in E1.
        destruct (run k G s1 x1 n1) as [ [ x2 n2 ] | ] eqn:R1; [ | discriminate ].
        assert (Ha1 : above n1 x1)
          by (eapply (proj1 (run_above k G)); eassumption).
        eapply L_step; try eassumption.
        -- eapply IHrun; eassumption.
        -- eapply IHrun; eassumption.
        -- eapply IHloop; [ eassumption | ].
           eapply (proj1 (run_above k G)); eassumption.
      * injection H as Heq Hn; subst b. apply Z.eqb_neq in E2.
        apply L_done; [ assumption | apply steq_refl ].
Qed.

(** **The extracted interpreter is sound**: every state it returns is one
    the reversible semantics allows, so all the theorems above apply to it. *)
Theorem run_sound : forall fuel G s a nf b nf',
  run fuel G s a nf = Some (b, nf') -> above nf a -> exec G s a b.
Proof. intros fuel G; apply (run_sound_aux fuel G). Qed.

(** 初期状態（ヒープが空）はどの上限についても不変条件を満たす。実際に
    プログラムを走らせるときは [nf = 0] から始めればよい。 *)
Lemma above_zero_heap : forall nf a,
  (forall l f, hp a l f = 0) -> above nf a.
Proof. intros nf a H l f _; apply H. Qed.

(** Consequences for the extracted interpreter, for free. *)
Corollary run_injective : forall fuel1 fuel2 G s a1 nf1 a2 nf2 b n1 n2,
  run fuel1 G s a1 nf1 = Some (b, n1) -> above nf1 a1 ->
  run fuel2 G s a2 nf2 = Some (b, n2) -> above nf2 a2 ->
  a1 == a2.
Proof.
  intros fuel1 fuel2 G s a1 nf1 a2 nf2 b n1 n2 H1 Ha1 H2 Ha2.
  eapply exec_inj; eapply run_sound; eassumption.
Qed.

Corollary run_invert : forall fuel G s a nf b nf',
  run fuel G s a nf = Some (b, nf') -> above nf a -> exec G (invert s) b a.
Proof. intros; apply exec_invert; eapply run_sound; eassumption. Qed.

(* ------------------------------------------------------------------ *)
(** * A static type system, and its preservation under inversion       *)
(* ------------------------------------------------------------------ *)

(** ROOPL++ is statically typed and the thesis proves that well-typedness is
    preserved by statement inversion (Haulund 2017, ROOPL).  Here is that
    theorem, mechanized for this core.  Classes are not distinguished (an
    object type carries no class name), which is the only place the type
    system below is coarser than the language's. *)

Inductive ty := Tint | Tobj | Tarr.

Definition tenv := id -> ty.

Definition ty_eq (t1 t2 : ty) : bool :=
  match t1, t2 with
  | Tint, Tint | Tobj, Tobj | Tarr, Tarr => true
  | _, _ => false
  end.

Inductive wt_exp (E : tenv) : exp -> Prop :=
| WTe_cst : forall z, wt_exp E (Cst z)
| WTe_var : forall x, E x = Tint -> wt_exp E (Var x)
| WTe_fld : forall x f, E x = Tobj -> wt_exp E (Fld x f)
| WTe_idx : forall x e, E x = Tarr -> wt_exp E e -> wt_exp E (Idx x e)
| WTe_bop : forall o e1 e2, wt_exp E e1 -> wt_exp E e2 -> wt_exp E (Bop o e1 e2).

(** Method signatures: the types of the formal parameters. *)
Definition sigenv := mid -> list ty.

(** 実引数の型。値渡しは式なので常に整数。 *)
Definition arg_ty (E : tenv) (a : arg) : ty :=
  match a with Aref x => E x | Aval _ => Tint end.

Inductive wt (E : tenv) (S : sigenv) : stm -> Prop :=
| WT_skip : wt E S Sskip
| WT_assign : forall x o e,
    E x = Tint -> ~ In x (fv e) -> wt_exp E e -> wt E S (Sassign x o e)
| WT_fassign : forall x f o e,
    E x = Tobj -> wt_exp E e -> wt E S (Sfassign x f o e)
| WT_aassign : forall x ei o e,
    E x = Tarr -> wt_exp E ei -> wt_exp E e -> wt E S (Saassign x ei o e)
| WT_swap : forall x y,
    E x = Tint -> E y = Tint -> wt E S (Sswap x y)
| WT_aswap : forall x e1 y e2,
    E x = Tarr -> E y = Tarr -> wt_exp E e1 -> wt_exp E e2 ->
    wt E S (Saswap x e1 y e2)
| WT_oswap : forall x y,
    E x = Tobj -> E y = Tobj -> wt E S (Soswap x y)
| WT_copy : forall x y,
    E x = Tobj -> E y = Tobj -> x <> y -> wt E S (Scopy x y)
| WT_uncopy : forall x y,
    E x = Tobj -> E y = Tobj -> x <> y -> wt E S (Suncopy x y)
| WT_seq : forall s1 s2, wt E S s1 -> wt E S s2 -> wt E S (Sseq s1 s2)
| WT_if : forall e1 s1 s2 e2,
    wt_exp E e1 -> wt E S s1 -> wt E S s2 -> wt_exp E e2 ->
    wt E S (Sif e1 s1 s2 e2)
| WT_loop : forall e1 s1 s2 e2,
    wt_exp E e1 -> wt E S s1 -> wt E S s2 -> wt_exp E e2 ->
    wt E S (Sloop e1 s1 s2 e2)
| WT_local : forall x e1 s e2,
    E x = Tint -> ~ In x (fv e1) -> ~ In x (fv e2) ->
    wt_exp E e1 -> wt_exp E e2 -> wt E S s ->
    wt E S (Slocal x e1 s e2)
(* construct allocates a cell block: an object or a (fixed-size) array *)
| WT_obj : forall cl x s,
    E x = Tobj \/ E x = Tarr -> wt E S s -> wt E S (Sobj cl x s)
(* ブロックにしない new / delete。反転で互いに移るので同じ前提を課す *)
| WT_new : forall cl x,
    E x = Tobj \/ E x = Tarr -> wt E S (Snew cl x)
| WT_delete : forall cl x,
    E x = Tobj \/ E x = Tarr -> wt E S (Sdelete cl x)
| WT_show : forall e, wt_exp E e -> wt E S (Sshow e)
| WT_ocall : forall x m args,
    E x = Tobj -> wt E S (Socall x m args)
| WT_ouncall : forall x m args,
    E x = Tobj -> wt E S (Souncall x m args)
| WT_call : forall m args,
    map (arg_ty E) args = S m -> wt E S (Scall m args)
| WT_uncall : forall m args,
    map (arg_ty E) args = S m -> wt E S (Suncall m args).

(** **Well-typedness is preserved by inversion** (Haulund 2017, ROOPL,
    Theorem "type preservation under inversion").  Inversion never changes a
    type: it only swaps an update operator for its inverse, exchanges the two
    guards of a conditional or loop, reverses a sequence, and turns call into
    uncall. *)
Theorem wt_invert : forall E S s, wt E S s -> wt E S (invert s).
Proof.
  intros E S s H; induction H; simpl;
    solve [ constructor; assumption
          | econstructor; eassumption
          | now constructor
          | now apply WT_obj ].
Qed.

Corollary wt_invert_invert : forall E S s, wt E S s -> wt E S (invert (invert s)).
Proof. intros E S s H; rewrite invert_invert; assumption. Qed.

(* ------------------------------------------------------------------ *)
(** * for と switch（インタプリタの追加構文）                            *)
(* ------------------------------------------------------------------ *)

(** このインタプリタ (lib/eval.ml) は ROOPL++ に [for] と [switch] を足して
    いる。どちらも新しい原始構文ではなく、**既にある構文への糖衣**として
    与えられる。ここではその糖衣を定義し、**反転が糖衣と可換であること**
    ——すなわち for と switch もまた可逆であること——を証明する。

    原始構文として足さないのは意味論を弱めるためではない。糖衣として書ける
    ということ自体が「for は局所ブロックと二重ガードのループ、switch は
    二重ガードの条件分岐の入れ子にすぎない」という主張であり、可逆性は
    既に証明した [exec_invert] からそのまま従う。 *)

(** ** for *)

(** [for x in (e1..e2) do s end]：局所変数 [x] を [e1] に束ね、[x] が [e2] に
    なるまで体を走らせる。体は [x] の値ごとに 1 回、両端を含めて走る。

    昇順と降順を別の糖衣にしてあるのは、そうすると**反転がちょうど互いを
    写す**からである（[invert_for_up]）。インタプリタは実行時に [e1] と
    [e2] の大小で向きを選ぶので、表層の [for] はどちらかに対応する。 *)
Definition for_up (x : id) (e1 e2 : exp) (s : stm) : stm :=
  Slocal x e1
    (Sloop (Bop Oeq (Var x) e1) s (Sassign x MAdd (Cst 1)) (Bop Oeq (Var x) e2))
    e2.

Definition for_down (x : id) (e1 e2 : exp) (s : stm) : stm :=
  Slocal x e1
    (Sloop (Bop Oeq (Var x) e1) s (Sassign x MSub (Cst 1)) (Bop Oeq (Var x) e2))
    e2.

(** 反転は昇順と降順をちょうど入れ替え、両端も入れ替える。
    定義どおりに一致するので [reflexivity] で済む。 *)
Lemma invert_for_up : forall x e1 e2 s,
  invert (for_up x e1 e2 s) = for_down x e2 e1 (invert s).
Proof. reflexivity. Qed.

Lemma invert_for_down : forall x e1 e2 s,
  invert (for_down x e1 e2 s) = for_up x e2 e1 (invert s).
Proof. reflexivity. Qed.

(** ゆえに [for] は可逆：昇順に走らせたものは、体を反転した降順の [for] で
    ちょうど元の状態に戻る。 *)
Corollary for_up_reversible : forall G x e1 e2 s a b,
  exec G (for_up x e1 e2 s) a b -> exec G (for_down x e2 e1 (invert s)) b a.
Proof.
  intros G x e1 e2 s a b H.
  rewrite <- invert_for_up; now apply exec_invert.
Qed.

Corollary for_down_reversible : forall G x e1 e2 s a b,
  exec G (for_down x e1 e2 s) a b -> exec G (for_up x e2 e1 (invert s)) b a.
Proof.
  intros G x e1 e2 s a b H.
  rewrite <- invert_for_down; now apply exec_invert.
Qed.

(** 型付けも部品から組み上がる（ループ変数は整数変数）。 *)
Lemma wt_for_up : forall E S x e1 e2 s,
  E x = Tint -> ~ In x (fv e1) -> ~ In x (fv e2) ->
  wt_exp E e1 -> wt_exp E e2 -> wt E S s ->
  wt E S (for_up x e1 e2 s).
Proof.
  intros E S x e1 e2 s Hx H1 H2 We1 We2 Ws.
  apply WT_local; try assumption.
  apply WT_loop; try assumption.
  - apply WTe_bop; [ now apply WTe_var | assumption ].
  - apply WT_assign; [ assumption | simpl; tauto | constructor ].
  - apply WTe_bop; [ now apply WTe_var | assumption ].
Qed.

Lemma wt_for_down : forall E S x e1 e2 s,
  E x = Tint -> ~ In x (fv e1) -> ~ In x (fv e2) ->
  wt_exp E e1 -> wt_exp E e2 -> wt E S s ->
  wt E S (for_down x e1 e2 s).
Proof.
  intros E S x e1 e2 s Hx H1 H2 We1 We2 Ws.
  apply WT_local; try assumption.
  apply WT_loop; try assumption.
  - apply WTe_bop; [ now apply WTe_var | assumption ].
  - apply WT_assign; [ assumption | simpl; tauto | constructor ].
  - apply WTe_bop; [ now apply WTe_var | assumption ].
Qed.

(** ** switch *)

(** [switch x  case v1 s1 esac w1  …  hctiws y]：入口では [x] の値で枝を選び、
    出口では [y] の値で**どの枝を通ったかを思い出す**。これは二重ガードの
    条件分岐を入れ子にしたものにほかならない。

    枝が選べなかったときに走る文 [d]（インタプリタの [switch] 末尾の文列）を
    最内の else に置く。出口の値 [w] が枝ごとに相異なることは、後続の枝を
    通ったときに外側の出口表明が偽になるための条件で、規則 [E_if_f] が
    実行時にそれを確かめている。 *)
Fixpoint rev_switch (x : id) (cs : list (Z * stm * Z)) (d : stm) (y : id) : stm :=
  match cs with
  | [] => d
  | (v, s, w) :: tl =>
      Sif (Bop Oeq (Var x) (Cst v)) s (rev_switch x tl d y)
          (Bop Oeq (Var y) (Cst w))
  end.

(** 枝の反転：入口の値と出口の値を入れ替え、体を反転する。 *)
Definition swap_case (c : Z * stm * Z) : Z * stm * Z :=
  let '(v, s, w) := c in (w, invert s, v).

Lemma swap_case_involutive : forall c, swap_case (swap_case c) = c.
Proof.
  intros [[v s] w]; simpl; now rewrite invert_invert.
Qed.

(** 反転は switch の糖衣と可換：入口と出口の変数も入れ替わる。 *)
Lemma invert_rev_switch : forall x cs d y,
  invert (rev_switch x cs d y) = rev_switch y (map swap_case cs) (invert d) x.
Proof.
  intros x cs d y; induction cs as [ | [[v s] w] tl IH ]; simpl.
  - reflexivity.
  - now rewrite IH.
Qed.

(** ゆえに switch も可逆。 *)
Corollary rev_switch_reversible : forall G x cs d y a b,
  exec G (rev_switch x cs d y) a b ->
  exec G (rev_switch y (map swap_case cs) (invert d) x) b a.
Proof.
  intros G x cs d y a b H.
  rewrite <- invert_rev_switch; now apply exec_invert.
Qed.

(** 二度反転すると元に戻る（[invert_invert] の switch 版）。 *)
Corollary rev_switch_invert_invert : forall x cs d y,
  invert (invert (rev_switch x cs d y)) = rev_switch x cs d y.
Proof. intros; now rewrite invert_invert. Qed.

(* ------------------------------------------------------------------ *)
(** * Sanity checks: the semantics is not vacuous                      *)
(* ------------------------------------------------------------------ *)

(* 例で使う環境。cells は「クラスあたりのセル数」。C0 は 2 フィールド、
   配列のクラス CA は長さ 4 とする。 *)
Definition empty_env : menv :=
  MEnv (fun _ => None) (fun _ => None) (fun _ => 4%nat).
Definition zero : state :=
  St (fun _ => 0) (fun _ => None) 0%nat (fun _ _ => 0) (fun _ => 0%nat).
Definition X : id := 0%nat.
Definition Y : id := 1%nat.
Definition T : id := 2%nat.
Definition O : id := 3%nat.
Definition C0 : cid := 0%nat.   (* 例で使うクラス名 *)
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
  Sobj C0 O (Sseq (Sfassign O F0 MAdd (Cst 3))
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
    + reflexivity.
    + apply steq_refl.
  - split; [ reflexivity | split; reflexivity ].
Qed.

(** An array is an object with a dynamic index.  Inside a block:
      ar[0] += 5 ; ar[0] <=> ar[1] ; X += ar[1] ; ar[1] -= 5
    leaves X = 5 and the cells zero-cleared, so `destruct` may pop them. *)
Definition arrprog : stm :=
  Sobj C0 O (Sseq (Saassign O (Cst 0) MAdd (Cst 5))
          (Sseq (Saswap O (Cst 0) O (Cst 1))
           (Sseq (Sassign X MAdd (Idx O (Cst 1)))
                 (Saassign O (Cst 1) MSub (Cst 5))))).

Example ex_array :
  exists b, exec empty_env arrprog zero b /\ vs b X = 5 /\ hn b = 0%nat.
Proof.
  eexists. split.
  - eapply E_obj.
    + reflexivity.
    + eapply E_seq.
      * eapply E_aassign with (l := 0%nat);
          [ reflexivity | simpl; lia | simpl; lia | apply steq_refl
          | reflexivity | reflexivity ].
      * eapply E_seq.
        -- eapply E_aswap with (l1 := 0%nat) (l2 := 0%nat);
             [ reflexivity | simpl; lia | simpl; lia
             | reflexivity | simpl; lia | simpl; lia
             | apply steq_refl | reflexivity | reflexivity ].
        -- eapply E_seq.
           ++ apply E_assign; [ simpl; tauto | apply steq_refl ].
           ++ eapply E_aassign with (l := 0%nat);
                [ reflexivity | simpl; lia | simpl; lia | apply steq_refl
                | reflexivity | reflexivity ].
    + reflexivity.
    + reflexivity.
    + intro f; simpl; destruct f as [ | [ | f ] ]; reflexivity.
    + reflexivity.
    + apply steq_refl.
  - split; reflexivity.
Qed.

(** The array program is well typed, and so is its inverse (wt_invert). *)
Definition tenv0 : tenv := fun x => if Nat.eqb x O then Tarr else Tint.
Definition sig0 : sigenv := fun _ => [].

Example ex_wt_array : wt tenv0 sig0 arrprog.
Proof.
  unfold arrprog.
  apply WT_obj; [ right; unfold tenv0, O; reflexivity | ].
  apply WT_seq.
  - apply WT_aassign; [ reflexivity | constructor | constructor ].
  - apply WT_seq.
    + apply WT_aswap; [ reflexivity | reflexivity | constructor | constructor ].
    + apply WT_seq.
      * apply WT_assign; [ reflexivity | simpl; tauto | ].
        apply WTe_idx; [ reflexivity | constructor ].
      * apply WT_aassign; [ reflexivity | constructor | constructor ].
Qed.

Example ex_wt_array_inverse : wt tenv0 sig0 (invert arrprog).
Proof. apply wt_invert, ex_wt_array. Qed.

(** copy C x y then uncopy C x y restores the state. *)
Example ex_copy_uncopy :
  exists b c, exec empty_env (Sobj C0 O (Sseq (Scopy O Y) (Suncopy O Y))) zero b
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
    + reflexivity.
    + apply steq_refl.
  - split; [ | apply steq_refl ].
    steq_split; simpl.
    + intro y0; reflexivity.
    + intro y0; destruct y0 as [ | [ | [ | [ | y0 ] ] ] ]; reflexivity.
    + reflexivity.
    + intros l f Hl; lia.
    + intros l Hl; lia.
Qed.

(** A method with a parameter, called by reference:
      method inc(int n)  n += 1
    `call inc(X)` increments the caller's X, and `uncall inc(X)` undoes it. *)
Definition M0 : mid := 0%nat.
Definition P0 : id := 10%nat.
Definition genv : menv :=
  MEnv (fun m => if Nat.eqb m M0 then Some (MDecl [P0] (Sassign P0 MAdd (Cst 1)))
                 else None)
       (fun _ => None) (fun _ => 4%nat).

Example ex_call_uncall :
  exists b c, exec genv (Scall M0 [Aref X]) zero b
              /\ exec genv (Suncall M0 [Aref X]) b c
              /\ vs b X = 1 /\ c == zero.
Proof.
  eexists. eexists. split; [ | split; [ | split ] ].
  - eapply E_call; [ reflexivity | reflexivity | ].
    simpl. apply E_assign; [ simpl; tauto | apply steq_refl ].
  - eapply E_uncall; [ reflexivity | reflexivity | ].
    simpl. apply E_assign; [ simpl; tauto | apply steq_refl ].
  - reflexivity.
  - steq_split; simpl.
    + intro y; unfold X; destruct y; reflexivity.
    + intro y; reflexivity.
    + reflexivity.
    + intros l f Hl; lia.
    + hc_auto.
Qed.

(** **Subtype polymorphism.**  Class B inherits A and overrides `bump`;
    class C inherits A and does not.  The very same statement
    `call o::bump()` runs B's method when o was constructed as a B and A's
    method when it was constructed as a C, because dispatch looks at the
    *run-time* class recorded in the heap. *)
Definition CA : cid := 1%nat.
Definition CB : cid := 2%nat.
Definition CC : cid := 3%nat.
Definition MB : mid := 1%nat.      (* the method name "bump" *)
Definition TH : id := 20%nat.      (* its receiver parameter, i.e. `this` *)

Definition bumpA : mdecl := MDecl [TH] (Sfassign TH F0 MAdd (Cst 1)).
Definition bumpB : mdecl := MDecl [TH] (Sfassign TH F0 MAdd (Cst 2)).

Definition ctab : ctable := fun c =>
  if Nat.eqb c CA then Some (CDecl None (fun m => if Nat.eqb m MB then Some bumpA else None))
  else if Nat.eqb c CB then Some (CDecl (Some CA) (fun m => if Nat.eqb m MB then Some bumpB else None))
  else if Nat.eqb c CC then Some (CDecl (Some CA) (fun _ => None))
  else None.

Definition oenv : menv := MEnv (fun _ => None) ctab (fun _ => 4%nat).

(** construct <c> o   call o::bump()   X += o.f   o.f -= <k>   destruct o *)
Definition dispatch_prog (c : cid) (k : Z) : stm :=
  Sobj c O (Sseq (Socall O MB [])
            (Sseq (Sassign X MAdd (Fld O F0))
                  (Sfassign O F0 MSub (Cst k)))).

Example ex_dispatch_override :
  exists b, exec oenv (dispatch_prog CB 2) zero b /\ vs b X = 2 /\ hn b = 0%nat.
Proof.
  eexists. split.
  - eapply E_obj.
    + reflexivity.
    + eapply E_seq.
      * eapply E_ocall with (l := 0%nat) (d := bumpB).
        -- reflexivity.
        -- simpl; lia.
        -- eapply D_here; reflexivity.
        -- simpl. eapply E_fassign with (l := 0%nat);
             [ reflexivity | simpl; lia | apply steq_refl | reflexivity ].
        -- reflexivity.
        -- reflexivity.
        -- reflexivity.
      * eapply E_seq.
        -- apply E_assign; [ simpl; tauto | apply steq_refl ].
        -- eapply E_fassign with (l := 0%nat);
             [ reflexivity | simpl; lia | apply steq_refl | reflexivity ].
    + reflexivity.
    + reflexivity.
    + intro f; simpl; destruct f; reflexivity.
    + reflexivity.
    + apply steq_refl.
  - split; reflexivity.
Qed.

Example ex_dispatch_inherited :
  exists b, exec oenv (dispatch_prog CC 1) zero b /\ vs b X = 1 /\ hn b = 0%nat.
Proof.
  eexists. split.
  - eapply E_obj.
    + reflexivity.
    + eapply E_seq.
      * eapply E_ocall with (l := 0%nat) (d := bumpA).
        -- reflexivity.
        -- simpl; lia.
        -- eapply D_up; [ reflexivity | reflexivity | eapply D_here; reflexivity ].
        -- simpl. eapply E_fassign with (l := 0%nat);
             [ reflexivity | simpl; lia | apply steq_refl | reflexivity ].
        -- reflexivity.
        -- reflexivity.
        -- reflexivity.
      * eapply E_seq.
        -- apply E_assign; [ simpl; tauto | apply steq_refl ].
        -- eapply E_fassign with (l := 0%nat);
             [ reflexivity | simpl; lia | apply steq_refl | reflexivity ].
    + reflexivity.
    + reflexivity.
    + intro f; simpl; destruct f; reflexivity.
    + reflexivity.
    + apply steq_refl.
  - split; reflexivity.
Qed.

(** 値渡しの引数。
      method addto(int n, int k)  n += k
    を `call addto(X, 3)` と呼ぶと、k は式 3 に束ねられる。値渡しの仮引数は
    局所ブロックで包まれるので、**本体が k を書き換えたままだと出口の表明で
    落ちる**（これが値引数の可逆性の条件）。 *)
Definition M1 : mid := 1%nat.
Definition P1 : id := 11%nat.

Definition venv : menv :=
  MEnv (fun m => if Nat.eqb m M1
                 then Some (MDecl [P0; P1] (Sassign P0 MAdd (Var P1)))
                 else None)
       (fun _ => None) (fun _ => 4%nat).

Example ex_call_value_arg :
  exists b, exec venv (Scall M1 [Aref X; Aval (Cst 3)]) zero b
         /\ vs b X = 3 /\ vs b P1 = 0.
Proof.
  eexists. split.
  - eapply E_call; [ reflexivity | reflexivity | ].
    simpl. eapply E_local; [ simpl; tauto | simpl; tauto | | | apply steq_refl ].
    + apply E_assign; [ unfold X, P1; simpl; intuition discriminate
                      | apply steq_refl ].
    + reflexivity.
  - split; reflexivity.
Qed.

(** 値渡しの仮引数を書き換える本体は、出口の表明で落ちて導出が存在しない。
    値渡しがちょうど局所ブロックであることを、まず計算で確かめる。 *)
Definition badenv : menv :=
  MEnv (fun m => if Nat.eqb m M1
                 then Some (MDecl [P0; P1] (Sassign P1 MAdd (Cst 1)))
                 else None)
       (fun _ => None) (fun _ => 4%nat).

Example ex_bind_args_is_local :
  bind_args [P0; P1] [Aref X; Aval (Cst 3)] (Sassign P1 MAdd (Cst 1))
  = Slocal P1 (Cst 3) (Sassign P1 MAdd (Cst 1)) (Cst 3).
Proof. reflexivity. Qed.

Example ex_value_arg_must_not_change :
  forall a b,
    ~ exec badenv (Slocal P1 (Cst 3) (Sassign P1 MAdd (Cst 1)) (Cst 3)) a b.
Proof.
  intros a b H; inversion H; subst.
  match goal with
  | [ HA : exec badenv (Sassign _ _ _) _ _ |- _ ] => inversion HA; subst
  end.
  (* いま「出口表明 vs b P1 = 3」と「b は P1 を 4 にした状態」が同居する。
     前者を後者で書き換えると 3 = 4 になって矛盾する。
     （Ltac のパターンでは == 記法ではなく steq を書く必要がある） *)
  match goal with
  | [ HB : steq ?m (setv ?st P1 (mapp ?o ?u ?w)) |- _ ] =>
      assert (Hval : vs m P1 = mapp o u w)
        by (rewrite (steq_vs m (setv st P1 (mapp o u w)) P1 HB);
            simpl; try rewrite Nat.eqb_refl; reflexivity)
  end.
  (* 出口表明は vs b P1 = 3、いま示した Hval は vs b P1 = 4。lia が潰す *)
  simpl in *; lia.
Qed.

(** 抽出したインタプリタも値渡しを扱う。 *)
Example ex_run_value_arg :
  exists b nf, run 100 venv (Scall M1 [Aref X; Aval (Cst 3)]) zero 0%nat
               = Some (b, nf) /\ vs b X = 3.
Proof. eexists; eexists; split; [ reflexivity | reflexivity ]. Qed.

Example ex_run_value_arg_changed :
  run 100 badenv (Scall M1 [Aref X; Aval (Cst 3)]) zero 0%nat = None.
Proof. reflexivity. Qed.

(** ブロックにしない new / delete。ブロック形と同じ計算をする。 *)
Definition newprog : stm :=
  Sseq (Snew C0 O)
       (Sseq (Sfassign O F0 MAdd (Cst 3))
             (Sseq (Sassign X MAdd (Fld O F0))
                   (Sseq (Sfassign O F0 MSub (Cst 3))
                         (Sdelete C0 O)))).

Example ex_new_delete :
  exists b, exec empty_env newprog zero b /\ vs b X = 3 /\ hn b = 0%nat
         /\ os b O = None.
Proof.
  eexists. split.
  - eapply E_seq; [ apply E_new; [ reflexivity | apply steq_refl ] | ].
    eapply E_seq;
      [ eapply E_fassign; [ reflexivity | simpl; lia | apply steq_refl | reflexivity ] | ].
    eapply E_seq;
      [ apply E_assign; [ unfold X, O; simpl; intuition discriminate
                        | apply steq_refl ] | ].
    eapply E_seq;
      [ eapply E_fassign; [ reflexivity | simpl; lia | apply steq_refl | reflexivity ] | ].
    apply E_delete.
    + reflexivity.
    + simpl; lia.
    + intro f; simpl; destruct f; reflexivity.
    + reflexivity.
    + apply steq_refl.
  - split; [ reflexivity | ]. split; reflexivity.
Qed.

(** 反転は new と delete をちょうど入れ替える。 *)
Example ex_invert_new : invert (Snew C0 O) = Sdelete C0 O.
Proof. reflexivity. Qed.

Example ex_invert_delete : invert (Sdelete C0 O) = Snew C0 O.
Proof. reflexivity. Qed.

(** 実行してから逆を実行すると元に戻る（[exec_invert] の new/delete 版）。 *)
Example ex_new_delete_back :
  exists b, exec empty_env newprog zero b /\ exec empty_env (invert newprog) b zero.
Proof.
  destruct ex_new_delete as [ b [ Hb _ ] ]. exists b.
  split; [ assumption | now apply exec_invert ].
Qed.

(** 抽出したインタプリタも走らせる。 *)
Example ex_run_new_delete :
  exists b nf, run 100 empty_env newprog zero 0%nat = Some (b, nf)
            /\ vs b X = 3 /\ hn b = 0%nat.
Proof.
  eexists; eexists; split; [ reflexivity | ]. split; reflexivity.
Qed.

(** ゼロクリアを忘れた delete は落ちる。 *)
Example ex_run_delete_garbage :
  run 100 empty_env
      (Sseq (Snew C0 O) (Sseq (Sfassign O F0 MAdd (Cst 3)) (Sdelete C0 O)))
      zero 0%nat = None.
Proof. reflexivity. Qed.

(** 抽出したインタプリタが**オブジェクトブロックを実際に走らせる**こと。
    出口の「全フィールドがゼロ」は、持ち回った上限までを調べて判定している。 *)
Example ex_run_object :
  exists b nf, run 100 empty_env objprog zero 0%nat = Some (b, nf)
            /\ vs b X = 3 /\ hn b = 0%nat /\ os b O = None.
Proof.
  eexists; eexists; split; [ reflexivity | ].
  split; [ reflexivity | ]. split; reflexivity.
Qed.

(** ゼロクリアを忘れたオブジェクトブロックは（意味論と同じく）落ちる。 *)
Example ex_run_object_garbage :
  run 100 empty_env (Sobj C0 O (Sfassign O F0 MAdd (Cst 3))) zero 0%nat = None.
Proof. reflexivity. Qed.

(** 動的束縛つきのメソッド呼出しも走る（[ex_dispatch_override] の計算版）。 *)
Example ex_run_dispatch :
  exists b nf, run 200 oenv (dispatch_prog CB 2) zero 0%nat = Some (b, nf)
            /\ vs b X = 2 /\ hn b = 0%nat.
Proof.
  eexists; eexists; split; [ reflexivity | ]. split; reflexivity.
Qed.

Example ex_run_dispatch_inherited :
  exists b nf, run 200 oenv (dispatch_prog CC 1) zero 0%nat = Some (b, nf)
            /\ vs b X = 1.
Proof. eexists; eexists; split; [ reflexivity | reflexivity ]. Qed.

(** The side condition on assignment bites: `X += X` has no derivation. *)
Example ex_self_assign_stuck :
  forall a b, ~ exec empty_env (Sassign X MAdd (Var X)) a b.
Proof.
  intros a b H; inversion H; subst. simpl in *. intuition.
Qed.

(** for x in (1..3) do X += x end  は X を 6 にし、x は残らない。 *)
Definition forprog : stm := for_up T (Cst 1) (Cst 3) (Sassign X MAdd (Var T)).

Example ex_for :
  exists b, exec empty_env forprog zero b /\ vs b X = 6 /\ vs b T = 0.
Proof.
  eexists. split.
  - eapply E_local; [ simpl; tauto | simpl; tauto | | | apply steq_refl ].
    + eapply E_loop.
      * simpl; discriminate.
      * apply E_assign; [ unfold X, T; simpl; intuition discriminate
                        | apply steq_refl ].
      * eapply L_step.
        -- reflexivity.
        -- apply E_assign; [ simpl; tauto | apply steq_refl ].
        -- reflexivity.
        -- apply E_assign; [ unfold X, T; simpl; intuition discriminate
                           | apply steq_refl ].
        -- eapply L_step.
           ++ reflexivity.
           ++ apply E_assign; [ simpl; tauto | apply steq_refl ].
           ++ reflexivity.
           ++ apply E_assign; [ unfold X, T; simpl; intuition discriminate
                              | apply steq_refl ].
           ++ apply L_done; [ simpl; discriminate | apply steq_refl ].
    + reflexivity.
  - split; reflexivity.
Qed.

(** その逆：体を反転した降順の for が元の状態へ戻す。 *)
Example ex_for_back :
  exists b, exec empty_env
              (for_down T (Cst 3) (Cst 1) (invert (Sassign X MAdd (Var T))))
              b zero.
Proof.
  destruct ex_for as [ b [ Hb _ ] ]. exists b.
  now apply for_up_reversible.
Qed.

(** switch X  case 1 → Y += 10 esac 10 | case 2 → Y += 20 esac 20  hctiws Y。
    X = 2 なので二番目の枝を通る。外側の枝の出口表明 (Y = 10) が偽である
    ことが、「一番目の枝は通らなかった」という情報を残す。 *)
Definition swcases : list (Z * stm * Z) :=
  (1, Sassign Y MAdd (Cst 10), 10) :: (2, Sassign Y MAdd (Cst 20), 20) :: nil.

Definition swbody : stm := rev_switch X swcases Sskip Y.

Definition swprog : stm := Sseq (Sassign X MAdd (Cst 2)) swbody.

Example ex_switch :
  exists b, exec empty_env swprog zero b /\ vs b X = 2 /\ vs b Y = 20.
Proof.
  eexists. split.
  - eapply E_seq.
    + apply E_assign; [ simpl; tauto | apply steq_refl ].
    + apply E_if_f.
      * reflexivity.
      * apply E_if_t.
        -- simpl; discriminate.
        -- apply E_assign; [ simpl; tauto | apply steq_refl ].
        -- simpl; discriminate.
      * reflexivity.
  - split; reflexivity.
Qed.

(** その逆：入口と出口の変数を入れ替え、枝ごとに入口の値と出口の値を
    入れ替えた switch が、どんな実行も元へ戻す。 *)
Example ex_switch_back : forall a b,
  exec empty_env swbody a b ->
  exec empty_env (rev_switch Y (map swap_case swcases) Sskip X) b a.
Proof.
  intros a b H.
  (* [invert Sskip] は [Sskip] に簡約されるが、evar 越しには合わないので明示する *)
  apply (rev_switch_reversible empty_env X swcases Sskip Y a b H).
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
Print Assumptions wt_invert.
Print Assumptions run_sound.
Print Assumptions run_injective.
Print Assumptions run_above.
Print Assumptions ex_run_object.
Print Assumptions ex_run_dispatch.
Print Assumptions ex_new_delete.
Print Assumptions ex_new_delete_back.
Print Assumptions ex_call_value_arg.
Print Assumptions ex_value_arg_must_not_change.
Print Assumptions for_up_reversible.
Print Assumptions rev_switch_reversible.
Print Assumptions ex_for.
Print Assumptions ex_switch.
