(**
  roopl.v -- Rocq/Coq formalization of the reversible statement core of
  ROOPL++ (Cservenka 2018), the language implemented by this interpreter,
  together with machine-checked proofs of

     invert_invert   invert (invert s) = s              (inversion is an involution)
     exec_invert     exec s a b -> exec (invert s) b a  (the inverted program runs backwards)
     exec_iff        exec s a b <-> exec (invert s) b a
     exec_det        forward determinism
     exec_inj        exec s a1 b -> exec s a2 b -> a1 == a2   (REVERSIBILITY)

  exec_inj is the reversibility statement: the final store determines the
  initial store, i.e. every program denotes an injective partial function on
  stores.  ROOPL and ROOPL++ come with pen-and-paper semantics and type
  systems (Haulund 2017, Cservenka 2018) but, unlike Janus (vJanus in the
  sister project PyJanus), had no mechanized reversibility proof.

  Scope.  The statement core: skip, reversible assignment (+=, -=, ^=), swap,
  sequencing, the two-guard conditional and loop, local blocks
  (local/delocal), object blocks (construct/destruct) and parameterless
  method call/uncall.  Objects are modelled as a single cell, so fields,
  arrays, inheritance and dynamic dispatch are NOT covered -- see README.md.

  Axioms.  None.  Stores are functions, so instead of assuming functional
  extensionality the rules that build a store relate it *pointwise* (==) to
  the intended update; `Print Assumptions` at the end reports Closed.
*)

From Stdlib Require Import ZArith List Bool Arith Lia.
Import ListNotations.
Open Scope Z_scope.

(* ------------------------------------------------------------------ *)
(** * Stores                                                           *)
(* ------------------------------------------------------------------ *)

Definition id := nat.
Definition mid := nat.
Definition store := id -> Z.

Definition update (s : store) (x : id) (v : Z) : store :=
  fun y => if Nat.eqb x y then v else s y.

(** Pointwise equality of stores. Used instead of Leibniz equality so the
    development needs no functional extensionality. *)
Definition seq (s1 s2 : store) : Prop := forall y, s1 y = s2 y.
Infix "==" := seq (at level 70, no associativity).

Lemma seq_refl : forall s, s == s.
Proof. intros s y; reflexivity. Qed.

Lemma seq_sym : forall s1 s2, s1 == s2 -> s2 == s1.
Proof. intros s1 s2 H y; symmetry; apply H. Qed.

Lemma seq_trans : forall s1 s2 s3, s1 == s2 -> s2 == s3 -> s1 == s3.
Proof. intros s1 s2 s3 H1 H2 y; rewrite H1; apply H2. Qed.

Lemma update_same : forall s x, update s x (s x) == s.
Proof.
  intros s x y; unfold update.
  destruct (Nat.eqb x y) eqn:E; [ apply Nat.eqb_eq in E; subst | ]; reflexivity.
Qed.

Lemma update_shadow : forall s x u v, update (update s x u) x v == update s x v.
Proof.
  intros s x u v y; unfold update; destruct (Nat.eqb x y); reflexivity.
Qed.

Lemma update_hit : forall s x v, update s x v x = v.
Proof. intros; unfold update; rewrite Nat.eqb_refl; reflexivity. Qed.

Lemma update_miss : forall s x v y, x <> y -> update s x v y = s y.
Proof.
  intros s x v y H; unfold update.
  destruct (Nat.eqb x y) eqn:E; [ apply Nat.eqb_eq in E; contradiction | reflexivity ].
Qed.

Lemma update_seq : forall s1 s2 x v, s1 == s2 -> update s1 x v == update s2 x v.
Proof. intros s1 s2 x v H y; unfold update; destruct (Nat.eqb x y); auto. Qed.

(* ------------------------------------------------------------------ *)
(** * Expressions                                                      *)
(* ------------------------------------------------------------------ *)

Inductive binop := Oadd | Osub | Omul | Oeq | Olt.

Inductive exp :=
| Cst (z : Z)
| Var (x : id)
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

Fixpoint eval (e : exp) (s : store) : Z :=
  match e with
  | Cst z => z
  | Var x => s x
  | Bop o e1 e2 => eval_binop o (eval e1 s) (eval e2 s)
  end.

Fixpoint fv (e : exp) : list id :=
  match e with
  | Cst _ => []
  | Var x => [x]
  | Bop _ e1 e2 => fv e1 ++ fv e2
  end.

Lemma eval_seq : forall e s1 s2, s1 == s2 -> eval e s1 = eval e s2.
Proof.
  induction e; intros s1 s2 H; simpl; auto.
  rewrite (IHe1 s1 s2 H), (IHe2 s1 s2 H); reflexivity.
Qed.

(** The side condition that makes an assignment reversible: the updated
    variable does not occur in the right-hand side. *)
Lemma eval_update_notin :
  forall e s w v, ~ In w (fv e) -> eval e (update s w v) = eval e s.
Proof.
  induction e; intros s w v H; simpl in *; auto.
  - unfold update; destruct (Nat.eqb w x) eqn:E.
    + apply Nat.eqb_eq in E; subst; exfalso; apply H; now left.
    + reflexivity.
  - rewrite IHe1, IHe2; auto.
    + intro; apply H; apply in_or_app; now right.
    + intro; apply H; apply in_or_app; now left.
Qed.

(** The form actually used below: a store that agrees with s except at x. *)
Lemma eval_off :
  forall e s c w v, ~ In w (fv e) -> c == update s w v -> eval e c = eval e s.
Proof.
  intros e s c w v Hn Hc.
  rewrite (eval_seq e c (update s w v) Hc).
  now apply eval_update_notin.
Qed.

(* ------------------------------------------------------------------ *)
(** * Reversible update operators                                      *)
(* ------------------------------------------------------------------ *)

Inductive modop := MAdd | MSub | MXor.

Definition mapp (o : modop) (a b : Z) : Z :=
  match o with
  | MAdd => a + b
  | MSub => a - b
  | MXor => Z.lxor a b
  end.

Definition minv (o : modop) : modop :=
  match o with MAdd => MSub | MSub => MAdd | MXor => MXor end.

Lemma minv_involutive : forall o, minv (minv o) = o.
Proof. destruct o; reflexivity. Qed.

(** The local law that makes `x op= e` reversible. *)
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
| Sassign (x : id) (o : modop) (e : exp)            (**r x op= e *)
| Sswap (x y : id)                                   (**r x <=> y *)
| Sseq (s1 s2 : stm)
| Sif (e1 : exp) (s1 s2 : stm) (e2 : exp)            (**r if e1 then s1 else s2 fi e2 *)
| Sloop (e1 : exp) (s1 s2 : stm) (e2 : exp)          (**r from e1 do s1 loop s2 until e2 *)
| Slocal (x : id) (e1 : exp) (s : stm) (e2 : exp)    (**r local x = e1  s  delocal x = e2 *)
| Sobj (x : id) (s : stm)                            (**r construct C x  s  destruct x *)
| Scall (m : mid)
| Suncall (m : mid).

Fixpoint invert (s : stm) : stm :=
  match s with
  | Sskip => Sskip
  | Sassign x o e => Sassign x (minv o) e
  | Sswap x y => Sswap x y
  | Sseq s1 s2 => Sseq (invert s2) (invert s1)
  | Sif e1 s1 s2 e2 => Sif e2 (invert s1) (invert s2) e1
  | Sloop e1 s1 s2 e2 => Sloop e2 (invert s1) (invert s2) e1
  | Slocal x e1 s e2 => Slocal x e2 (invert s) e1
  | Sobj x s => Sobj x (invert s)
  | Scall m => Suncall m
  | Suncall m => Scall m
  end.

(** Program inversion is an involution (ROOPL++ thesis, Prop. 3.1). *)
Theorem invert_invert : forall s, invert (invert s) = s.
Proof.
  induction s; simpl; try reflexivity;
    try (rewrite IHs1, IHs2; reflexivity);
    try (rewrite IHs; reflexivity).
  - rewrite minv_involutive; reflexivity.
Qed.

(* ------------------------------------------------------------------ *)
(** * Big-step operational semantics                                   *)
(* ------------------------------------------------------------------ *)

(** Method environment: a parameterless method body per name. *)
Definition menv := mid -> option stm.

(** Every rule that builds a store relates it *pointwise* (==) to the
    intended value, which is what keeps the development free of functional
    extensionality. *)
Inductive exec (G : menv) : stm -> store -> store -> Prop :=
| E_skip : forall a b,
    a == b -> exec G Sskip a b
| E_assign : forall x o e a b,
    ~ In x (fv e) ->
    b == update a x (mapp o (a x) (eval e a)) ->
    exec G (Sassign x o e) a b
| E_swap : forall x y a b,
    b == update (update a x (a y)) y (a x) ->
    exec G (Sswap x y) a b
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
    exec G s (update a x (eval e1 a)) b ->
    b x = eval e2 b ->
    c == update b x (a x) ->
    exec G (Slocal x e1 s e2) a c
| E_obj : forall x s a b c,
    exec G s (update a x 0) b ->
    b x = 0 ->
    c == update b x (a x) ->
    exec G (Sobj x s) a c
| E_call : forall m s a b,
    G m = Some s -> exec G s a b -> exec G (Scall m) a b
| E_uncall : forall m s a b,
    G m = Some s -> exec G (invert s) a b -> exec G (Suncall m) a b

(** [loopx G e1 s1 s2 e2 a b]: the tail of [from e1 do s1 loop s2 until e2],
    entered after s1 has run. *)
with loopx (G : menv) : exp -> stm -> stm -> exp -> store -> store -> Prop :=
| L_done : forall e1 s1 s2 e2 a b,
    eval e2 a <> 0 -> a == b -> loopx G e1 s1 s2 e2 a b
| L_step : forall e1 s1 s2 e2 a b c d,
    eval e2 a = 0 ->
    exec G s2 a b ->
    eval e1 b = 0 ->
    exec G s1 b c ->
    loopx G e1 s1 s2 e2 c d ->
    loopx G e1 s1 s2 e2 a d.

Scheme exec_min := Minimality for exec Sort Prop
  with loopx_min := Minimality for loopx Sort Prop.
Combined Scheme exec_loopx_min from exec_min, loopx_min.

(** Executions respect pointwise equality of stores on both ends.  This is
    the price of not assuming functional extensionality, and it is what lets
    the determinism and inversion proofs compose derivations. *)
Theorem exec_loopx_eq : forall G,
  (forall s a b, exec G s a b ->
     forall a' b', a == a' -> b == b' -> exec G s a' b')
  /\ (forall e1 s1 s2 e2 a b, loopx G e1 s1 s2 e2 a b ->
     forall a' b', a == a' -> b == b' -> loopx G e1 s1 s2 e2 a' b').
Proof.
  intro G; apply exec_loopx_min.
  - (* skip *)
    intros a b Hab a' b' Ha Hb.
    apply E_skip. eauto using seq_trans, seq_sym.
  - (* assign *)
    intros x o e a b Hn Hb a' b' Ha Hb'.
    apply E_assign; auto.
    intro y. rewrite <- Hb'. rewrite Hb. unfold update.
    rewrite (Ha x), (eval_seq e a a' Ha).
    destruct (Nat.eqb x y); [ reflexivity | apply Ha ].
  - (* swap *)
    intros x y a b Hb a' b' Ha Hb'.
    apply E_swap. intro z. rewrite <- Hb', Hb. unfold update.
    rewrite (Ha x), (Ha y), (Ha z). reflexivity.
  - (* seq *)
    intros s1 s2 a b c H1 IH1 H2 IH2 a' c' Ha Hc.
    eapply E_seq.
    + apply IH1; [ exact Ha | apply seq_refl ].
    + apply IH2; [ apply seq_refl | exact Hc ].
  - (* if true *)
    intros e1 s1 s2 e2 a b H1 H2 IH H3 a' b' Ha Hb.
    apply E_if_t.
    + rewrite <- (eval_seq e1 a a' Ha); assumption.
    + apply IH; assumption.
    + rewrite <- (eval_seq e2 b b' Hb); assumption.
  - (* if false *)
    intros e1 s1 s2 e2 a b H1 H2 IH H3 a' b' Ha Hb.
    apply E_if_f.
    + rewrite <- (eval_seq e1 a a' Ha); assumption.
    + apply IH; assumption.
    + rewrite <- (eval_seq e2 b b' Hb); assumption.
  - (* loop *)
    intros e1 s1 s2 e2 a b c H1 H2 IH1 H3 IH2 a' c' Ha Hc.
    eapply E_loop.
    + rewrite <- (eval_seq e1 a a' Ha); assumption.
    + apply IH1; [ exact Ha | apply seq_refl ].
    + apply IH2; [ apply seq_refl | exact Hc ].
  - (* local *)
    intros x e1 s e2 a b c Hn1 Hn2 Hs IH Hx Hc a' c' Ha Hc'.
    eapply E_local; eauto.
    + apply IH; [ | apply seq_refl ].
      intro y. unfold update. rewrite (eval_seq e1 a a' Ha).
      destruct (Nat.eqb x y); [ reflexivity | apply Ha ].
    + intro y. rewrite <- Hc', Hc. unfold update.
      rewrite (Ha x). reflexivity.
  - (* object block *)
    intros x s a b c Hs IH Hx Hc a' c' Ha Hc'.
    eapply E_obj; eauto.
    + apply IH; [ | apply seq_refl ].
      intro y. unfold update.
      destruct (Nat.eqb x y); [ reflexivity | apply Ha ].
    + intro y. rewrite <- Hc', Hc. unfold update.
      rewrite (Ha x). reflexivity.
  - (* call *)
    intros m s a b Hm Hs IH a' b' Ha Hb.
    eapply E_call; eauto.
  - (* uncall *)
    intros m s a b Hm Hs IH a' b' Ha Hb.
    eapply E_uncall; eauto.
  - (* loop done *)
    intros e1 s1 s2 e2 a b H1 Hab a' b' Ha Hb.
    apply L_done.
    + rewrite <- (eval_seq e2 a a' Ha); assumption.
    + eauto using seq_trans, seq_sym.
  - (* loop step *)
    intros e1 s1 s2 e2 a b c d H1 H2 IH1 H3 H4 IH2 H5 IH3 a' d' Ha Hd.
    eapply L_step.
    + rewrite <- (eval_seq e2 a a' Ha); assumption.
    + apply IH1; [ exact Ha | apply seq_refl ].
    + eassumption.
    + eassumption.
    + apply IH3; [ apply seq_refl | exact Hd ].
Qed.

Definition exec_eq G := proj1 (exec_loopx_eq G).
Definition loopx_eq G := proj2 (exec_loopx_eq G).

(* ------------------------------------------------------------------ *)
(** * Inversion is a semantic inverse                                  *)
(* ------------------------------------------------------------------ *)

(** A loop tail always stops in a state satisfying its exit assertion. *)
Lemma loopx_exit : forall G e1 s1 s2 e2 a b,
  loopx G e1 s1 s2 e2 a b -> eval e2 b <> 0.
Proof.
  intros G e1 s1 s2 e2 a b H; induction H.
  - rewrite <- (eval_seq e2 a b H0); assumption.
  - assumption.
Qed.

(** The two statements proved by mutual induction.  For loop tails the
    statement threads a continuation: given the already-reversed step that
    entered [a] and a reversed tail continuing from [t], the reversed run of
    the whole tail reaches that continuation. *)
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
    intros a b Hab. apply E_skip. now apply seq_sym.
  - (* assign *)
    intros x o e a b Hn Hb. apply E_assign; auto.
    intro y. unfold update.
    destruct (Nat.eqb x y) eqn:E.
    + apply Nat.eqb_eq in E; subst y.
      rewrite (eval_off e a b x (mapp o (a x) (eval e a)) Hn Hb).
      rewrite (Hb x). unfold update. rewrite Nat.eqb_refl.
      symmetry. apply mapp_minv.
    + rewrite (Hb y). unfold update. rewrite E. reflexivity.
  - (* swap *)
    intros x y a b Hb. apply E_swap. intro z.
    assert (Hbx : b x = if Nat.eqb y x then a x else a y).
    { rewrite (Hb x). unfold update. rewrite Nat.eqb_refl.
      destruct (Nat.eqb y x); reflexivity. }
    assert (Hby : b y = a x).
    { rewrite (Hb y). unfold update. rewrite Nat.eqb_refl. reflexivity. }
    rewrite Hbx, Hby. unfold update.
    destruct (Nat.eqb y z) eqn:Eyz.
    + apply Nat.eqb_eq in Eyz; subst z.
      destruct (Nat.eqb y x) eqn:Eyx.
      * apply Nat.eqb_eq in Eyx; subst y. reflexivity.
      * reflexivity.
    + destruct (Nat.eqb x z) eqn:Exz.
      * apply Nat.eqb_eq in Exz; subst z.
        destruct (Nat.eqb y x) eqn:Eyx.
        -- congruence.
        -- reflexivity.
      * rewrite (Hb z). unfold update. rewrite Eyz, Exz. reflexivity.
  - (* seq *)
    intros s1 s2 a b c H1 IH1 H2 IH2. simpl. eapply E_seq; eassumption.
  - (* if true *)
    intros e1 s1 s2 e2 a b H1 H2 IH H3. simpl. apply E_if_t; assumption.
  - (* if false *)
    intros e1 s1 s2 e2 a b H1 H2 IH H3. simpl. apply E_if_f; assumption.
  - (* loop *)
    intros e1 s1 s2 e2 a b c H1 H2 IH1 H3 IH2. simpl.
    destruct (IH2 a a IH1 (L_done G e2 (invert s1) (invert s2) e1 a a H1 (seq_refl a)))
      as [m [Hm Hloop]].
    eapply E_loop.
    + eapply loopx_exit; eassumption.
    + eassumption.
    + assumption.
  - (* local *)
    intros x e1 s e2 a b c Hn1 Hn2 Hs IH Hx Hc. simpl.
    eapply E_local with (b := update a x (eval e1 a)); auto.
    + eapply exec_eq; [ eassumption | | apply seq_refl ].
      intro y. unfold update.
      rewrite (eval_off e2 b c x (a x) Hn2 Hc), <- Hx.
      destruct (Nat.eqb x y) eqn:E.
      * apply Nat.eqb_eq in E; subst y. reflexivity.
      * rewrite (Hc y). unfold update. rewrite E. reflexivity.
    + rewrite update_hit. symmetry. now apply eval_update_notin.
    + intro y. rewrite (Hc x). unfold update. rewrite Nat.eqb_refl.
      destruct (Nat.eqb x y) eqn:E.
      * apply Nat.eqb_eq in E; subst y. reflexivity.
      * reflexivity.
  - (* object block *)
    intros x s a b c Hs IH Hx Hc. simpl.
    eapply E_obj with (b := update a x 0).
    + eapply exec_eq; [ eassumption | | apply seq_refl ].
      intro y. unfold update.
      destruct (Nat.eqb x y) eqn:E.
      * apply Nat.eqb_eq in E; subst y. now symmetry.
      * rewrite (Hc y). unfold update. rewrite E. reflexivity.
    + apply update_hit.
    + intro y. rewrite (Hc x). unfold update. rewrite Nat.eqb_refl.
      destruct (Nat.eqb x y) eqn:E.
      * apply Nat.eqb_eq in E; subst y. reflexivity.
      * reflexivity.
  - (* call *)
    intros m s a b Hm Hs IH. simpl. eapply E_uncall; eassumption.
  - (* uncall *)
    intros m s a b Hm Hs IH. simpl. eapply E_call; [ eassumption | ].
    rewrite invert_invert in IH. assumption.
  - (* loop tail: done *)
    intros e1 s1 s2 e2 a b H1 Hab t u Ht Hu.
    exists t. split; [ | assumption ].
    eapply exec_eq; [ eassumption | assumption | apply seq_refl ].
  - (* loop tail: step *)
    intros e1 s1 s2 e2 a b c d H1 H2 IH1 H3 H4 IH2 H5 IH3 t u Ht Hu.
    apply (IH3 b u IH2).
    eapply L_step; try eassumption.
Qed.

Definition exec_invert G := proj1 (exec_loopx_invert G).

(** [invert] denotes the inverse relation. *)
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
    intros a b Hab b' H; inversion H; subst.
    eauto using seq_trans, seq_sym.
  - (* assign *)
    intros x o e a b Hn Hb b' H; inversion H; subst.
    eauto using seq_trans, seq_sym.
  - (* swap *)
    intros x y a b Hb b' H; inversion H; subst.
    eauto using seq_trans, seq_sym.
  - (* seq *)
    intros s1 s2 a b c H1 IH1 H2 IH2 c' H; inversion H; subst.
    apply IH2. eapply exec_eq; [ eassumption | | apply seq_refl ].
    apply seq_sym. now apply IH1.
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
    apply IH2. eapply loopx_eq; [ eassumption | | apply seq_refl ].
    apply seq_sym. now apply IH1.
  - (* local *)
    intros x e1 s e2 a b c Hn1 Hn2 Hs IH Hx Hc c' H; inversion H; subst.
    eapply seq_trans; [ eassumption | ].
    eapply seq_trans; [ | apply seq_sym; eassumption ].
    apply update_seq. now apply IH.
  - (* object block *)
    intros x s a b c Hs IH Hx Hc c' H; inversion H; subst.
    eapply seq_trans; [ eassumption | ].
    eapply seq_trans; [ | apply seq_sym; eassumption ].
    apply update_seq. now apply IH.
  - (* call *)
    intros m s a b Hm Hs IH b' H; inversion H; subst.
    match goal with
    | [ HG : G m = Some ?s0 |- _ ] =>
        rewrite Hm in HG; injection HG as ->
    end.
    now apply IH.
  - (* uncall *)
    intros m s a b Hm Hs IH b' H; inversion H; subst.
    match goal with
    | [ HG : G m = Some ?s0 |- _ ] =>
        rewrite Hm in HG; injection HG as ->
    end.
    now apply IH.
  - (* loop tail: done *)
    intros e1 s1 s2 e2 a b H1 Hab b' H; inversion H; subst.
    + eauto using seq_trans, seq_sym.
    + contradiction.
  - (* loop tail: step *)
    intros e1 s1 s2 e2 a b c d H1 H2 IH1 H3 H4 IH2 H5 IH3 d' H; inversion H; subst.
    + contradiction.
    + apply IH3. eapply loopx_eq; [ eassumption | | apply seq_refl ].
      apply seq_sym. apply IH2.
      eapply exec_eq; [ eassumption | | apply seq_refl ].
      apply seq_sym. now apply IH1.
Qed.

Definition exec_det G := proj1 (exec_loopx_det G).

(** **Reversibility**: the final store determines the initial store, i.e.
    every ROOPL++ statement denotes an injective partial function on stores.
    This is the property the language is designed to have, and it follows
    from [exec_invert] plus forward determinism. *)
Theorem exec_inj : forall G s a1 a2 b,
  exec G s a1 b -> exec G s a2 b -> a1 == a2.
Proof.
  intros G s a1 a2 b H1 H2.
  apply exec_invert in H1. apply exec_invert in H2.
  eapply exec_det; eassumption.
Qed.

(** Round trip: running a program and then its inverse is the identity. *)
Corollary exec_round_trip : forall G s a b c,
  exec G s a b -> exec G (invert s) b c -> a == c.
Proof.
  intros G s a b c H1 H2.
  apply exec_invert in H1.
  eapply exec_det; eassumption.
Qed.

(* ------------------------------------------------------------------ *)
(** * Sanity checks: the semantics is not vacuous                      *)
(* ------------------------------------------------------------------ *)

(** Theorems about an unsatisfiable relation would be vacuously true, so we
    exhibit concrete derivations. *)

Definition empty_env : menv := fun _ => None.
Definition zero : store := fun _ => 0.
Definition X : id := 0%nat.
Definition Y : id := 1%nat.

(** X += 3 ; X <=> Y   leaves X = 0 and Y = 3 *)
Example ex_swap :
  exists b, exec empty_env (Sseq (Sassign X MAdd (Cst 3)) (Sswap X Y)) zero b
            /\ b X = 0 /\ b Y = 3.
Proof.
  eexists. split.
  - eapply E_seq.
    + apply E_assign; [ simpl; tauto | apply seq_refl ].
    + apply E_swap. apply seq_refl.
  - split; reflexivity.
Qed.

(** from X = 0 loop X += 1 until X = 2   counts to 2 *)
Definition count2 : stm :=
  Sloop (Bop Oeq (Var X) (Cst 0)) Sskip (Sassign X MAdd (Cst 1))
        (Bop Oeq (Var X) (Cst 2)).

Example ex_loop : exists b, exec empty_env count2 zero b /\ b X = 2.
Proof.
  eexists. split.
  - eapply E_loop; [ simpl; discriminate | apply E_skip; apply seq_refl | ].
    eapply L_step; [ reflexivity | apply E_assign; [ simpl; tauto | apply seq_refl ]
                   | reflexivity | apply E_skip; apply seq_refl | ].
    eapply L_step; [ reflexivity | apply E_assign; [ simpl; tauto | apply seq_refl ]
                   | reflexivity | apply E_skip; apply seq_refl | ].
    apply L_done; [ simpl; discriminate | apply seq_refl ].
  - reflexivity.
Qed.

(** The loop really is reversible: uncounting brings the store back. *)
Example ex_loop_back : exists b, exec empty_env (invert count2) b zero.
Proof.
  destruct ex_loop as [b [Hb _]]. exists b. now apply exec_invert.
Qed.

(** A local block: local t = 3  X += t  delocal t = 3 *)
Definition T : id := 2%nat.
Example ex_local :
  exists b,
    exec empty_env
      (Slocal T (Cst 3) (Sassign X MAdd (Var T)) (Cst 3)) zero b
    /\ b X = 3 /\ b T = 0.
Proof.
  eexists. split.
  - eapply E_local; [ simpl; tauto | simpl; tauto | | | apply seq_refl ].
    + apply E_assign; [ unfold X, T; simpl; intuition discriminate | apply seq_refl ].
    + reflexivity.
  - split; reflexivity.
Qed.

(** call/uncall of a method: calling then uncalling is the identity. *)
Definition incX : stm := Sassign X MAdd (Cst 1).
Definition M0 : mid := 0%nat.
Definition genv : menv := fun m => if Nat.eqb m M0 then Some incX else None.

Example ex_call_uncall :
  exists b c, exec genv (Scall M0) zero b
              /\ exec genv (Suncall M0) b c
              /\ c == zero.
Proof.
  eexists. eexists. split; [ | split ].
  - eapply E_call; [ reflexivity | apply E_assign; [ simpl; tauto | apply seq_refl ] ].
  - eapply E_uncall; [ reflexivity | apply E_assign; [ simpl; tauto | apply seq_refl ] ].
  - intro y. unfold update, X, zero. destruct y; reflexivity.
Qed.

(** The side condition on assignment bites: `X += X` has no derivation,
    which is what keeps assignment injective. *)
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
