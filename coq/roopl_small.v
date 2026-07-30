(**
  roopl_small.v -- 小ステップ（構造的操作的）意味論と，その可逆性。

  roopl.v の大ステップ意味論 [exec] は「文を実行すると状態がこう変わる」を
  一息で述べる。ここでは実行の途中も配置（configuration）として表す小ステップ
  意味論を与え、次を機械検証する。

     step_det   前方決定性：ある配置から進める先はたかだか一つ
     step_inj   **後方決定性（＝小ステップの可逆性）**：ある配置へ入って
                こられる配置はたかだか一つ
     exec_steps 大ステップとの対応：exec s a b なら (•s, a) から (s•, b) へ
                有限回で到達する

  可逆言語の要点は step_inj の証明の中に現れる。たとえば
  [Mlp1 e1 (Mpre s1) s2 e2] という配置には
    - ループに入ってきた（[from e1] の入口表明が真）
    - 前の周回から戻ってきた（入口表明が偽）
  の二通りの入り方がありうるが、**入口表明 e1 の値がこの二つを区別する**。
  条件分岐の [Mpost (Sif ...)] も同様に出口表明 e2 で区別される。つまり
  ROOPL++ が二重ガードを持つことが、そのまま後方決定性の証明になっている。

  対象は制御構造の核（skip・可逆代入・swap・並び・条件分岐・ループ）。
  局所ブロック・オブジェクト・呼出しは roopl.v 側にあり、ここへの拡張は
  同じ形の規則を足す作業になる（README 参照）。
*)

Require Import ZArith List Bool Arith Lia Wf_nat.
Require Import ROOPL.roopl.
Import ListNotations.
Open Scope Z_scope.

(* ------------------------------------------------------------------ *)
(** * 制御トークン付きの文（ジッパー）                                  *)
(* ------------------------------------------------------------------ *)

(** [Mpre s] は「これから s を実行する」（• s），[Mpost s] は「s を実行し
    終えた」（s •）。入れ子の構成子は，プログラムのどこにトークンがあるかを
    保つ。プログラム全体が配置に残るので，配置は「どのプログラムの，どの
    位置か」を一意に表す。 *)
Inductive mstm :=
| Mpre  (s : stm)
| Mpost (s : stm)
| Mseql (m : mstm) (s2 : stm)                              (**r (m) ; s2 *)
| Mseqr (s1 : stm) (m : mstm)                              (**r s1 ; (m) *)
| Mift  (e1 : exp) (m : mstm) (s2 : stm) (e2 : exp)        (**r then 枝の中 *)
| Miff  (e1 : exp) (s1 : stm) (m : mstm) (e2 : exp)        (**r else 枝の中 *)
| Mlp1  (e1 : exp) (m : mstm) (s2 : stm) (e2 : exp)        (**r ループの s1 の中 *)
| Mlp2  (e1 : exp) (s1 : stm) (m : mstm) (e2 : exp)        (**r ループの s2 の中 *)
| Mloc  (x : id) (e1 : exp) (m : mstm) (e2 : exp) (v : Z)
   (**r 局所ブロックの中。[v] は外側の [x] の値（出るときに戻す） *)
| Mobj  (cl : cid) (x : id) (m : mstm) (h : nat).
   (**r オブジェクトブロックの中。[h] は入口でのヒープの高さ（＝確保した
        対象の位置）。局所ブロックが外側の値を退避するのと同じ役割 *)

(** トークンを消すと元の文に戻る（配置がプログラムを保っていることの確認）。 *)
Fixpoint erase (m : mstm) : stm :=
  match m with
  | Mpre s | Mpost s => s
  | Mseql m s2 => Sseq (erase m) s2
  | Mseqr s1 m => Sseq s1 (erase m)
  | Mift e1 m s2 e2 => Sif e1 (erase m) s2 e2
  | Miff e1 s1 m e2 => Sif e1 s1 (erase m) e2
  | Mlp1 e1 m s2 e2 => Sloop e1 (erase m) s2 e2
  | Mlp2 e1 s1 m e2 => Sloop e1 s1 (erase m) e2
  | Mloc x e1 m e2 _ => Slocal x e1 (erase m) e2
  | Mobj cl x m _ => Sobj cl x (erase m)
  end.



(* ------------------------------------------------------------------ *)
(** * 小ステップ関係                                                    *)
(* ------------------------------------------------------------------ *)

(** 配置は (トークン付きの文, 状態)。対ではなく 4 引数の関係にしてあるのは、
    対の添字だと induction/inversion が構成子の形を割り出せないため。 *)
Inductive step : mstm -> state -> mstm -> state -> Prop :=
(* 原子的な文 *)
(* 状態を作る他の規則と同じく、結果は点ごとに等しい状態であればよい
   （大ステップの E_skip と同じ形。厳密な等号にすると、状態が関数なので
   「点ごとには等しいが = では等しくない」状態を受け取れなくなる）。 *)
| S_skip : forall a b,
    a == b -> step (Mpre Sskip) a (Mpost Sskip) b
| S_assign : forall x o e a b,
    ~ In x (fv e) ->
    b == setv a x (mapp o (vs a x) (eval e a)) ->
    step (Mpre (Sassign x o e)) a (Mpost (Sassign x o e)) b
| S_swap : forall x y a b,
    b == setv (setv a x (vs a y)) y (vs a x) ->
    step (Mpre (Sswap x y)) a (Mpost (Sswap x y)) b
| S_show : forall e a b,
    a == b -> step (Mpre (Sshow e)) a (Mpost (Sshow e)) b
| S_fassign : forall x f o e a b l,
    os a x = Some l -> (l < hn a)%nat ->
    b == setf a l f (mapp o (hp a l f) (eval e a)) ->
    eval e b = eval e a ->
    step (Mpre (Sfassign x f o e)) a (Mpost (Sfassign x f o e)) b
| S_aassign : forall x ei o e a b l,
    os a x = Some l -> (l < hn a)%nat ->
    b == setf a l (Z.to_nat (eval ei a))
              (mapp o (hp a l (Z.to_nat (eval ei a))) (eval e a)) ->
    eval ei b = eval ei a -> eval e b = eval e a ->
    step (Mpre (Saassign x ei o e)) a (Mpost (Saassign x ei o e)) b
| S_aswap : forall x e1 y e2 a b l1 l2,
    os a x = Some l1 -> (l1 < hn a)%nat ->
    os a y = Some l2 -> (l2 < hn a)%nat ->
    b == setf (setf a l1 (Z.to_nat (eval e1 a)) (hp a l2 (Z.to_nat (eval e2 a))))
              l2 (Z.to_nat (eval e2 a)) (hp a l1 (Z.to_nat (eval e1 a))) ->
    eval e1 b = eval e1 a -> eval e2 b = eval e2 a ->
    step (Mpre (Saswap x e1 y e2)) a (Mpost (Saswap x e1 y e2)) b
| S_oswap : forall x y a b,
    b == seto (seto a x (os a y)) y (os a x) ->
    step (Mpre (Soswap x y)) a (Mpost (Soswap x y)) b
| S_copy : forall x y a b,
    x <> y -> os a y = None -> b == seto a y (os a x) ->
    step (Mpre (Scopy x y)) a (Mpost (Scopy x y)) b
| S_uncopy : forall x y a b,
    x <> y -> os a x = os a y -> b == seto a y None ->
    step (Mpre (Suncopy x y)) a (Mpost (Suncopy x y)) b

(* 並び *)
| S_seq_in : forall s1 s2 a,
    step (Mpre (Sseq s1 s2)) a (Mseql (Mpre s1) s2) a
| S_seq_l : forall m m' s2 a a',
    step m a m' a' ->
    step (Mseql m s2) a (Mseql m' s2) a'
| S_seq_mid : forall s1 s2 a,
    step (Mseql (Mpost s1) s2) a (Mseqr s1 (Mpre s2)) a
| S_seq_r : forall s1 m m' a a',
    step m a m' a' ->
    step (Mseqr s1 m) a (Mseqr s1 m') a'
| S_seq_out : forall s1 s2 a,
    step (Mseqr s1 (Mpost s2)) a (Mpost (Sseq s1 s2)) a

(* 条件分岐：入口条件で枝を選び、出口表明で枝を思い出す *)
| S_if_in_t : forall e1 s1 s2 e2 a,
    eval e1 a <> 0 ->
    step (Mpre (Sif e1 s1 s2 e2)) a (Mift e1 (Mpre s1) s2 e2) a
| S_if_in_f : forall e1 s1 s2 e2 a,
    eval e1 a = 0 ->
    step (Mpre (Sif e1 s1 s2 e2)) a (Miff e1 s1 (Mpre s2) e2) a
| S_if_t : forall e1 m m' s2 e2 a a',
    step m a m' a' ->
    step (Mift e1 m s2 e2) a (Mift e1 m' s2 e2) a'
| S_if_f : forall e1 s1 m m' e2 a a',
    step m a m' a' ->
    step (Miff e1 s1 m e2) a (Miff e1 s1 m' e2) a'
| S_if_out_t : forall e1 s1 s2 e2 a,
    eval e2 a <> 0 ->
    step (Mift e1 (Mpost s1) s2 e2) a (Mpost (Sif e1 s1 s2 e2)) a
| S_if_out_f : forall e1 s1 s2 e2 a,
    eval e2 a = 0 ->
    step (Miff e1 s1 (Mpost s2) e2) a (Mpost (Sif e1 s1 s2 e2)) a

(* ループ：入口表明で「入ってきた／戻ってきた」を区別する *)
| S_lp_in : forall e1 s1 s2 e2 a,
    eval e1 a <> 0 ->
    step (Mpre (Sloop e1 s1 s2 e2)) a (Mlp1 e1 (Mpre s1) s2 e2) a
| S_lp_1 : forall e1 m m' s2 e2 a a',
    step m a m' a' ->
    step (Mlp1 e1 m s2 e2) a (Mlp1 e1 m' s2 e2) a'
| S_lp_exit : forall e1 s1 s2 e2 a,
    eval e2 a <> 0 ->
    step (Mlp1 e1 (Mpost s1) s2 e2) a (Mpost (Sloop e1 s1 s2 e2)) a
| S_lp_more : forall e1 s1 s2 e2 a,
    eval e2 a = 0 ->
    step (Mlp1 e1 (Mpost s1) s2 e2) a (Mlp2 e1 s1 (Mpre s2) e2) a
| S_lp_2 : forall e1 s1 m m' e2 a a',
    step m a m' a' ->
    step (Mlp2 e1 s1 m e2) a (Mlp2 e1 s1 m' e2) a'
| S_lp_back : forall e1 s1 s2 e2 a,
    eval e1 a = 0 ->
    step (Mlp2 e1 s1 (Mpost s2) e2) a (Mlp1 e1 (Mpre s1) s2 e2) a

(* 局所ブロック：入るときに外側の値を文脈へ退避し、出るときに戻す *)
| S_loc_in : forall x e1 s e2 a b,
    ~ In x (fv e1) -> ~ In x (fv e2) ->
    b == setv a x (eval e1 a) ->
    step (Mpre (Slocal x e1 s e2)) a (Mloc x e1 (Mpre s) e2 (vs a x)) b
| S_loc : forall x e1 m m' e2 v a a',
    step m a m' a' ->
    step (Mloc x e1 m e2 v) a (Mloc x e1 m' e2 v) a'
| S_loc_out : forall x e1 s e2 v a b,
    ~ In x (fv e2) ->
    vs a x = eval e2 a ->
    b == setv a x v ->
    step (Mloc x e1 (Mpost s) e2 v) a (Mpost (Slocal x e1 s e2)) b

(* オブジェクトブロック：入口で確保し、出口でゼロクリアを確かめて解放する。
   確保した対象は常にヒープの一番上（高さ [hn] が退避値の役をする）。 *)
| S_obj_in : forall cl x s a b,
    os a x = None ->
    b == alloc a cl x ->
    step (Mpre (Sobj cl x s)) a (Mobj cl x (Mpre s) (hn a)) b
| S_obj : forall cl x m m' h a a',
    step m a m' a' ->
    step (Mobj cl x m h) a (Mobj cl x m' h) a'
| S_obj_out : forall cl x s h a b,
    hn a = S h ->
    os a x = Some h ->
    (forall f, hp a h f = 0) ->
    hc a h = cl ->
    b == dealloc a x ->
    step (Mobj cl x (Mpost s) h) a (Mpost (Sobj cl x s)) b.

(** 多ステップ *)
Inductive steps : mstm -> state -> mstm -> state -> Prop :=
| steps_refl : forall m a, steps m a m a
| steps_step : forall m1 a1 m2 a2 m3 a3,
    step m1 a1 m2 a2 -> steps m2 a2 m3 a3 -> steps m1 a1 m3 a3.

Lemma steps_one : forall m1 a1 m2 a2, step m1 a1 m2 a2 -> steps m1 a1 m2 a2.
Proof. intros; eapply steps_step; [ eassumption | apply steps_refl ]. Qed.

Lemma steps_trans : forall m1 a1 m2 a2 m3 a3,
  steps m1 a1 m2 a2 -> steps m2 a2 m3 a3 -> steps m1 a1 m3 a3.
Proof.
  intros m1 a1 m2 a2 m3 a3 H; induction H; intro H2; [ assumption | ].
  eapply steps_step; [ eassumption | auto ].
Qed.

(* ------------------------------------------------------------------ *)
(** * 進めない／戻れない形                                              *)
(* ------------------------------------------------------------------ *)

(** 実行し終えた文からは進めない（[s •] は終端）。 *)
Lemma no_step_from_post : forall s a m' a', ~ step (Mpost s) a m' a'.
Proof. intros s a m' a' H; inversion H. Qed.

(** どの規則も「これから実行する」形を作らない。ゆえに [• s] へは戻れない。 *)
Lemma no_step_to_pre : forall m a s a', ~ step m a (Mpre s) a'.
Proof. intros m a s a' H; inversion H. Qed.

(** 原子文（文脈へ入らない文）。 *)
Inductive atomic : stm -> Prop :=
| A_skip : atomic Sskip
| A_assign : forall x o e, atomic (Sassign x o e)
| A_swap : forall x y, atomic (Sswap x y)
| A_show : forall e, atomic (Sshow e)
| A_fassign : forall x f o e, atomic (Sfassign x f o e)
| A_aassign : forall x ei o e, atomic (Saassign x ei o e)
| A_aswap : forall x e1 y e2, atomic (Saswap x e1 y e2)
| A_oswap : forall x y, atomic (Soswap x y)
| A_copy : forall x y, atomic (Scopy x y)
| A_uncopy : forall x y, atomic (Suncopy x y).

(** 内容のないメソッド環境（原子文の意味は環境に依らない）。 *)
Definition dummy_env : menv := MEnv (fun _ => None) (fun _ => None).

(** [• s] から [s •] への一歩は、原子文の大ステップ一歩そのもの
    （並び・分岐・ループは文脈へ入るので、この形にはならない）。 *)
Lemma atom_exec : forall G s a b,
  step (Mpre s) a (Mpost s) b -> exec G s a b.
Proof.
  intros G s a b H; inversion H; subst.
  - apply E_skip; assumption.
  - apply E_assign; assumption.
  - apply E_swap; assumption.
  - apply E_show; assumption.
  - eapply E_fassign; eassumption.
  - eapply E_aassign; eassumption.
  - eapply E_aswap; eassumption.
  - apply E_oswap; assumption.
  - apply E_copy; assumption.
  - apply E_uncopy; assumption.
Qed.

(** その逆。原子文の大ステップ一歩は小ステップ一歩でもある。 *)
Lemma exec_atom : forall G s a b,
  atomic s -> exec G s a b -> step (Mpre s) a (Mpost s) b.
Proof.
  intros G s a b Hat H; inversion Hat; subst; inversion H; subst;
    econstructor; eassumption.
Qed.

(** したがって原子文の局所可逆性は、大ステップの可逆性 [exec_inj] から出る。 *)
Lemma atom_inj : forall s a1 a2 b,
  step (Mpre s) a1 (Mpost s) b -> step (Mpre s) a2 (Mpost s) b -> a1 == a2.
Proof.
  intros s a1 a2 b H1 H2.
  eapply exec_inj with (G := dummy_env); eapply atom_exec; eassumption.
Qed.

Ltac impossible_step :=
  match goal with
  | [ H : step (Mpost _) _ _ _ |- _ ] => exfalso; eapply no_step_from_post; eassumption
  | [ H : step _ _ (Mpre _) _ |- _ ] => exfalso; eapply no_step_to_pre; eassumption
  end.

(* ------------------------------------------------------------------ *)
(** * 前方決定性                                                        *)
(* ------------------------------------------------------------------ *)

(** 状態は点ごとの等しさで比べる（規則が状態を [==] で関係づけているため）。 *)
Theorem step_det : forall m a m1 a1 m2 a2,
  step m a m1 a1 -> step m a m2 a2 -> m1 = m2 /\ a1 == a2.
Proof.
  intros m a m1 a1 mz az H1; revert mz az.
  induction H1; intros mz az Hz; inversion Hz; subst;
    try impossible_step;
    try congruence;
    try (split; [ reflexivity | eauto using steq_trans, steq_sym ]);
    try (match goal with
         | [ IH : forall m0 a0, step ?mm ?aa m0 a0 -> _,
             HS : step ?mm ?aa ?mm' ?aa' |- _ ] =>
             destruct (IH mm' aa' HS) as [ Em Ea ]; subst;
             split; [ reflexivity | assumption ]
         end).
  (* 残るのは新しい原子文のケース：まず位置を os から同定し、
     結果が同じ状態に == であることから合流させる *)
  all: lazymatch goal with
       | [ |- _ = _ /\ steq _ _ ] => split; [ reflexivity | ]
       | _ => idtac
       end.
  all: repeat match goal with
       | [ HA : os ?aa ?xx = Some ?l1, HB : os ?aa ?xx = Some ?l2 |- _ ] =>
           tryif constr_eq l1 l2 then fail else
             (assert (l1 = l2) by congruence; subst)
       end.
  all: solve [ eauto using steq_trans, steq_sym
             | eapply steq_trans; [ eassumption | apply steq_sym; eassumption ] ].
Qed.

(* ------------------------------------------------------------------ *)
(** * 原子的な文の局所可逆性                                            *)
(* ------------------------------------------------------------------ *)

(** 可逆代入は、結果から入口の状態を一意に決める。副条件
    「更新する変数が右辺に現れない」がここで効く。 *)
Lemma assign_inj : forall x o e a1 a2 b,
  ~ In x (fv e) ->
  b == setv a1 x (mapp o (vs a1 x) (eval e a1)) ->
  b == setv a2 x (mapp o (vs a2 x) (eval e a2)) ->
  a1 == a2.
Proof.
  intros x o e a1 a2 b Hn H1 H2.
  assert (E1 : eval e b = eval e a1) by (eapply eval_off_v; eauto).
  assert (E2 : eval e b = eval e a2) by (eapply eval_off_v; eauto).
  assert (V1 : vs a1 x = mapp (minv o) (vs b x) (eval e b)).
  { rewrite (steq_vs b _ x H1); simpl; rewrite Nat.eqb_refl, E1.
    symmetry; apply mapp_minv. }
  assert (V2 : vs a2 x = mapp (minv o) (vs b x) (eval e b)).
  { rewrite (steq_vs b _ x H2); simpl; rewrite Nat.eqb_refl, E2.
    symmetry; apply mapp_minv. }
  steq_split.
  - intro y; destruct (Nat.eqb x y) eqn:E.
    + apply Nat.eqb_eq in E; subst y; rewrite V1, V2; reflexivity.
    + assert (Q1 : vs b y = vs a1 y)
        by (rewrite (steq_vs b _ y H1); simpl; now rewrite E).
      assert (Q2 : vs b y = vs a2 y)
        by (rewrite (steq_vs b _ y H2); simpl; now rewrite E).
      rewrite <- Q1, <- Q2; reflexivity.
  - intro y.
    assert (Q1 : os b y = os a1 y) by (rewrite (steq_os b _ y H1); reflexivity).
    assert (Q2 : os b y = os a2 y) by (rewrite (steq_os b _ y H2); reflexivity).
    rewrite <- Q1, <- Q2; reflexivity.
  - assert (Q1 : hn b = hn a1) by (rewrite (steq_hn b _ H1); reflexivity).
    assert (Q2 : hn b = hn a2) by (rewrite (steq_hn b _ H2); reflexivity).
    rewrite <- Q1, <- Q2; reflexivity.
  - intros l f Hl.
    assert (Hb : (l < hn b)%nat) by (rewrite (steq_hn b _ H1); simpl; assumption).
    assert (Q1 : hp b l f = hp a1 l f)
      by (rewrite (steq_hp b _ l f H1); [ reflexivity | assumption ]).
    assert (Q2 : hp b l f = hp a2 l f)
      by (rewrite (steq_hp b _ l f H2); [ reflexivity | assumption ]).
    rewrite <- Q1, <- Q2; reflexivity.
  - intros l Hl.
    assert (Hb : (l < hn b)%nat) by (rewrite (steq_hn b _ H1); simpl; assumption).
    assert (Q1 : hc b l = hc a1 l)
      by (rewrite (steq_hc b _ l H1); [ reflexivity | assumption ]).
    assert (Q2 : hc b l = hc a2 l)
      by (rewrite (steq_hc b _ l H2); [ reflexivity | assumption ]).
    rewrite <- Q1, <- Q2; reflexivity.
Qed.

(** 入れ替えも同様（x = y の場合も含む）。 *)
Lemma swap_inj : forall x y a1 a2 b,
  b == setv (setv a1 x (vs a1 y)) y (vs a1 x) ->
  b == setv (setv a2 x (vs a2 y)) y (vs a2 x) ->
  a1 == a2.
Proof.
  intros x y a1 a2 b H1 H2.
  assert (K : forall a, b == setv (setv a x (vs a y)) y (vs a x) ->
              forall z, vs a z = if Nat.eqb y z then vs b x
                                 else if Nat.eqb x z then vs b y else vs b z).
  { intros a H z.
    assert (Bx : vs b x = if Nat.eqb y x then vs a x else vs a y).
    { rewrite (steq_vs b _ x H); simpl; rewrite Nat.eqb_refl.
      destruct (Nat.eqb y x); reflexivity. }
    assert (By : vs b y = vs a x)
      by (rewrite (steq_vs b _ y H); simpl; now rewrite Nat.eqb_refl).
    destruct (Nat.eqb y z) eqn:Eyz.
    - apply Nat.eqb_eq in Eyz; subst z. rewrite Bx.
      destruct (Nat.eqb y x) eqn:Eyx;
        [ apply Nat.eqb_eq in Eyx; subst y; reflexivity | reflexivity ].
    - destruct (Nat.eqb x z) eqn:Exz.
      + apply Nat.eqb_eq in Exz; subst z; now rewrite By.
      + assert (Q : vs b z = vs a z)
          by (rewrite (steq_vs b _ z H); simpl; now rewrite Eyz, Exz).
        now rewrite Q. }
  steq_split.
  - intro z; rewrite (K a1 H1 z), (K a2 H2 z); reflexivity.
  - intro z.
    assert (Q1 : os b z = os a1 z) by (rewrite (steq_os b _ z H1); reflexivity).
    assert (Q2 : os b z = os a2 z) by (rewrite (steq_os b _ z H2); reflexivity).
    rewrite <- Q1, <- Q2; reflexivity.
  - assert (Q1 : hn b = hn a1) by (rewrite (steq_hn b _ H1); reflexivity).
    assert (Q2 : hn b = hn a2) by (rewrite (steq_hn b _ H2); reflexivity).
    rewrite <- Q1, <- Q2; reflexivity.
  - intros l f Hl.
    assert (Hb : (l < hn b)%nat) by (rewrite (steq_hn b _ H1); simpl; assumption).
    assert (Q1 : hp b l f = hp a1 l f)
      by (rewrite (steq_hp b _ l f H1); [ reflexivity | assumption ]).
    assert (Q2 : hp b l f = hp a2 l f)
      by (rewrite (steq_hp b _ l f H2); [ reflexivity | assumption ]).
    rewrite <- Q1, <- Q2; reflexivity.
  - intros l Hl.
    assert (Hb : (l < hn b)%nat) by (rewrite (steq_hn b _ H1); simpl; assumption).
    assert (Q1 : hc b l = hc a1 l)
      by (rewrite (steq_hc b _ l H1); [ reflexivity | assumption ]).
    assert (Q2 : hc b l = hc a2 l)
      by (rewrite (steq_hc b _ l H2); [ reflexivity | assumption ]).
    rewrite <- Q1, <- Q2; reflexivity.
Qed.

(** 局所ブロックに入る一歩の単射性。文脈が退避値 [vs a x] を持つので、
    同じ配置へ来た二つの状態は [x] でも一致する。 *)
Lemma loc_in_inj : forall x e1 a az b,
  b == setv a x (eval e1 a) -> b == setv az x (eval e1 az) ->
  vs a x = vs az x -> a == az.
Proof.
  intros x e1 a az b H1 H2 Hx.
  steq_split.
  - intro y; destruct (Nat.eqb x y) eqn:E.
    + apply Nat.eqb_eq in E; subst y; assumption.
    + assert (Q1 : vs b y = vs a y)
        by (rewrite (steq_vs b _ y H1); simpl; now rewrite E).
      assert (Q2 : vs b y = vs az y)
        by (rewrite (steq_vs b _ y H2); simpl; now rewrite E).
      rewrite <- Q1, <- Q2; reflexivity.
  - intro y.
    assert (Q1 : os b y = os a y) by (rewrite (steq_os b _ y H1); reflexivity).
    assert (Q2 : os b y = os az y) by (rewrite (steq_os b _ y H2); reflexivity).
    rewrite <- Q1, <- Q2; reflexivity.
  - assert (Q1 : hn b = hn a) by (rewrite (steq_hn b _ H1); reflexivity).
    assert (Q2 : hn b = hn az) by (rewrite (steq_hn b _ H2); reflexivity).
    rewrite <- Q1, <- Q2; reflexivity.
  - intros l f Hl.
    assert (Hb : (l < hn b)%nat) by (rewrite (steq_hn b _ H1); simpl; assumption).
    assert (Q1 : hp b l f = hp a l f)
      by (rewrite (steq_hp b _ l f H1); [ reflexivity | assumption ]).
    assert (Q2 : hp b l f = hp az l f)
      by (rewrite (steq_hp b _ l f H2); [ reflexivity | assumption ]).
    rewrite <- Q1, <- Q2; reflexivity.
  - intros l Hl.
    assert (Hb : (l < hn b)%nat) by (rewrite (steq_hn b _ H1); simpl; assumption).
    assert (Q1 : hc b l = hc a l)
      by (rewrite (steq_hc b _ l H1); [ reflexivity | assumption ]).
    assert (Q2 : hc b l = hc az l)
      by (rewrite (steq_hc b _ l H2); [ reflexivity | assumption ]).
    rewrite <- Q1, <- Q2; reflexivity.
Qed.

(** 局所ブロックを出る一歩の単射性。退避値も一意に決まる
    （出口表明 [vs a x = eval e2 a] が [x] の値を復元させる）。 *)
Lemma loc_out_inj : forall x e2 v v0 a az b,
  ~ In x (fv e2) ->
  vs a x = eval e2 a -> b == setv a x v ->
  vs az x = eval e2 az -> b == setv az x v0 ->
  v = v0 /\ a == az.
Proof.
  intros x e2 v v0 a az b Hn Ha H1 Haz H2.
  assert (Ev : v = vs b x)
    by (rewrite (steq_vs b _ x H1); simpl; now rewrite Nat.eqb_refl).
  assert (Ev0 : v0 = vs b x)
    by (rewrite (steq_vs b _ x H2); simpl; now rewrite Nat.eqb_refl).
  assert (E1 : eval e2 b = eval e2 a) by (eapply eval_off_v; eauto).
  assert (E2 : eval e2 b = eval e2 az) by (eapply eval_off_v; eauto).
  split; [ congruence | ].
  assert (Hx : vs a x = vs az x) by congruence.
  steq_split.
  - intro y; destruct (Nat.eqb x y) eqn:E.
    + apply Nat.eqb_eq in E; subst y; assumption.
    + assert (Q1 : vs b y = vs a y)
        by (rewrite (steq_vs b _ y H1); simpl; now rewrite E).
      assert (Q2 : vs b y = vs az y)
        by (rewrite (steq_vs b _ y H2); simpl; now rewrite E).
      rewrite <- Q1, <- Q2; reflexivity.
  - intro y.
    assert (Q1 : os b y = os a y) by (rewrite (steq_os b _ y H1); reflexivity).
    assert (Q2 : os b y = os az y) by (rewrite (steq_os b _ y H2); reflexivity).
    rewrite <- Q1, <- Q2; reflexivity.
  - assert (Q1 : hn b = hn a) by (rewrite (steq_hn b _ H1); reflexivity).
    assert (Q2 : hn b = hn az) by (rewrite (steq_hn b _ H2); reflexivity).
    rewrite <- Q1, <- Q2; reflexivity.
  - intros l f Hl.
    assert (Hb : (l < hn b)%nat) by (rewrite (steq_hn b _ H1); simpl; assumption).
    assert (Q1 : hp b l f = hp a l f)
      by (rewrite (steq_hp b _ l f H1); [ reflexivity | assumption ]).
    assert (Q2 : hp b l f = hp az l f)
      by (rewrite (steq_hp b _ l f H2); [ reflexivity | assumption ]).
    rewrite <- Q1, <- Q2; reflexivity.
  - intros l Hl.
    assert (Hb : (l < hn b)%nat) by (rewrite (steq_hn b _ H1); simpl; assumption).
    assert (Q1 : hc b l = hc a l)
      by (rewrite (steq_hc b _ l H1); [ reflexivity | assumption ]).
    assert (Q2 : hc b l = hc az l)
      by (rewrite (steq_hc b _ l H2); [ reflexivity | assumption ]).
    rewrite <- Q1, <- Q2; reflexivity.
Qed.

(** オブジェクトブロックに入る一歩の単射性。確保はヒープの一番上に載せる
    決定的な操作なので、確保後の状態から確保前の状態が復元できる。 *)
Lemma obj_in_inj : forall cl x a1 a2 b,
  os a1 x = None -> b == alloc a1 cl x ->
  os a2 x = None -> b == alloc a2 cl x ->
  a1 == a2.
Proof.
  intros cl x a1 a2 b Hx1 H1 Hx2 H2.
  assert (Hn1 : hn b = S (hn a1)) by (rewrite (steq_hn b _ H1); reflexivity).
  assert (Hn2 : hn b = S (hn a2)) by (rewrite (steq_hn b _ H2); reflexivity).
  assert (Hnn : hn a1 = hn a2) by lia.
  steq_split.
  - intro y.
    assert (Q1 : vs b y = vs a1 y) by (rewrite (steq_vs b _ y H1); reflexivity).
    assert (Q2 : vs b y = vs a2 y) by (rewrite (steq_vs b _ y H2); reflexivity).
    rewrite <- Q1, <- Q2; reflexivity.
  - intro y; destruct (Nat.eqb x y) eqn:E.
    + apply Nat.eqb_eq in E; subst y; rewrite Hx1, Hx2; reflexivity.
    + assert (Q1 : os b y = os a1 y)
        by (rewrite (steq_os b _ y H1); simpl; now rewrite E).
      assert (Q2 : os b y = os a2 y)
        by (rewrite (steq_os b _ y H2); simpl; now rewrite E).
      rewrite <- Q1, <- Q2; reflexivity.
  - assumption.
  - intros l f Hl.
    assert (Hb : (l < hn b)%nat) by lia.
    assert (Q1 : hp b l f = hp a1 l f).
    { rewrite (steq_hp b _ l f H1) by assumption. simpl.
      destruct (Nat.eqb l (hn a1)) eqn:E; [ apply Nat.eqb_eq in E; lia | reflexivity ]. }
    assert (Q2 : hp b l f = hp a2 l f).
    { rewrite (steq_hp b _ l f H2) by assumption. simpl.
      destruct (Nat.eqb l (hn a2)) eqn:E; [ apply Nat.eqb_eq in E; lia | reflexivity ]. }
    rewrite <- Q1, <- Q2; reflexivity.
  - intros l Hl.
    assert (Hb : (l < hn b)%nat) by lia.
    assert (Q1 : hc b l = hc a1 l).
    { rewrite (steq_hc b _ l H1) by assumption. simpl.
      destruct (Nat.eqb l (hn a1)) eqn:E; [ apply Nat.eqb_eq in E; lia | reflexivity ]. }
    assert (Q2 : hc b l = hc a2 l).
    { rewrite (steq_hc b _ l H2) by assumption. simpl.
      destruct (Nat.eqb l (hn a2)) eqn:E; [ apply Nat.eqb_eq in E; lia | reflexivity ]. }
    rewrite <- Q1, <- Q2; reflexivity.
Qed.

(** 出る一歩の単射性。解放前の状態は、解放後の状態と
    「一番上のセルはゼロクリア済み・クラスは cl・[x] はそこを指す」から復元できる。 *)
Lemma obj_out_inj : forall cl x h a1 a2 b,
  hn a1 = S h -> os a1 x = Some h ->
  (forall f, hp a1 h f = 0) -> hc a1 h = cl -> b == dealloc a1 x ->
  hn a2 = S h -> os a2 x = Some h ->
  (forall f, hp a2 h f = 0) -> hc a2 h = cl -> b == dealloc a2 x ->
  a1 == a2.
Proof.
  intros cl x h a1 a2 b Hp1 Ho1 Hz1 Hc1 H1 Hp2 Ho2 Hz2 Hc2 H2.
  assert (Hnn : hn a1 = hn a2) by lia.
  steq_split.
  - intro y.
    assert (Q1 : vs b y = vs a1 y) by (rewrite (steq_vs b _ y H1); reflexivity).
    assert (Q2 : vs b y = vs a2 y) by (rewrite (steq_vs b _ y H2); reflexivity).
    rewrite <- Q1, <- Q2; reflexivity.
  - intro y; destruct (Nat.eqb x y) eqn:E.
    + apply Nat.eqb_eq in E; subst y; rewrite Ho1, Ho2; reflexivity.
    + assert (Q1 : os b y = os a1 y)
        by (rewrite (steq_os b _ y H1); simpl; now rewrite E).
      assert (Q2 : os b y = os a2 y)
        by (rewrite (steq_os b _ y H2); simpl; now rewrite E).
      rewrite <- Q1, <- Q2; reflexivity.
  - assumption.
  - intros l f Hl.
    destruct (Nat.eqb l h) eqn:E.
    + apply Nat.eqb_eq in E; subst l. rewrite Hz1, Hz2; reflexivity.
    + apply Nat.eqb_neq in E.
      assert (Hn1 : hn b = pred (hn a1)) by (rewrite (steq_hn b _ H1); reflexivity).
      assert (Hb : (l < hn b)%nat) by lia.
      assert (Q1 : hp b l f = hp a1 l f)
        by (rewrite (steq_hp b _ l f H1); [ reflexivity | assumption ]).
      assert (Q2 : hp b l f = hp a2 l f)
        by (rewrite (steq_hp b _ l f H2); [ reflexivity | assumption ]).
      rewrite <- Q1, <- Q2; reflexivity.
  - intros l Hl.
    destruct (Nat.eqb l h) eqn:E.
    + apply Nat.eqb_eq in E; subst l. rewrite Hc1, Hc2; reflexivity.
    + apply Nat.eqb_neq in E.
      assert (Hn1 : hn b = pred (hn a1)) by (rewrite (steq_hn b _ H1); reflexivity).
      assert (Hb : (l < hn b)%nat) by lia.
      assert (Q1 : hc b l = hc a1 l)
        by (rewrite (steq_hc b _ l H1); [ reflexivity | assumption ]).
      assert (Q2 : hc b l = hc a2 l)
        by (rewrite (steq_hc b _ l H2); [ reflexivity | assumption ]).
      rewrite <- Q1, <- Q2; reflexivity.
Qed.

(** 退避した高さも一意に決まる（解放後の高さがそれを教える）。 *)
Lemma obj_out_h : forall x h h0 a1 a2 b,
  hn a1 = S h -> b == dealloc a1 x ->
  hn a2 = S h0 -> b == dealloc a2 x ->
  h = h0.
Proof.
  intros x h h0 a1 a2 b Hp1 H1 Hp2 H2.
  assert (Q1 : hn b = pred (hn a1)) by (rewrite (steq_hn b _ H1); reflexivity).
  assert (Q2 : hn b = pred (hn a2)) by (rewrite (steq_hn b _ H2); reflexivity).
  lia.
Qed.

(* ------------------------------------------------------------------ *)
(** * 後方決定性（小ステップの可逆性）                                  *)
(* ------------------------------------------------------------------ *)

(** ある配置へ入ってこられる配置はたかだか一つ。

    面白いのは衝突しそうな場面がすべて**言語の表明**で分かれることである：
    - [Mlp1 e1 (Mpre s1) s2 e2] へはループに入るときと周回して戻るときの
      二通りで来られるが、入口表明 e1 の真偽が両者を排他にする
    - [Mpost (Sif ...)] へは then 枝と else 枝の二通りだが、出口表明 e2 が
      これを分ける
    二重ガードを持つ ROOPL++ の設計が、そのまま後方決定性になっている。 *)
Theorem step_inj : forall m1 a1 m2 a2 m a,
  step m1 a1 m a -> step m2 a2 m a -> m1 = m2 /\ a1 == a2.
Proof.
  intros m1 a1 mz az m a H1; revert mz az.
  induction H1; intros mz az Hz; inversion Hz; subst;
    try impossible_step;
    try congruence;
    try (split; [ reflexivity | apply steq_refl ]);
    try (split; [ reflexivity | eapply assign_inj; eassumption ]);
    try (split; [ reflexivity | eapply swap_inj; eassumption ]);
    try (match goal with
         | [ IH : forall m0 a0, step m0 a0 ?mm' ?aa' -> _,
             HS : step ?mm ?aa ?mm' ?aa' |- _ ] =>
             destruct (IH mm aa HS) as [ Em Ea ]; subst;
             split; [ reflexivity | assumption ]
         end).
  (* 残るのは原子文のケース。前提から小ステップの一歩を組み立て直し、
     大ステップの可逆性を通す atom_inj に渡す *)
  (* 局所ブロックの出口：退避した値も一致することを先に示す
     （配置に v が入っているので、目標の第一成分は reflexivity では閉じない） *)
  all: try (match goal with
       | [ Hn : ~ In ?x (fv ?e2), Ha : vs ?a ?x = eval ?e2 ?a,
           H1 : ?b == setv ?a ?x ?v, Haz : vs ?az ?x = eval ?e2 ?az,
           H2 : ?b == setv ?az ?x ?v0 |- _ ] =>
           destruct (loc_out_inj x e2 v v0 a az b Hn Ha H1 Haz H2) as [ Hv Hst ];
           subst; split; [ reflexivity | assumption ]
       end).
  (* 局所ブロックの入口：配置が退避値を持つので x でも一致する *)
  all: try (match goal with
       | [ H1 : ?b == setv ?a ?x (eval ?e1 ?a),
           H2 : ?b == setv ?az ?x (eval ?e1 ?az) |- _ ] =>
           split; [ reflexivity | ];
           eapply loc_in_inj; [ eassumption | eassumption | congruence ]
       end).
  (* オブジェクトブロックの出口：退避した高さの一致を先に示す *)
  all: try (match goal with
       | [ Hh1 : hn ?aa = S ?hh, Hb1 : ?b == dealloc ?aa ?xx,
           Hh2 : hn ?az = S ?hh0, Hb2 : ?b == dealloc ?az ?xx |- _ ] =>
           assert (hh = hh0) by (eapply obj_out_h; eassumption); subst;
           split; [ reflexivity | ];
           eapply obj_out_inj; (eassumption || reflexivity)
       end).
  all: lazymatch goal with
       | [ |- _ = _ /\ steq _ _ ] => split; [ reflexivity | ]
       | _ => idtac
       end.
  all: try solve [ eauto using steq_trans, steq_sym ].
  (* 文の形を先に固定しないと、たとえば配列代入の目標に S_fassign が
     当たってしまう（動的添字を静的フィールドとして読んでしまう）ので、
     atom_inj には文を明示して渡す *)
  all: try solve
       [ eapply (atom_inj (Sfassign _ _ _ _)); (eapply S_fassign; eassumption)
       | eapply (atom_inj (Saassign _ _ _ _)); (eapply S_aassign; eassumption)
       | eapply (atom_inj (Saswap _ _ _ _));  (eapply S_aswap; eassumption)
       | eapply (atom_inj (Soswap _ _));      (eapply S_oswap; eassumption)
       | eapply (atom_inj (Scopy _ _));       (eapply S_copy; eassumption)
       | eapply (atom_inj (Suncopy _ _));     (eapply S_uncopy; eassumption) ].
  (* 配列の入れ替えは os の前提が 2 つあり、eassumption だと x と y の
     対応づけを取り違えるので、文を具体的に与えて曖昧さを消す *)
  all: try (match goal with
       | [ HZ : step _ _ (Mpost (Saswap ?xx ?ee1 ?yy ?ee2)) _ |- _ ] =>
           eapply (atom_inj (Saswap xx ee1 yy ee2)); (eapply S_aswap; eassumption)
       end).
  (* オブジェクトブロックの出入り *)
  all: try (split; [ reflexivity | ]).
  (* クラス名が [hc a (pred (hn a))] に単一化されている場合があるので、
     その前提は reflexivity で閉じる *)
  all: solve [ eapply obj_in_inj; (eassumption || reflexivity)
             | eapply obj_out_inj; (eassumption || reflexivity)
             | split; [ reflexivity | ];
               solve [ eapply obj_in_inj; (eassumption || reflexivity)
                     | eapply obj_out_inj; (eassumption || reflexivity) ] ].
  (* skip は結果を == で関係づけるので、両方の入口が同じ結果に等しい *)
  all: split; [ reflexivity | eauto using steq_trans, steq_sym ].
Qed.

(* ------------------------------------------------------------------ *)
(** * プログラムは配置に保たれる                                        *)
(* ------------------------------------------------------------------ *)

(** ステップはトークンを動かすだけで、プログラム本体は変えない。だから
    配置は「どのプログラムの、どの位置にいるか」を表しており、後方決定性が
    プログラムをまたいで壊れることがない。 *)
Theorem step_preserves_program : forall m a m' a',
  step m a m' a' -> erase m = erase m'.
Proof.
  intros m a m' a' H; induction H; simpl; try reflexivity;
    try (rewrite IHstep; reflexivity).
Qed.

Corollary steps_preserve_program : forall m a m' a',
  steps m a m' a' -> erase m = erase m'.
Proof.
  intros m a m' a' H; induction H; [ reflexivity | ].
  rewrite <- IHsteps; eapply step_preserves_program; eassumption.
Qed.

(* ------------------------------------------------------------------ *)
(** * 状態の点ごとの等しさに関する合同性                                *)
(* ------------------------------------------------------------------ *)

(** 1 ステップは状態の [==] を尊重する（大ステップ側の exec_loopx_eq に対応）。 *)
Theorem step_eq : forall m a m' a',
  step m a m' a' -> forall a2, a == a2 ->
  exists a2', step m a2 m' a2' /\ a' == a2'.
Proof.
  intros m a m' a' H; induction H; intros a2 Ha.
  - exists a2; split; [ apply S_skip, steq_refl | ].
    eauto using steq_trans, steq_sym.
  - exists (setv a2 x (mapp o (vs a2 x) (eval e a2))); split.
    + apply S_assign; [ assumption | apply steq_refl ].
    + eapply steq_trans; [ eassumption | ].
      rewrite (steq_vs a a2 x Ha), (eval_steq e a a2 Ha); now apply setv_steq.
  - exists (setv (setv a2 x (vs a2 y)) y (vs a2 x)); split.
    + apply S_swap; apply steq_refl.
    + eapply steq_trans; [ eassumption | ].
      rewrite (steq_vs a a2 x Ha), (steq_vs a a2 y Ha).
      apply setv_steq; now apply setv_steq.
  (* 原子文は「小ステップ一歩 = 大ステップ一歩」なので、状態の合同性は
     大ステップ側の exec_eq をそのまま通せばよい *)
  - (* show *)
    assert (Hx : exec dummy_env (Sshow e) a b) by (apply E_show; assumption).
    exists b; split; [ | apply steq_refl ].
    eapply exec_atom; [ constructor | ].
    exact (exec_eq dummy_env _ _ _ Hx _ _ Ha (steq_refl b)).
  - (* field assign *)
    assert (Hx : exec dummy_env (Sfassign x f o e) a b)
      by (eapply E_fassign; eassumption).
    exists b; split; [ | apply steq_refl ].
    eapply exec_atom; [ constructor | ].
    exact (exec_eq dummy_env _ _ _ Hx _ _ Ha (steq_refl b)).
  - (* array assign *)
    assert (Hx : exec dummy_env (Saassign x ei o e) a b)
      by (eapply E_aassign; eassumption).
    exists b; split; [ | apply steq_refl ].
    eapply exec_atom; [ constructor | ].
    exact (exec_eq dummy_env _ _ _ Hx _ _ Ha (steq_refl b)).
  - (* array swap *)
    assert (Hx : exec dummy_env (Saswap x e1 y e2) a b)
      by (eapply E_aswap; eassumption).
    exists b; split; [ | apply steq_refl ].
    eapply exec_atom; [ constructor | ].
    exact (exec_eq dummy_env _ _ _ Hx _ _ Ha (steq_refl b)).
  - (* object swap *)
    assert (Hx : exec dummy_env (Soswap x y) a b) by (apply E_oswap; assumption).
    exists b; split; [ | apply steq_refl ].
    eapply exec_atom; [ constructor | ].
    exact (exec_eq dummy_env _ _ _ Hx _ _ Ha (steq_refl b)).
  - (* copy *)
    assert (Hx : exec dummy_env (Scopy x y) a b) by (apply E_copy; assumption).
    exists b; split; [ | apply steq_refl ].
    eapply exec_atom; [ constructor | ].
    exact (exec_eq dummy_env _ _ _ Hx _ _ Ha (steq_refl b)).
  - (* uncopy *)
    assert (Hx : exec dummy_env (Suncopy x y) a b) by (apply E_uncopy; assumption).
    exists b; split; [ | apply steq_refl ].
    eapply exec_atom; [ constructor | ].
    exact (exec_eq dummy_env _ _ _ Hx _ _ Ha (steq_refl b)).
  - exists a2; split; [ apply S_seq_in | assumption ].
  - destruct (IHstep a2 Ha) as [ a2' [ Hs He ] ].
    exists a2'; split; [ now apply S_seq_l | assumption ].
  - exists a2; split; [ apply S_seq_mid | assumption ].
  - destruct (IHstep a2 Ha) as [ a2' [ Hs He ] ].
    exists a2'; split; [ now apply S_seq_r | assumption ].
  - exists a2; split; [ apply S_seq_out | assumption ].
  - exists a2; split; [ | assumption ].
    apply S_if_in_t; rewrite <- (eval_steq e1 a a2 Ha); assumption.
  - exists a2; split; [ | assumption ].
    apply S_if_in_f; rewrite <- (eval_steq e1 a a2 Ha); assumption.
  - destruct (IHstep a2 Ha) as [ a2' [ Hs He ] ].
    exists a2'; split; [ now apply S_if_t | assumption ].
  - destruct (IHstep a2 Ha) as [ a2' [ Hs He ] ].
    exists a2'; split; [ now apply S_if_f | assumption ].
  - exists a2; split; [ | assumption ].
    apply S_if_out_t; rewrite <- (eval_steq e2 a a2 Ha); assumption.
  - exists a2; split; [ | assumption ].
    apply S_if_out_f; rewrite <- (eval_steq e2 a a2 Ha); assumption.
  - exists a2; split; [ | assumption ].
    apply S_lp_in; rewrite <- (eval_steq e1 a a2 Ha); assumption.
  - destruct (IHstep a2 Ha) as [ a2' [ Hs He ] ].
    exists a2'; split; [ now apply S_lp_1 | assumption ].
  - exists a2; split; [ | assumption ].
    apply S_lp_exit; rewrite <- (eval_steq e2 a a2 Ha); assumption.
  - exists a2; split; [ | assumption ].
    apply S_lp_more; rewrite <- (eval_steq e2 a a2 Ha); assumption.
  - destruct (IHstep a2 Ha) as [ a2' [ Hs He ] ].
    exists a2'; split; [ now apply S_lp_2 | assumption ].
  - exists a2; split; [ | assumption ].
    apply S_lp_back; rewrite <- (eval_steq e1 a a2 Ha); assumption.
  - (* 局所ブロックに入る *)
    exists (setv a2 x (eval e1 a2)); split.
    + rewrite (steq_vs a a2 x Ha).   (* 配置が持つ退避値を合わせる *)
      apply S_loc_in; [ assumption | assumption | apply steq_refl ].
    + eapply steq_trans; [ eassumption | ].
      rewrite (eval_steq e1 a a2 Ha); now apply setv_steq.
  - (* 局所ブロックの中 *)
    destruct (IHstep a2 Ha) as [ a2' [ Hs He ] ].
    exists a2'; split; [ now apply S_loc | assumption ].
  - (* 局所ブロックを出る *)
    exists (setv a2 x v); split.
    + apply S_loc_out; [ assumption | | apply steq_refl ].
      rewrite <- (steq_vs a a2 x Ha), <- (eval_steq e2 a a2 Ha); assumption.
    + eapply steq_trans; [ eassumption | now apply setv_steq ].
  - (* オブジェクトブロックに入る *)
    exists (alloc a2 cl x); split.
    + rewrite (steq_hn a a2 Ha).     (* 配置が持つ入口の高さを合わせる *)
      apply S_obj_in; [ | apply steq_refl ].
      rewrite <- (steq_os a a2 x Ha); assumption.
    + eapply steq_trans; [ eassumption | now apply alloc_steq ].
  - (* オブジェクトブロックの中 *)
    destruct (IHstep a2 Ha) as [ a2' [ Hs He ] ].
    exists a2'; split; [ now apply S_obj | assumption ].
  - (* オブジェクトブロックを出る *)
    exists (dealloc a2 x); split.
    + apply S_obj_out.
      * rewrite <- (steq_hn a a2 Ha); assumption.
      * rewrite <- (steq_os a a2 x Ha); assumption.
      * intro f; rewrite <- (steq_hp a a2 h f Ha); [ auto | lia ].
      * rewrite <- (steq_hc a a2 h Ha); [ assumption | lia ].
      * apply steq_refl.
    + eapply steq_trans; [ eassumption | now apply dealloc_steq ].
Qed.

Theorem steps_eq : forall m a m' a',
  steps m a m' a' -> forall a2, a == a2 ->
  exists a2', steps m a2 m' a2' /\ a' == a2'.
Proof.
  intros m a m' a' H; induction H; intros u Hu.
  - exists u; split; [ apply steps_refl | assumption ].
  - destruct (step_eq _ _ _ _ H u Hu) as [ x2 [ Hs He ] ].
    destruct (IHsteps x2 He) as [ y2 [ Hss Hee ] ].
    exists y2; split; [ eapply steps_step; eassumption | assumption ].
Qed.

(** 文脈の中での多ステップ *)
Lemma steps_seql : forall m a m' a' s2,
  steps m a m' a' -> steps (Mseql m s2) a (Mseql m' s2) a'.
Proof.
  intros m a m' a' s2 H; induction H; [ apply steps_refl | ].
  eapply steps_step; [ apply S_seq_l; eassumption | assumption ].
Qed.

Lemma steps_seqr : forall s1 m a m' a',
  steps m a m' a' -> steps (Mseqr s1 m) a (Mseqr s1 m') a'.
Proof.
  intros s1 m a m' a' H; induction H; [ apply steps_refl | ].
  eapply steps_step; [ apply S_seq_r; eassumption | assumption ].
Qed.

Lemma steps_ift : forall e1 m a m' a' s2 e2,
  steps m a m' a' -> steps (Mift e1 m s2 e2) a (Mift e1 m' s2 e2) a'.
Proof.
  intros e1 m a m' a' s2 e2 H; induction H; [ apply steps_refl | ].
  eapply steps_step; [ apply S_if_t; eassumption | assumption ].
Qed.

Lemma steps_iff : forall e1 s1 m a m' a' e2,
  steps m a m' a' -> steps (Miff e1 s1 m e2) a (Miff e1 s1 m' e2) a'.
Proof.
  intros e1 s1 m a m' a' e2 H; induction H; [ apply steps_refl | ].
  eapply steps_step; [ apply S_if_f; eassumption | assumption ].
Qed.

Lemma steps_lp1 : forall e1 m a m' a' s2 e2,
  steps m a m' a' -> steps (Mlp1 e1 m s2 e2) a (Mlp1 e1 m' s2 e2) a'.
Proof.
  intros e1 m a m' a' s2 e2 H; induction H; [ apply steps_refl | ].
  eapply steps_step; [ apply S_lp_1; eassumption | assumption ].
Qed.

Lemma steps_loc : forall x e1 m a m' a' e2 v,
  steps m a m' a' -> steps (Mloc x e1 m e2 v) a (Mloc x e1 m' e2 v) a'.
Proof.
  intros x e1 m a m' a' e2 v H; induction H; [ apply steps_refl | ].
  eapply steps_step; [ apply S_loc; eassumption | assumption ].
Qed.

Lemma steps_obj : forall cl x m a m' a' h,
  steps m a m' a' -> steps (Mobj cl x m h) a (Mobj cl x m' h) a'.
Proof.
  intros cl x m a m' a' h H; induction H; [ apply steps_refl | ].
  eapply steps_step; [ apply S_obj; eassumption | assumption ].
Qed.

Lemma steps_lp2 : forall e1 s1 m a m' a' e2,
  steps m a m' a' -> steps (Mlp2 e1 s1 m e2) a (Mlp2 e1 s1 m' e2) a'.
Proof.
  intros e1 s1 m a m' a' e2 H; induction H; [ apply steps_refl | ].
  eapply steps_step; [ apply S_lp_2; eassumption | assumption ].
Qed.

(* ------------------------------------------------------------------ *)
(** * 大ステップ意味論との対応                                          *)
(* ------------------------------------------------------------------ *)

(** 小ステップ側が規則を持つ断片（制御構造の核）。 *)
Inductive core : stm -> Prop :=
| C_skip : core Sskip
| C_assign : forall x o e, core (Sassign x o e)
| C_swap : forall x y, core (Sswap x y)
| C_show : forall e, core (Sshow e)
| C_fassign : forall x f o e, core (Sfassign x f o e)
| C_aassign : forall x ei o e, core (Saassign x ei o e)
| C_aswap : forall x e1 y e2, core (Saswap x e1 y e2)
| C_oswap : forall x y, core (Soswap x y)
| C_copy : forall x y, core (Scopy x y)
| C_uncopy : forall x y, core (Suncopy x y)
| C_seq : forall s1 s2, core s1 -> core s2 -> core (Sseq s1 s2)
| C_if : forall e1 s1 s2 e2, core s1 -> core s2 -> core (Sif e1 s1 s2 e2)
| C_loop : forall e1 s1 s2 e2, core s1 -> core s2 -> core (Sloop e1 s1 s2 e2)
| C_local : forall x e1 s e2, core s -> core (Slocal x e1 s e2)
| C_obj : forall cl x s, core s -> core (Sobj cl x s).

Ltac not_core := intros; match goal with [ H : core _ |- _ ] => inversion H end.

(** 大ステップで [a] から [b] へ行けるなら、小ステップでも [• s] から [s •] へ
    有限回で到達する（終状態は点ごとに等しい）。 *)
Theorem exec_steps : forall G,
  (forall s a b, exec G s a b -> core s ->
     exists b', steps (Mpre s) a (Mpost s) b' /\ b == b')
  /\ (forall e1 s1 s2 e2 a b, loopx G e1 s1 s2 e2 a b -> core s1 -> core s2 ->
     exists b', steps (Mlp1 e1 (Mpost s1) s2 e2) a
                      (Mpost (Sloop e1 s1 s2 e2)) b' /\ b == b').
Proof.
  intro G; apply exec_loopx_min.
  - (* skip *)
    intros a b Hab _. exists a; split.
    + apply steps_one, S_skip, steq_refl.
    + now apply steq_sym.
  - (* assign *)
    intros x o e a b Hn Hb _.
    exists (setv a x (mapp o (vs a x) (eval e a))); split.
    + apply steps_one, S_assign; [ assumption | apply steq_refl ].
    + assumption.
  - (* field assign *)
    intros x f o e a b l H1 H2 H3 H4 Hc.
    exists b; split; [ | apply steq_refl ].
    apply steps_one; eapply exec_atom with (G := dummy_env) (s := Sfassign x f o e);
      [ constructor | eapply E_fassign; eassumption ].
  - (* array assign *)
    intros x ei o e a b l H1 H2 H3 H4 H5 Hc.
    exists b; split; [ | apply steq_refl ].
    apply steps_one; eapply exec_atom with (G := dummy_env) (s := Saassign x ei o e);
      [ constructor | eapply E_aassign; eassumption ].
  - (* swap *)
    intros x y a b Hb _.
    exists (setv (setv a x (vs a y)) y (vs a x)); split.
    + apply steps_one, S_swap, steq_refl.
    + assumption.
  - (* array swap *)
    intros x e1 y e2 a b l1 l2 H1 H2 H3 H4 H5 H6 H7 Hc.
    exists b; split; [ | apply steq_refl ].
    apply steps_one; eapply exec_atom with (G := dummy_env) (s := Saswap x e1 y e2);
      [ constructor | eapply E_aswap; eassumption ].
  - (* object swap *)
    intros x y a b H1 Hc.
    exists b; split; [ | apply steq_refl ].
    apply steps_one; eapply exec_atom with (G := dummy_env) (s := Soswap x y);
      [ constructor | eapply E_oswap; eassumption ].
  - (* copy *)
    intros x y a b H1 H2 H3 Hc.
    exists b; split; [ | apply steq_refl ].
    apply steps_one; eapply exec_atom with (G := dummy_env) (s := Scopy x y);
      [ constructor | eapply E_copy; eassumption ].
  - (* uncopy *)
    intros x y a b H1 H2 H3 Hc.
    exists b; split; [ | apply steq_refl ].
    apply steps_one; eapply exec_atom with (G := dummy_env) (s := Suncopy x y);
      [ constructor | eapply E_uncopy; eassumption ].
  - (* seq *)
    intros s1 s2 a b c H1 IH1 H2 IH2 Hc; inversion Hc; subst.
    destruct (IH1 ltac:(assumption)) as [ b' [ Hs1 Hb ] ].
    destruct (IH2 ltac:(assumption)) as [ c' [ Hs2 Hc' ] ].
    destruct (steps_eq _ _ _ _ Hs2 b' Hb) as [ c'' [ Hs2' Hc'' ] ].
    exists c''; split.
    + eapply steps_step; [ apply S_seq_in | ].
      eapply steps_trans; [ apply steps_seql; eassumption | ].
      eapply steps_step; [ apply S_seq_mid | ].
      eapply steps_trans; [ apply steps_seqr; eassumption | ].
      apply steps_one, S_seq_out.
    + eauto using steq_trans.
  - (* if true *)
    intros e1 s1 s2 e2 a b H1 H2 IH H3 Hc; inversion Hc; subst.
    destruct (IH ltac:(assumption)) as [ b' [ Hs Hb ] ].
    exists b'; split; [ | assumption ].
    eapply steps_step; [ now apply S_if_in_t | ].
    eapply steps_trans; [ apply steps_ift; eassumption | ].
    apply steps_one, S_if_out_t.
    rewrite <- (eval_steq e2 b b' Hb); assumption.
  - (* if false *)
    intros e1 s1 s2 e2 a b H1 H2 IH H3 Hc; inversion Hc; subst.
    destruct (IH ltac:(assumption)) as [ b' [ Hs Hb ] ].
    exists b'; split; [ | assumption ].
    eapply steps_step; [ now apply S_if_in_f | ].
    eapply steps_trans; [ apply steps_iff; eassumption | ].
    apply steps_one, S_if_out_f.
    rewrite <- (eval_steq e2 b b' Hb); assumption.
  - (* loop *)
    intros e1 s1 s2 e2 a b c H1 H2 IH1 H3 IH2 Hc; inversion Hc; subst.
    destruct (IH1 ltac:(assumption)) as [ b' [ Hs1 Hb ] ].
    destruct (IH2 ltac:(assumption) ltac:(assumption)) as [ c' [ Hs2 Hc' ] ].
    destruct (steps_eq _ _ _ _ Hs2 b' Hb) as [ c'' [ Hs2' Hc'' ] ].
    exists c''; split.
    + eapply steps_step; [ now apply S_lp_in | ].
      eapply steps_trans; [ apply steps_lp1; eassumption | eassumption ].
    + eauto using steq_trans.
  - (* local *)
    intros x e1 s e2 a b c Hn1 Hn2 Hs IH Hx Hc Hcore; inversion Hcore; subst.
    destruct (IH ltac:(assumption)) as [ b' [ Hsteps Hb ] ].
    exists (setv b' x (vs a x)); split.
    + eapply steps_step.
      * apply S_loc_in; [ assumption | assumption | apply steq_refl ].
      * eapply steps_trans; [ apply steps_loc; eassumption | ].
        apply steps_one, S_loc_out; [ assumption | | apply steq_refl ].
        rewrite <- (steq_vs b b' x Hb), <- (eval_steq e2 b b' Hb); assumption.
    + eapply steq_trans; [ eassumption | now apply setv_steq ].
  - (* show *)
    intros e a b H1 Hc.
    exists b; split; [ | apply steq_refl ].
    apply steps_one; eapply exec_atom with (G := dummy_env) (s := Sshow e);
      [ constructor | eapply E_show; eassumption ].
  - (* object block *)
    intros cl x s a b c Hx Hs IH Hbx Hbn Hbz Hbc Hc Hcore; inversion Hcore; subst.
    destruct (IH ltac:(assumption)) as [ b' [ Hsteps Hb ] ].
    exists (dealloc b' x); split.
    + eapply steps_step.
      * apply S_obj_in; [ assumption | apply steq_refl ].
      * assert (Hn : hn b' = S (hn a))
          by (rewrite <- (steq_hn b b' Hb); exact Hbn).
        eapply steps_trans; [ apply steps_obj; eassumption | ].
        apply steps_one, S_obj_out.
        -- exact Hn.
        -- rewrite <- (steq_os b b' x Hb); exact Hbx.
        -- intro f; rewrite <- (steq_hp b b' (hn a) f Hb); [ apply Hbz | lia ].
        -- (* cl は subst で hc b (hn a) に置き換わっている *)
           rewrite <- (steq_hc b b' (hn a) Hb); [ reflexivity | lia ].
        -- apply steq_refl.
    + eapply steq_trans; [ eassumption | now apply dealloc_steq ].
  - (* call *) not_core.
  - (* uncall *) not_core.
  - (* object call *) not_core.
  - (* object uncall *) not_core.
  - (* loop tail: done *)
    intros e1 s1 s2 e2 a b H1 Hab _ _.
    exists a; split.
    + apply steps_one, S_lp_exit; assumption.
    + now apply steq_sym.
  - (* loop tail: step *)
    intros e1 s1 s2 e2 a b c d H1 H2 IH1 H3 H4 IH2 H5 IH3 Hs1 Hs2.
    destruct (IH1 ltac:(assumption)) as [ b' [ Hb Hbe ] ].
    destruct (IH2 ltac:(assumption)) as [ c' [ Hcst Hce ] ].
    destruct (steps_eq _ _ _ _ Hcst b' Hbe) as [ c'' [ Hcst' Hce' ] ].
    assert (Hcc : c == c'') by eauto using steq_trans.
    destruct (IH3 ltac:(assumption) ltac:(assumption)) as [ d' [ Hd Hde ] ].
    destruct (steps_eq _ _ _ _ Hd c'' Hcc) as [ d'' [ Hd' Hde' ] ].
    exists d''; split.
    + eapply steps_step; [ now apply S_lp_more | ].
      eapply steps_trans; [ apply steps_lp2; eassumption | ].
      eapply steps_step.
      * apply S_lp_back. rewrite <- (eval_steq e1 b b' Hbe); assumption.
      * eapply steps_trans; [ apply steps_lp1; eassumption | eassumption ].
    + eauto using steq_trans.
Qed.

(* ------------------------------------------------------------------ *)
(** * 逆方向：多ステップから大ステップを組み立てる                      *)
(* ------------------------------------------------------------------ *)

(** ステップ数を添字にした多ステップ。分解補題で「残りは短い」と言うために要る。
    帰納型ではなく再帰定義にしてあるので、分解は [destruct] だけで済む。 *)
Fixpoint stepsn (n : nat) (m : mstm) (a : state) (m' : mstm) (a' : state) : Prop :=
  (* roopl.v の例で Definition O : id が O を隠すので 0%nat と書く *)
  match n with
  | 0%nat => m = m' /\ a = a'
  | S k => exists m2 a2, step m a m2 a2 /\ stepsn k m2 a2 m' a'
  end.

Lemma stepsn_steps : forall n m a m' a', stepsn n m a m' a' -> steps m a m' a'.
Proof.
  induction n as [ | k IH ]; intros m a m' a' H.
  - destruct H as [ -> -> ]; apply steps_refl.
  - destruct H as [ m2 [ a2 [ HS HR ] ] ].
    eapply steps_step; [ eassumption | now apply IH ].
Qed.

Lemma steps_stepsn : forall m a m' a',
  steps m a m' a' -> exists n, stepsn n m a m' a'.
Proof.
  intros m a m' a' H; induction H.
  - exists 0%nat; split; reflexivity.
  - destruct IHsteps as [ n Hn ]; exists (S n), m2, a2; split; assumption.
Qed.

(** 実行し終えた形からは動けないので、そこから始まる列は空である。 *)
Lemma post_stuck : forall n s a m' a',
  stepsn n (Mpost s) a m' a' -> m' = Mpost s /\ a' = a.
Proof.
  intros n s a m' a' H; destruct n as [ | k ].
  - destruct H as [ -> -> ]; auto.
  - destruct H as [ m2 [ a2 [ HS _ ] ] ].
    exfalso; eapply no_step_from_post; eassumption.
Qed.

(** 列全体でもプログラムは変わらない。 *)
Lemma stepsn_program : forall n m a m' a',
  stepsn n m a m' a' -> erase m = erase m'.
Proof.
  intros; eapply steps_preserve_program, stepsn_steps; eassumption.
Qed.

(* --- 文脈ごとの分解補題 --- *)

Lemma seql_split : forall n m a s1 s2 c,
  stepsn n (Mseql m s2) a (Mpost (Sseq s1 s2)) c ->
  exists n1 b n2, stepsn n1 m a (Mpost s1) b
               /\ stepsn n2 (Mseqr s1 (Mpre s2)) b (Mpost (Sseq s1 s2)) c
               /\ (n1 < n)%nat /\ (n2 < n)%nat.
Proof.
  induction n as [ | k IH ]; intros m a s1 s2 c H.
  - destruct H as [ Hm _ ]; discriminate.
  - destruct H as [ m2 [ a2 [ HS HR ] ] ]; inversion HS; subst.
    + (* 内側のステップ *)
      destruct (IH _ _ _ _ _ HR) as [ q1 [ bb [ q2 [ P1 [ P2 [ L1 L2 ] ] ] ] ] ].
      exists (S q1), bb, q2; repeat split; try assumption; try lia.
      exists m', a2; split; assumption.
    + (* 並びの真ん中へ *)
      assert (Hp := stepsn_program _ _ _ _ _ HR); simpl in Hp;
        injection Hp as Hp1; subst.
      exists 0%nat, a2, k; repeat split; try reflexivity; try assumption; try lia.
Qed.

Lemma seqr_split : forall n s1 m b s2 c,
  stepsn n (Mseqr s1 m) b (Mpost (Sseq s1 s2)) c ->
  exists n2, stepsn n2 m b (Mpost s2) c /\ (n2 < n)%nat.
Proof.
  induction n as [ | k IH ]; intros s1 m b s2 c H.
  - destruct H as [ Hm _ ]; discriminate.
  - destruct H as [ m2 [ a2 [ HS HR ] ] ]; inversion HS; subst.
    + destruct (IH _ _ _ _ _ HR) as [ q2 [ P2 L2 ] ].
      exists (S q2); split; try lia. exists m', a2; split; assumption.
    + destruct (post_stuck _ _ _ _ _ HR) as [ Em Ea ].
      injection Em as Hs2; subst.
      exists 0%nat; split; try lia; split; reflexivity.
Qed.

Lemma ift_split : forall n e1 m a s1 s2 e2 c,
  stepsn n (Mift e1 m s2 e2) a (Mpost (Sif e1 s1 s2 e2)) c ->
  exists n1, stepsn n1 m a (Mpost s1) c /\ eval e2 c <> 0 /\ (n1 < n)%nat.
Proof.
  induction n as [ | k IH ]; intros e1 m a s1 s2 e2 c H.
  - destruct H as [ Hm _ ]; discriminate.
  - destruct H as [ m2 [ a2 [ HS HR ] ] ]; inversion HS; subst.
    + destruct (IH _ _ _ _ _ _ _ HR) as [ q1 [ P1 [ Pe L1 ] ] ].
      exists (S q1); repeat split; try assumption; try lia.
      exists m', a2; split; assumption.
    + destruct (post_stuck _ _ _ _ _ HR) as [ Em Ea ].
      injection Em as Hs1; subst.
      exists 0%nat; repeat split; try reflexivity; try assumption; try lia.
Qed.

Lemma iff_split : forall n e1 s1 m a s2 e2 c,
  stepsn n (Miff e1 s1 m e2) a (Mpost (Sif e1 s1 s2 e2)) c ->
  exists n1, stepsn n1 m a (Mpost s2) c /\ eval e2 c = 0 /\ (n1 < n)%nat.
Proof.
  induction n as [ | k IH ]; intros e1 s1 m a s2 e2 c H.
  - destruct H as [ Hm _ ]; discriminate.
  - destruct H as [ m2 [ a2 [ HS HR ] ] ]; inversion HS; subst.
    + destruct (IH _ _ _ _ _ _ _ HR) as [ q1 [ P1 [ Pe L1 ] ] ].
      exists (S q1); repeat split; try assumption; try lia.
      exists m', a2; split; assumption.
    + destruct (post_stuck _ _ _ _ _ HR) as [ Em Ea ].
      injection Em as Hs2; subst.
      exists 0%nat; repeat split; try reflexivity; try assumption; try lia.
Qed.

Lemma lp1_split : forall n e1 m a s1 s2 e2 c,
  stepsn n (Mlp1 e1 m s2 e2) a (Mpost (Sloop e1 s1 s2 e2)) c ->
  exists n1 b n2, stepsn n1 m a (Mpost s1) b
               /\ stepsn n2 (Mlp1 e1 (Mpost s1) s2 e2) b (Mpost (Sloop e1 s1 s2 e2)) c
               /\ (n1 < n)%nat /\ (n2 <= n)%nat.
Proof.
  induction n as [ | k IH ]; intros e1 m a s1 s2 e2 c H.
  - destruct H as [ Hm _ ]; discriminate.
  - destruct (H) as [ m2 [ a2 [ HS HR ] ] ]; inversion HS; subst.
    + destruct (IH _ _ _ _ _ _ _ HR) as [ q1 [ bb [ q2 [ P1 [ P2 [ L1 L2 ] ] ] ] ] ].
      exists (S q1), bb, q2; repeat split; try assumption; try lia.
      exists m', a2; split; assumption.
    + (* 出口：この時点で m = Mpost s1。s1 の同定は post_stuck から *)
      destruct (post_stuck _ _ _ _ _ HR) as [ Em Ea ]; inversion Em; subst.
      exists 0%nat, a2, (S k); repeat split; try reflexivity; try assumption; try lia.
    + (* 周回継続：s1 の同定は erase から *)
      assert (Hp := stepsn_program _ _ _ _ _ HR); simpl in Hp; inversion Hp; subst.
      exists 0%nat, a2, (S k); repeat split; try reflexivity; try assumption; try lia.
Qed.

Lemma lp2_split : forall n e1 s1 m b s2 e2 c,
  stepsn n (Mlp2 e1 s1 m e2) b (Mpost (Sloop e1 s1 s2 e2)) c ->
  exists n1 d n2, stepsn n1 m b (Mpost s2) d
               /\ eval e1 d = 0
               /\ stepsn n2 (Mlp1 e1 (Mpre s1) s2 e2) d (Mpost (Sloop e1 s1 s2 e2)) c
               /\ (n1 < n)%nat /\ (n2 < n)%nat.
Proof.
  induction n as [ | k IH ]; intros e1 s1 m b s2 e2 c H.
  - destruct H as [ Hm _ ]; discriminate.
  - destruct H as [ m2 [ a2 [ HS HR ] ] ]; inversion HS; subst.
    + destruct (IH _ _ _ _ _ _ _ HR)
        as [ q1 [ dd [ q2 [ P1 [ Pe [ P2 [ L1 L2 ] ] ] ] ] ] ].
      exists (S q1), dd, q2; repeat split; try assumption; try lia.
      exists m', a2; split; assumption.
    + (* S_lp_back: s1 の同定は erase から *)
      assert (Hp := stepsn_program _ _ _ _ _ HR); simpl in Hp; inversion Hp; subst.
      exists 0%nat, a2, k; repeat split; try reflexivity; try assumption; try lia.
Qed.

Lemma loc_split : forall n x e1 m a s e2 v c,
  stepsn n (Mloc x e1 m e2 v) a (Mpost (Slocal x e1 s e2)) c ->
  exists n1 b, stepsn n1 m a (Mpost s) b
            /\ ~ In x (fv e2) /\ vs b x = eval e2 b /\ c == setv b x v
            /\ (n1 < n)%nat.
Proof.
  induction n as [ | k IH ]; intros x e1 m a s e2 v c H.
  - destruct H as [ Hm _ ]; discriminate.
  - destruct H as [ m2 [ a2 [ HS HR ] ] ]; inversion HS; subst.
    + (* 中で一歩進む *)
      destruct (IH _ _ _ _ _ _ _ _ HR)
        as [ q1 [ bb [ P1 [ Hn2 [ Hx [ Hcc L1 ] ] ] ] ] ].
      exists (S q1), bb; split; [ | split; [ | split; [ | split ] ] ].
      * exists m', a2; split; assumption.
      * assumption.
      * assumption.
      * assumption.
      * lia.
    + (* 抜ける *)
      destruct (post_stuck _ _ _ _ _ HR) as [ Em Ec ]; inversion Em; subst.
      exists 0%nat, a; split; [ | split; [ | split; [ | split ] ] ].
      * split; reflexivity.
      * assumption.
      * assumption.
      * assumption.
      * lia.
Qed.

(** オブジェクトブロックの分解。 *)
Lemma obj_split : forall n cl x m a s h c,
  stepsn n (Mobj cl x m h) a (Mpost (Sobj cl x s)) c ->
  exists n1 b, stepsn n1 m a (Mpost s) b
            /\ hn b = S h /\ os b x = Some h
            /\ (forall f, hp b h f = 0) /\ hc b h = cl
            /\ c == dealloc b x
            /\ (n1 < n)%nat.
Proof.
  induction n as [ | k IH ]; intros cl x m a s h c H.
  - destruct H as [ Hm _ ]; discriminate.
  - destruct H as [ m2 [ a2 [ HS HR ] ] ]; inversion HS; subst.
    + (* 中で一歩進む *)
      destruct (IH _ _ _ _ _ _ _ HR)
        as [ q1 [ bb [ P1 [ Hn1 [ Ho [ Hz [ Hcc [ Hd L1 ] ] ] ] ] ] ] ].
      exists (S q1), bb.
      split; [ | split; [ | split; [ | split; [ | split; [ | split ] ] ] ] ].
      * exists m', a2; split; assumption.
      * assumption.
      * assumption.
      * assumption.
      * assumption.
      * assumption.
      * lia.
    + (* 抜ける *)
      destruct (post_stuck _ _ _ _ _ HR) as [ Em Ec ]; inversion Em; subst.
      exists 0%nat, a.
      split; [ | split; [ | split; [ | split; [ | split; [ | split ] ] ] ] ].
      * split; reflexivity.
      * assumption.
      * assumption.
      * assumption.
      * (* cl は subst で hc a h に置き換わっていることがある *)
        (assumption || reflexivity).
      * assumption.
      * lia.
Qed.

(* --- 本体：多ステップから大ステップを組み立てる --- *)

Lemma steps_exec_aux : forall G n,
  (forall s a b, stepsn n (Mpre s) a (Mpost s) b -> core s -> exec G s a b)
  /\ (forall e1 s1 s2 e2 b c,
        stepsn n (Mlp1 e1 (Mpost s1) s2 e2) b (Mpost (Sloop e1 s1 s2 e2)) c ->
        core s1 -> core s2 -> loopx G e1 s1 s2 e2 b c).
Proof.
  intro G.
  induction n as [ n IH ] using (well_founded_induction lt_wf); split.
  - (* 文 *)
    intros s a b H Hc; destruct Hc.
    + (* skip *)
      destruct n as [ | k ]; [ destruct H as [ Hm _ ]; discriminate | ].
      destruct H as [ m2 [ a2 [ HS HR ] ] ]; inversion HS; subst.
      destruct (post_stuck _ _ _ _ _ HR) as [ _ Eb ]; subst.
      apply E_skip; assumption.
    + (* assign *)
      destruct n as [ | k ]; [ destruct H as [ Hm _ ]; discriminate | ].
      destruct H as [ m2 [ a2 [ HS HR ] ] ]; inversion HS; subst.
      destruct (post_stuck _ _ _ _ _ HR) as [ _ Eb ]; subst.
      apply E_assign; assumption.
    + (* swap *)
      destruct n as [ | k ]; [ destruct H as [ Hm _ ]; discriminate | ].
      destruct H as [ m2 [ a2 [ HS HR ] ] ]; inversion HS; subst.
      destruct (post_stuck _ _ _ _ _ HR) as [ _ Eb ]; subst.
      apply E_swap; assumption.
    + (* show：原子文は一歩で終わり、その一歩が大ステップ一歩そのもの *)
      destruct n as [ | k ]; [ destruct H as [ Hm _ ]; discriminate | ].
      destruct H as [ m2 [ a2 [ HS HR ] ] ]; inversion HS; subst;
      destruct (post_stuck _ _ _ _ _ HR) as [ _ Eb ]; subst;
      eapply atom_exec; eassumption.
    + (* field assign：原子文は一歩で終わり、その一歩が大ステップ一歩そのもの *)
      destruct n as [ | k ]; [ destruct H as [ Hm _ ]; discriminate | ].
      destruct H as [ m2 [ a2 [ HS HR ] ] ]; inversion HS; subst;
      destruct (post_stuck _ _ _ _ _ HR) as [ _ Eb ]; subst;
      eapply atom_exec; eassumption.
    + (* array assign：原子文は一歩で終わり、その一歩が大ステップ一歩そのもの *)
      destruct n as [ | k ]; [ destruct H as [ Hm _ ]; discriminate | ].
      destruct H as [ m2 [ a2 [ HS HR ] ] ]; inversion HS; subst;
      destruct (post_stuck _ _ _ _ _ HR) as [ _ Eb ]; subst;
      eapply atom_exec; eassumption.
    + (* array swap：原子文は一歩で終わり、その一歩が大ステップ一歩そのもの *)
      destruct n as [ | k ]; [ destruct H as [ Hm _ ]; discriminate | ].
      destruct H as [ m2 [ a2 [ HS HR ] ] ]; inversion HS; subst;
      destruct (post_stuck _ _ _ _ _ HR) as [ _ Eb ]; subst;
      eapply atom_exec; eassumption.
    + (* object swap：原子文は一歩で終わり、その一歩が大ステップ一歩そのもの *)
      destruct n as [ | k ]; [ destruct H as [ Hm _ ]; discriminate | ].
      destruct H as [ m2 [ a2 [ HS HR ] ] ]; inversion HS; subst;
      destruct (post_stuck _ _ _ _ _ HR) as [ _ Eb ]; subst;
      eapply atom_exec; eassumption.
    + (* copy：原子文は一歩で終わり、その一歩が大ステップ一歩そのもの *)
      destruct n as [ | k ]; [ destruct H as [ Hm _ ]; discriminate | ].
      destruct H as [ m2 [ a2 [ HS HR ] ] ]; inversion HS; subst;
      destruct (post_stuck _ _ _ _ _ HR) as [ _ Eb ]; subst;
      eapply atom_exec; eassumption.
    + (* uncopy：原子文は一歩で終わり、その一歩が大ステップ一歩そのもの *)
      destruct n as [ | k ]; [ destruct H as [ Hm _ ]; discriminate | ].
      destruct H as [ m2 [ a2 [ HS HR ] ] ]; inversion HS; subst;
      destruct (post_stuck _ _ _ _ _ HR) as [ _ Eb ]; subst;
      eapply atom_exec; eassumption.
    + (* seq *)
      destruct n as [ | k ]; [ destruct H as [ Hm _ ]; discriminate | ].
      destruct H as [ m2 [ a2 [ HS HR ] ] ]; inversion HS; subst.
      destruct (seql_split _ _ _ _ _ _ HR)
        as [ q1 [ bb [ q2 [ P1 [ P2 [ L1 L2 ] ] ] ] ] ].
      destruct (seqr_split _ _ _ _ _ _ P2) as [ q3 [ P3 L3 ] ].
      eapply E_seq.
      * apply (proj1 (IH q1 ltac:(lia))); eassumption.
      * apply (proj1 (IH q3 ltac:(lia))); eassumption.
    + (* if *)
      destruct n as [ | k ]; [ destruct H as [ Hm _ ]; discriminate | ].
      destruct H as [ m2 [ a2 [ HS HR ] ] ]; inversion HS; subst.
      * (* then 枝 *)
        destruct (ift_split _ _ _ _ _ _ _ _ HR) as [ q1 [ P1 [ Pe L1 ] ] ].
        apply E_if_t; [ assumption | | assumption ].
        apply (proj1 (IH q1 ltac:(lia))); eassumption.
      * (* else 枝 *)
        destruct (iff_split _ _ _ _ _ _ _ _ HR) as [ q1 [ P1 [ Pe L1 ] ] ].
        apply E_if_f; [ assumption | | assumption ].
        apply (proj1 (IH q1 ltac:(lia))); eassumption.
    + (* loop *)
      destruct n as [ | k ]; [ destruct H as [ Hm _ ]; discriminate | ].
      destruct H as [ m2 [ a2 [ HS HR ] ] ]; inversion HS; subst.
      destruct (lp1_split _ _ _ _ _ _ _ _ HR)
        as [ q1 [ bb [ q2 [ P1 [ P2 [ L1 L2 ] ] ] ] ] ].
      eapply E_loop; [ assumption | | ].
      * apply (proj1 (IH q1 ltac:(lia))); eassumption.
      * apply (proj2 (IH q2 ltac:(lia))); eassumption.
    + (* local *)
      destruct n as [ | k ]; [ destruct H as [ Hm _ ]; discriminate | ].
      destruct H as [ m2 [ a2 [ HS HR ] ] ]; inversion HS; subst.
      destruct (loc_split _ _ _ _ _ _ _ _ _ HR)
        as [ q1 [ bb [ P1 [ Hn2 [ Hx [ Hcc L1 ] ] ] ] ] ].
      eapply E_local; try eassumption.
      assert (Hb : (q1 < S k)%nat) by lia.
      eapply exec_eq with (G := G);
        [ apply (proj1 (IH q1 Hb)); eassumption
        | eassumption | apply steq_refl ].
    + (* object block *)
      destruct n as [ | k ]; [ destruct H as [ Hm _ ]; discriminate | ].
      destruct H as [ m2 [ a2 [ HS HR ] ] ]; inversion HS; subst.
      destruct (obj_split _ _ _ _ _ _ _ _ HR)
        as [ q1 [ bb [ P1 [ Hn1 [ Ho [ Hz [ Hcc [ Hd L1 ] ] ] ] ] ] ] ].
      (* 小ステップ側は入口の高さ [hn a] を配置に退避しているので、
         大ステップ側が要求する [hn b = S (hn a)] がそのまま得られる *)
      eapply E_obj with (b := bb); try eassumption.
      assert (Hb : (q1 < S k)%nat) by lia.
      eapply exec_eq with (G := G);
        [ apply (proj1 (IH q1 Hb)); eassumption
        | eassumption | apply steq_refl ].
  - (* ループの残り *)
    intros e1 s1 s2 e2 b c H Hs1 Hs2.
    destruct n as [ | k ]; [ destruct H as [ Hm _ ]; discriminate | ].
    destruct H as [ m2 [ a2 [ HS HR ] ] ]; inversion HS; subst.
    + (* 内側のステップは Mpost からは起こらない *)
      exfalso; eapply no_step_from_post; eassumption.
    + (* 出口 *)
      destruct (post_stuck _ _ _ _ _ HR) as [ _ Ec ]; subst.
      apply L_done; [ assumption | apply steq_refl ].
    + (* 周回継続 *)
      destruct (lp2_split _ _ _ _ _ _ _ _ HR)
        as [ q1 [ dd [ q2 [ P1 [ Pe [ P2 [ L1 L2 ] ] ] ] ] ] ].
      destruct (lp1_split _ _ _ _ _ _ _ _ P2)
        as [ q3 [ ee [ q4 [ Q1 [ Q2 [ M1 M2 ] ] ] ] ] ].
      (* 先に exec を解いて中間状態のメタ変数を決めてから表明を出す *)
      eapply L_step.
      * assumption.
      * apply (proj1 (IH q1 ltac:(lia))); eassumption.
      * eassumption.
      * apply (proj1 (IH q3 ltac:(lia))); eassumption.
      * apply (proj2 (IH q4 ltac:(lia))); eassumption.
Qed.

(** **小ステップから大ステップへ**。[exec_steps] と合わせて、核の断片では
    二つの意味論が同じ関係を定めていることになる。 *)
Theorem steps_exec : forall G s a b,
  core s -> steps (Mpre s) a (Mpost s) b -> exec G s a b.
Proof.
  intros G s a b Hc H.
  destruct (steps_stepsn _ _ _ _ H) as [ n Hn ].
  eapply (proj1 (steps_exec_aux G n)); eassumption.
Qed.

(** 二つの意味論の同値（終状態は点ごとの等しさまで）。 *)
Theorem exec_iff_steps : forall G s a b,
  core s ->
  (exec G s a b <-> exists b', steps (Mpre s) a (Mpost s) b' /\ b == b').
Proof.
  intros G s a b Hc; split; intro H.
  - now apply (proj1 (exec_steps G)).
  - destruct H as [ b' [ Hs Hb ] ].
    eapply exec_eq; [ eapply steps_exec; eassumption | apply steq_refl | ].
    now apply steq_sym.
Qed.

(* ------------------------------------------------------------------ *)
(** * 空虚でないことの確認                                              *)
(* ------------------------------------------------------------------ *)

Definition X : id := 0%nat.
Definition Y : id := 1%nat.
Definition zero0 : state :=
  St (fun _ => 0) (fun _ => None) 0%nat (fun _ _ => 0) (fun _ => 0%nat).

(** v0 += 3 ; v0 <=> v1 を最後まで進める。 *)
Definition prog0 : stm :=
  Sseq (Sassign X MAdd (Cst 3)) (Sswap X Y).

Example ex_small_run :
  exists b, steps (Mpre prog0) zero0 (Mpost prog0) b
            /\ vs b X = 0 /\ vs b Y = 3.
Proof.
  eexists. split.
  - eapply steps_step; [ apply S_seq_in | ].
    eapply steps_step;
      [ apply S_seq_l; apply S_assign; [ simpl; tauto | apply steq_refl ] | ].
    eapply steps_step; [ apply S_seq_mid | ].
    eapply steps_step; [ apply S_seq_r; apply S_swap; apply steq_refl | ].
    eapply steps_step; [ apply S_seq_out | ].
    apply steps_refl.
  - split; reflexivity.
Qed.

(** ループも動く: from v0 = 0 do skip loop v0 += 1 until v0 = 2 *)
Definition prog1 : stm :=
  Sloop (Bop Oeq (Var X) (Cst 0)) Sskip (Sassign X MAdd (Cst 1))
        (Bop Oeq (Var X) (Cst 2)).

Example ex_small_loop :
  exists b, steps (Mpre prog1) zero0 (Mpost prog1) b /\ vs b X = 2.
Proof.
  eexists. split.
  - eapply steps_step; [ apply S_lp_in; simpl; discriminate | ].
    eapply steps_step; [ apply S_lp_1; apply S_skip, steq_refl | ].
    eapply steps_step; [ apply S_lp_more; reflexivity | ].
    eapply steps_step;
      [ apply S_lp_2; apply S_assign; [ simpl; tauto | apply steq_refl ] | ].
    eapply steps_step; [ apply S_lp_back; reflexivity | ].
    eapply steps_step; [ apply S_lp_1; apply S_skip, steq_refl | ].
    eapply steps_step; [ apply S_lp_more; reflexivity | ].
    eapply steps_step;
      [ apply S_lp_2; apply S_assign; [ simpl; tauto | apply steq_refl ] | ].
    eapply steps_step; [ apply S_lp_back; reflexivity | ].
    eapply steps_step; [ apply S_lp_1; apply S_skip, steq_refl | ].
    eapply steps_step; [ apply S_lp_exit; simpl; discriminate | ].
    apply steps_refl.
  - reflexivity.
Qed.

(** 原子文も動く: v0 += 3 ; show(v0) *)
Example ex_small_atomic :
  exists b, steps (Mpre (Sseq (Sassign X MAdd (Cst 3)) (Sshow (Var X))))
                  zero0
                  (Mpost (Sseq (Sassign X MAdd (Cst 3)) (Sshow (Var X)))) b
            /\ vs b X = 3.
Proof.
  eexists. split.
  - eapply steps_step; [ apply S_seq_in | ].
    eapply steps_step;
      [ apply S_seq_l; apply S_assign; [ simpl; tauto | apply steq_refl ] | ].
    eapply steps_step; [ apply S_seq_mid | ].
    eapply steps_step; [ apply S_seq_r; apply S_show; apply steq_refl | ].
    eapply steps_step; [ apply S_seq_out | ].
    apply steps_refl.
  - reflexivity.
Qed.

(** 局所ブロックも動く: local t = 3  v0 += t  delocal t = 3 *)
Definition T0 : id := 2%nat.
Definition prog2 : stm :=
  Slocal T0 (Cst 3) (Sassign X MAdd (Var T0)) (Cst 3).

Example ex_small_local :
  exists b, steps (Mpre prog2) zero0 (Mpost prog2) b
            /\ vs b X = 3 /\ vs b T0 = 0.
Proof.
  eexists. split.
  - eapply steps_step.
    + apply S_loc_in; [ simpl; tauto | simpl; tauto | apply steq_refl ].
    + eapply steps_step.
      * apply S_loc; apply S_assign;
          [ unfold X, T0; simpl; intuition discriminate | apply steq_refl ].
      * eapply steps_step; [ | apply steps_refl ].
        apply S_loc_out; [ simpl; tauto | reflexivity | apply steq_refl ].
  - split; reflexivity.
Qed.

(** オブジェクトブロック：new C p ... delete C p。
    体でフィールドに 5 を足して引き戻すので、出口の零消去条件を満たす。 *)
Definition P : id := 3%nat.
Definition CL : cid := 7%nat.
Definition F0 : field := 0%nat.

Definition prog3 : stm :=
  Sobj CL P (Sseq (Sfassign P F0 MAdd (Cst 5))
                  (Sfassign P F0 MSub (Cst 5))).

Example ex_small_obj :
  exists b, steps (Mpre prog3) zero0 (Mpost prog3) b
            /\ hn b = 0%nat /\ os b P = None.
Proof.
  eexists. split.
  - eapply steps_step; [ apply S_obj_in; [ reflexivity | apply steq_refl ] | ].
    eapply steps_step; [ apply S_obj, S_seq_in | ].
    eapply steps_step.
    { apply S_obj, S_seq_l, S_fassign with (l := 0%nat);
        [ reflexivity | simpl; lia | apply steq_refl | reflexivity ]. }
    eapply steps_step; [ apply S_obj, S_seq_mid | ].
    eapply steps_step.
    { apply S_obj, S_seq_r, S_fassign with (l := 0%nat);
        [ reflexivity | simpl; lia | apply steq_refl | reflexivity ]. }
    eapply steps_step; [ apply S_obj, S_seq_out | ].
    eapply steps_step; [ | apply steps_refl ].
    apply S_obj_out.
    + reflexivity.
    + reflexivity.
    + intro f; simpl; destruct f; reflexivity.
    + reflexivity.
    + apply steq_refl.
  - split; reflexivity.
Qed.

(* ------------------------------------------------------------------ *)
(** * 公理の確認                                                        *)
(* ------------------------------------------------------------------ *)

(** 系：大ステップの可逆性が小ステップ側の到達可能性に移る。
    同じプログラムを違う初期状態から走らせて同じ終状態に至ったなら、
    初期状態は（点ごとに）同じだったことになる。 *)
Corollary steps_inj_from_exec : forall G s a1 a2 b1 b2,
  core s ->
  exec G s a1 b1 -> exec G s a2 b2 -> b1 == b2 -> a1 == a2.
Proof.
  intros G s a1 a2 b1 b2 Hc H1 H2 Hb.
  eapply exec_inj; [ eassumption | ].
  eapply exec_eq; [ eassumption | apply steq_refl | now apply steq_sym ].
Qed.

Print Assumptions step_det.
Print Assumptions step_inj.
Print Assumptions step_preserves_program.
Print Assumptions exec_steps.
Print Assumptions steps_exec.
Print Assumptions exec_iff_steps.
