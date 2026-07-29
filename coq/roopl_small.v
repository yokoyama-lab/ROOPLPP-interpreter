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

Require Import ZArith List Bool Arith Lia.
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
| Mlp2  (e1 : exp) (s1 : stm) (m : mstm) (e2 : exp).       (**r ループの s2 の中 *)

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
  end.



(* ------------------------------------------------------------------ *)
(** * 小ステップ関係                                                    *)
(* ------------------------------------------------------------------ *)

(** 配置は (トークン付きの文, 状態)。対ではなく 4 引数の関係にしてあるのは、
    対の添字だと induction/inversion が構成子の形を割り出せないため。 *)
Inductive step : mstm -> state -> mstm -> state -> Prop :=
(* 原子的な文 *)
| S_skip : forall a,
    step (Mpre Sskip) a (Mpost Sskip) a
| S_assign : forall x o e a b,
    ~ In x (fv e) ->
    b == setv a x (mapp o (vs a x) (eval e a)) ->
    step (Mpre (Sassign x o e)) a (Mpost (Sassign x o e)) b
| S_swap : forall x y a b,
    b == setv (setv a x (vs a y)) y (vs a x) ->
    step (Mpre (Sswap x y)) a (Mpost (Sswap x y)) b

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
    step (Mlp2 e1 s1 (Mpost s2) e2) a (Mlp1 e1 (Mpre s1) s2 e2) a.

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
  intros m a m1 a1 m2 a2 H1; revert m2 a2.
  induction H1; intros m2 a2 H2; inversion H2; subst;
    try impossible_step;
    try congruence;
    try (split; [ reflexivity | eauto using steq_trans, steq_sym ]);
    try (match goal with
         | [ IH : forall m0 a0, step ?mm ?aa m0 a0 -> _,
             HS : step ?mm ?aa ?mm' ?aa' |- _ ] =>
             destruct (IH mm' aa' HS) as [ Em Ea ]; subst;
             split; [ reflexivity | assumption ]
         end).
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
  intros m1 a1 m2 a2 m a H1; revert m2 a2.
  induction H1; intros m2 a2 H2; inversion H2; subst;
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
    eapply steps_step; [ apply S_lp_1; apply S_skip | ].
    eapply steps_step; [ apply S_lp_more; reflexivity | ].
    eapply steps_step;
      [ apply S_lp_2; apply S_assign; [ simpl; tauto | apply steq_refl ] | ].
    eapply steps_step; [ apply S_lp_back; reflexivity | ].
    eapply steps_step; [ apply S_lp_1; apply S_skip | ].
    eapply steps_step; [ apply S_lp_more; reflexivity | ].
    eapply steps_step;
      [ apply S_lp_2; apply S_assign; [ simpl; tauto | apply steq_refl ] | ].
    eapply steps_step; [ apply S_lp_back; reflexivity | ].
    eapply steps_step; [ apply S_lp_1; apply S_skip | ].
    eapply steps_step; [ apply S_lp_exit; simpl; discriminate | ].
    apply steps_refl.
  - reflexivity.
Qed.

(* ------------------------------------------------------------------ *)
(** * 公理の確認                                                        *)
(* ------------------------------------------------------------------ *)

Print Assumptions step_det.
Print Assumptions step_inj.
Print Assumptions step_preserves_program.
