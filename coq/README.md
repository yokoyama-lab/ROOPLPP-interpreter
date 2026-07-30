# ROOPL++ の可逆性の機械検証（Rocq）

このインタプリタが実装している **ROOPL++**（Cservenka 2018）について，可逆性を Rocq で
機械検証したもの。`roopl.v`（大ステップ意味論，1799 行）と `roopl_small.v`（小ステップ意味論，2032 行）。
外部ライブラリ依存なし。

ROOPL / ROOPL++ には紙の操作的意味論と型システム（Haulund 2017, Cservenka 2018）があり，
「文の反転で well-typedness が保存される」ことなども証明されているが，**証明支援系による
機械検証はこれまで無かった**。姉妹プロジェクト PyJanus の vJanus（`PyJanus/coq/`）が Janus に
対して行っている検証の，ROOPL++ 版にあたる。

## ビルド

```
make          # rocq c -Q . ROOPL roopl.v
make check    # rocq check（独立した検査器で .vo を再検査）
```

Rocq 9.1.1 で確認。

## 証明した定理

| 定理 | 主張 |
|---|---|
| `invert_invert` | `invert (invert s) = s` — 反転は対合 |
| `exec_invert` | `exec s a b → exec (invert s) b a` — 反転プログラムは逆向きに走る |
| `exec_iff` | `exec s a b ↔ exec (invert s) b a` |
| `exec_det` | 前方決定性 |
| **`exec_inj`** | `exec s a1 b → exec s a2 b → a1 == a2` — **可逆性**：最終状態が初期状態を一意に定める（プログラムは状態上の単射部分関数を表す） |
| `exec_round_trip` | 実行してから逆を実行すると元に戻る |
| **`run_sound`** | `run fuel G s a = Some b → exec s a b` — **抽出した実行可能インタプリタが意味論に対して健全** |
| `run_injective` / `run_invert` | その系（`run` の結果は単射・逆は逆向きに走る） |
| **`wt_invert`** | `wt E S s → wt E S (invert s)` — **反転で型付けが保存される**（Haulund 2017 の定理） |
| `invert_for_up` / `invert_for_down` | `for` の反転はちょうど昇順と降順を入れ替える |
| `for_up_reversible` / `for_down_reversible` | **`for` は可逆** |
| `invert_rev_switch` | `switch` の反転は入口と出口を入れ替えた `switch` |
| `rev_switch_reversible` | **`switch` は可逆** |

補助定理として `exec_loopx_eq`（実行は状態の点ごとの等しさを保つ），`loopx_exit`（ループ末尾は
出口表明を満たす）を経由する。`exec_invert` / `exec_det` はいずれも `exec` と補助関係 `loopx`
に対する**相互帰納法**（`Combined Scheme`）で証明している。

**公理はゼロ**。`Print Assumptions` はすべて `Closed under the global context`
を返す（ビルド時に表示される）。

## `for` と `switch`（インタプリタの追加構文）

このインタプリタは ROOPL++ に `for` と `switch` を足している。形式化では
**原始構文ではなく既存構文への糖衣**として与える。糖衣として書けること自体が
「`for` は局所ブロック＋二重ガードのループ、`switch` は二重ガードの条件分岐の
入れ子にすぎない」という主張で、可逆性は `exec_invert` からそのまま従う。

```coq
Definition for_up (x : id) (e1 e2 : exp) (s : stm) : stm :=
  Slocal x e1
    (Sloop (Bop Oeq (Var x) e1) s (Sassign x MAdd (Cst 1)) (Bop Oeq (Var x) e2))
    e2.
(* for_down は MSub 版 *)

Fixpoint rev_switch (x : id) (cs : list (Z * stm * Z)) (d : stm) (y : id) : stm :=
  match cs with
  | [] => d
  | (v, s, w) :: tl =>
      Sif (Bop Oeq (Var x) (Cst v)) s (rev_switch x tl d y) (Bop Oeq (Var y) (Cst w))
  end.
```

昇順と降順を別の糖衣にしてあるのは、**反転がちょうど互いを写す**からで、
`invert_for_up` は `reflexivity` で閉じる。インタプリタは実行時に両端の大小で
向きを選ぶので、表層の `for` はどちらかに対応する。

### 実装との食い違い（差分テストで固定した）

糖衣は `make extract` で OCaml へ取り出してあり、`test/extracted_test.ml` が
**実装の `For`/`Switch` と糖衣を突き合わせる**。正常な場合は一致するが、
**実装が検査を省いている 2 か所**で食い違い、それをテストとして固定してある。

| 場面 | 実装 | 意味論（糖衣） |
|---|---|---|
| 出口の値が枝どうしで重複する `switch` | 通る | 落ちる |
| 体がループ変数を書き換える `for` | 完走する | 停止しない |

どちらも**落ちる／停止しない側が正しい**。前者は出口の値が枝を識別できず、
逆向きの実行が枝を選び直せない。後者は `lib/eval.ml` がループ変数の不変性を
**最初の 1 周でしか検査していない**ため（2 周目以降の書き換えを見逃し、しかも
次の周回で値を上書きしてしまう）。範囲式 `e1`/`e2` が体で変わらないことも
同じく検査されていない。

型システムは整数・オブジェクト・配列の 3 種（クラス名は区別しない）。`construct` は
オブジェクトと配列のどちらのセル列も確保できる。`wt_invert` は「反転は型を一切変えない
（更新演算子を逆にし，条件分岐とループの 2 つのガードを入れ替え，並びを逆順にし，
call を uncall にするだけ）」ことの帰結。

## 小ステップ意味論（`roopl_small.v`）

大ステップの `exec` は「文を実行すると状態がこう変わる」を一息で述べる。
`roopl_small.v` は実行の途中も配置として表す**小ステップ意味論**で、
制御トークン（R-CORE の `•` に相当）を文の中に埋め込む方式を採る。

```coq
Inductive mstm :=
| Mpre (s : stm)   (* • s : これから s を実行する *)
| Mpost (s : stm)  (* s • : s を実行し終えた *)
| Mseql | Mseqr | Mift | Miff | Mlp1 | Mlp2   (* 入れ子の位置 *)
| Mloc (x) (e1) (m) (e2) (v)                 (* 局所ブロックの中（退避値つき）*)
| Mobj (cl) (x) (m) (h)                      (* オブジェクトブロックの中（入口の高さつき）*)
| Mcall (s) (m)                              (* 手続き呼出しの中 *)
| Mocall (l) (cl) (h) (s) (m)                (* メソッド呼出しの中（受け手つき）*)
```

メソッド呼出しは本体を環境から引くので、`roopl_small.v` の小ステップ部分は
節（`Section`）でひとつの環境 `G` のもとにまとめてある（節を閉じると
`step G …` になる）。

| 定理 | 主張 |
|---|---|
| `step_det` | 前方決定性：ある配置から進める先はたかだか一つ |
| **`step_inj`** | **後方決定性（＝小ステップの可逆性）**：ある配置へ入ってこられる配置はたかだか一つ |
| `step_preserves_program` | ステップはトークンを動かすだけでプログラム本体を変えない（`erase` が不変） |
| `assign_inj` / `swap_inj` | 原子的な文の局所可逆性（結果から入口の状態が一意に決まる） |
| `atom_exec` / `exec_atom` | 原子文では「小ステップ一歩 ＝ 大ステップ一歩」 |
| `atom_inj` | その帰結：原子文の局所可逆性は大ステップの `exec_inj` から出る |
| `loc_in_inj` / `loc_out_inj` | 局所ブロックの出入りの単射性（退避値も一意に決まる） |
| `obj_in_inj` / `obj_out_inj` | オブジェクトブロックの出入りの単射性（零消去された対象が復元できる） |
| `core_rename` / `core_invert` | 実引数への束縛と反転は核の外へ出ない |
| `dispatch_core` | 動的束縛で選ばれた本体も核に収まる |
| `step_eq` / `steps_eq` | ステップは状態の `==` を尊重する |
| `exec_steps` | 大ステップ → 小ステップ：`exec s a b` なら `(•s, a) →* (s•, b')` で `b == b'` |
| `steps_exec` | 小ステップ → 大ステップ：`(•s, a) →* (s•, b)` なら `exec s a b` |
| **`exec_iff_steps`** | **二つの意味論の同値**（`core s` と `core_env` のとき） |

**`step_inj` の証明の中に可逆言語の要点が現れる。** 衝突しそうな場面が
すべて**言語の表明**で分かれる：

- `Mlp1 e1 (Mpre s1) s2 e2` へは「ループに入ってきた」「前の周回から戻ってきた」の
  二通りで来られるが、**入口表明 `e1` の真偽**が両者を排他にする
- `Mpost (Sif ...)` へは then 枝と else 枝の二通りだが、**出口表明 `e2`** が分ける

つまり ROOPL++ が二重ガードを持つという設計が、そのまま後方決定性の証明になっている。
`assign_inj` では可逆代入の副条件（更新する変数が右辺に現れない）が同じ役割を果たす。

対象（述語 `core`）は **原子文すべてと制御構造**：skip・可逆代入・整数 swap・
**`show`/`print`**・**フィールド代入 `x.f op= e`**・**配列代入 `x[e] op= e`**・
**配列要素の入れ替え `x[e1] <=> y[e2]`**・**オブジェクト変数の swap**・
**`copy`/`uncopy`**・並び・条件分岐・ループ。
**局所ブロック `local`**・**オブジェクトブロック**・**メソッド呼出し**も
対応済みで、`core` は言語の全構文をカバーする。まず局所ブロックは、配置に文脈構成子

```coq
| Mloc (x : id) (e1 : exp) (m : mstm) (e2 : exp) (v : Z)
```

を足し、**外側の `x` の値 `v` を配置に退避**する。出るときの表明
`vs a x = eval e2 a` が、退避値と合わせて入口の状態を一意に決める
（`loc_out_inj`）。

**オブジェクトブロック `construct C x … destruct C x`** も対応済み。配置は

```coq
| Mobj (cl : cid) (x : id) (m : mstm) (h : nat)
```

で、**入口でのヒープの高さ `h`（＝確保した対象の位置）を退避**する。局所ブロックが
外側の値を退避するのと同じ役割で、これが無いと「体の中で確保したまま解放しなかった
対象」を出口で見逃してしまい、大ステップ意味論の `E_obj`（`hn b = S (hn a)`）と
食い違う。出口では **対象の全フィールドが零消去されている**ことと **クラスが `cl` で
ある**ことを表明として要求し、この 2 つが `obj_out_inj`（＝解放後の状態から解放前を
一意に復元できる）を成り立たせる。ヒープをスタックとして扱う（`construct`／`destruct`
がブロック構造）設計なので、確保位置は状態の関数として決まる。

**メソッド呼出し**（手続き `call`／`uncall` と動的束縛つきの
`x::m(...)`／`uncall x::m(...)`）も対応済み。配置は

```coq
| Mcall  (s : stm) (m : mstm)                              (* 手続き *)
| Mocall (l : loc) (cl : cid) (h : nat) (s : stm) (m : mstm)  (* メソッド *)
```

で、**呼出し文そのものを配置が覚えている**（これで出口の戻り先が一意に決まり、
後方決定性が保たれる）。本体は環境から引いた `rename (mk_ren ps args) body`
（`uncall` はその `invert`）で、**入口と出口で同じ本体が引かれる**ことが
`procs G` が関数であることと `dispatch_det`（動的束縛の一意性）から従う。

動的束縛つきの呼出しでは、さらに **受け手の位置 `l`・その動的クラス `cl`・
入口でのヒープの高さ `h` を配置に退避**し、出口で `os a x = Some l`・
`hc a l = cl`・`hn a = h` を表明として確かめる。これは大ステップの `E_ocall`
が要求する「受け手は呼出し中に動かない・ヒープの高さが釣り合う」に対応する。

呼出し先の本体が核に収まっていることは環境の側の条件

```coq
Definition core_env : Prop :=
  (forall m ps body, procs G m = Some (MDecl ps body) -> core body)
  /\ (forall c p ms m ps body,
        classes G c = Some (CDecl p ms) -> ms m = Some (MDecl ps body) -> core body).
```

として述べ、対応定理（`exec_steps` / `steps_exec` / `exec_iff_steps`）の仮定に
置く。`core_env` が空虚でないことは例 `penv_core` で確かめている。

原子文を足すのが安く済んだのは、**「小ステップ一歩 ＝ 大ステップ一歩」**という
橋（`atom_exec` / `exec_atom`）を通したから。これで文ごとの局所可逆性補題を
書かずに、大ステップ側で証明済みの `exec_inj`（可逆性）と `exec_eq`（合同性）を
そのまま使える。実際 `step_inj` と `step_eq` の原子文ケースはこの橋だけで閉じる。

**大ステップとの対応は両方向とも証明済み**（`exec_iff_steps`）。

- `exec_steps`（大 → 小）は `exec` と `loopx` の相互帰納。ループの場合は
  「s1 を走る → 出口表明で判定 → s2 を走る → 入口表明で判定 → 次の周回」という
  小ステップの並びを組み立てる。
- `steps_exec`（小 → 大）はステップ数に関する強帰納法。多ステップを
  **再帰定義** `stepsn`（帰納型ではなく `Fixpoint`）にしてあるので、分解が
  `destruct` だけで済む。文脈ごとの分解補題 `seql_split` / `seqr_split` /
  `ift_split` / `iff_split` / `lp1_split` / `lp2_split` / `loc_split` /
  `obj_split` / `call_split` / `uncall_split` / `ocall_split` /
  `ouncall_split` が「列は文脈の中で進み、
  出るときに表明を満たす」ことを取り出し、`stepsn_program`（プログラムが列全体で
  不変）が「どの部分文だったか」の同定に効く。

系として `steps_inj_from_exec`（同じ終状態に至る 2 つの実行の初期状態は一致する）
も得られる。

## 状態のモデル

ROOPL++ は静的型付けなので，整数変数とオブジェクト変数を別のストアに分けている。

```coq
Record state := St {
  vs : id -> Z;              (* 整数変数 *)
  os : id -> option loc;     (* オブジェクト変数（None = nil） *)
  hn : nat;                  (* ヒープの高さ *)
  hp : loc -> field -> Z     (* 各オブジェクトのフィールド *)
}.
```

**ヒープはスタックである。** ROOPL++ の `construct C x s destruct x` はブロック構造なので，
確保される位置は常に現在の高さ `hn` であり，`alloc` が**決定的な関数**になる。これが
「逆向きのブロックが同じ位置を確保し直す」ことの根拠であり，可逆性の証明の要になっている。

状態の比較（`==`）は**ヒープの生きている前半部分だけ**を見る（`hn` 以上のセルは到達不能）。
そのため `dealloc (alloc a x) == a` が不変条件なしで成り立つ。nil や死んだ位置経由のフィールド
読み出しは 0 を返し（`rdf` のクランプ），書き込みは生きた位置を要求する。

関数外延性を仮定しないために，状態を作る規則はその結果を**点ごとの等値 `==`** で関係づける。
その代償が合同性補題 `exec_loopx_eq` である。

## 形式化した範囲

| 構成子 | ROOPL++ の構文 |
|---|---|
| `Sskip` | `skip` |
| `Sassign x o e` | `x += e` / `x -= e` / `x ^= e`（`x ∉ fv(e)` が副条件） |
| `Sfassign x f o e` | `x.f += e` ほか（書き込みが右辺を乱さないことが副条件） |
| `Saassign x ei o e` | **`x[ei] += e`** ほか（添字と右辺が乱れないことが副条件） |
| `Sswap x y` | 整数変数の `x <=> y` |
| `Saswap x e1 y e2` | **`x[e1] <=> y[e2]`**（同一セルの場合も含めて対合） |
| `Soswap x y` | オブジェクト変数の `x <=> y` |
| `Scopy x y` / `Suncopy x y` | `copy C x y` / `uncopy C x y`（`x ≠ y` が副条件） |
| `Sseq` | 文の並び |
| `Sif e1 s1 s2 e2` | `if e1 then s1 else s2 fi e2` |
| `Sloop e1 s1 s2 e2` | `from e1 do s1 loop s2 until e2` |
| `Slocal x e1 s e2` | `local t x = e1  s  delocal t x = e2` |
| `Sobj cl x s` | `construct C x  s  destruct x`（確保・ゼロクリア検査・解放つき） |
| `Socall x m args` / `Souncall x m args` | **`call x::m(...)` / `uncall x::m(...)`（動的束縛）** |
| `Sshow e` | `show(e)` / `print("...")`（状態を変えないので恒等） |
| `Scall m args` / `Suncall m args` | **引数つきメソッド**の `call` / `uncall`（参照渡し） |

式は定数・整数変数・**フィールド参照 `x.f`**・**配列参照 `x[e]`**・二項演算。

**配列は「添字が動的なオブジェクト」として同じヒープに載せている**（`x[i]` は
オブジェクト `x` のセル `i`）。したがって配列の確保・解放も `construct`/`destruct`
と同じ規則で扱える。長さは追跡していない（範囲検査は実装側の動的検査に委ねる）。
添字は `Z.to_nat` で読み替えるので，負の添字はセル 0 を指す。

メソッドは `MDecl ps body`（仮引数リストと本体）で，呼出しは**参照渡し**を
「仮引数を実引数の名前に置き換えて本体を実行する」として形式化している
（`rename (mk_ren ps args) body`）。`invert_rename` により名前替えは反転と可換。
名前替えは capture-avoiding ではない（束縛子も一様に置き換える）が，ROOPL++ の
スコープ規則の下では捕獲は起きず，定理はこの点に依存しない。

副条件について：整数代入は構文的な `x ∉ fv(e)`，フィールド代入は**意味的な条件**
`eval e b = eval e a`（書き込みが右辺の値を変えない）を使う。後者は別名（`x` と `y` が同じ
オブジェクトを指す場合など）を構文で近似せずに直接表現したもの。`copy`/`uncopy` の `x ≠ y`
は ROOPL++ の別名禁止規則そのもので，これが無いと `uncopy x x` が非可逆になる。

### 継承とサブタイプ多相

クラス表 `ctable` は各クラスの親クラスとメソッド表を持ち，`dispatch` 関係が
**オブジェクトの実行時クラス**から継承チェーンを上へたどってメソッドを決める
（`dispatch_det` で一意性も証明済み）。実行時クラスは状態の `hc : loc -> cid`
に記録され，`construct C x` が確保時に書き込む。

受け手の束縛は既存の名前替えで済む：メソッドの**第 1 仮引数が `this`** という規約に
して，`call_body d x args = rename (mk_ren ps (x :: args)) body` とした。

`ex_dispatch_override` / `ex_dispatch_inherited` が多相を示す：まったく同じ
`call o::bump()` が，`o` を B（`bump` をオーバーライド）で構築すると 2 を，
C（A から継承）で構築すると 1 を足す。

呼出し規則には「呼出し中に受け手が動かない・ヒープ高さが釣り合う」という前提を
置いている。ROOPL++ では `this` が代入不可でブロックが釣り合うので構文的に保証
されるが，ここでは意味論の側で述べている。

### 含まれていないもの（今後の課題）

- **`for x in (e1..e2) do s end`** — 可逆化には `from/loop` とは別の帰納法（数え上げ
  チェーンの反転補題）が必要。さらに**本体が範囲式 `e1`/`e2` を変えないこと**が
  可逆性の条件になるが，**この処理系はそれを検査していない**（例:
  `for i in (0..n) do n += 1 end` は逆にできない）。形式化はこの条件を明示する形で
  行うべきで、実装側にも検査を足す価値がある
- **`switch` / `hctiws`** — case 列・`esac` 値・`break` の意味論が大きく，別途必要
- **配列の長さと範囲検査**（`new int[n] xs` の `n` を状態に持たせていない）
- **継承・サブタイプ・動的ディスパッチ**
- **値渡し引数**（`call q(3)` のような式引数と，その不変性条件）
- **`new` / `delete`**（ブロック構造でない確保・解放。ヒープをスタックとして扱えなくなる）
- 型の健全性（well-typed なら実行が詰まらない）。可逆言語では表明の失敗で
  実行が止まりうるので，progress は成り立たない形になる

## 空虚でないことの確認

規則系が矛盾していれば定理はすべて空虚に真になるので，具体的な導出を置いてある：

- `ex_swap` — `X += 3; X <=> Y` が `X = 0, Y = 3` に至る
- `ex_loop` — `from X = 0 loop X += 1 until X = 2` が 2 まで数える
- `ex_loop_back` — その逆プログラムが元へ戻る（`exec_invert` の適用）
- `ex_local` — 局所ブロックが `X = 3` を残し局所変数を消す
- **`ex_object`** — `construct O ... destruct O` の中でフィールドに書き，読み戻し，消す。
  ブロック中はヒープが伸び，`destruct` で戻る
- **`ex_wt_array` / `ex_wt_array_inverse`** — 配列プログラムが型付けでき，
  その逆プログラムも `wt_invert` で型付けできる
- **`ex_array`** — ブロック内で `ar[0] += 5; ar[0] <=> ar[1]; X += ar[1]; ar[1] -= 5`
  を実行し，`X = 5` を残してセルをゼロクリアして解放する
- **`ex_copy_uncopy`** — 参照の複製と取り消しが元に戻る
- **`ex_call_uncall`** — `method inc(int n) n += 1` を `call inc(X)` で呼ぶと
  呼出し側の `X` が 1 になり（＝参照渡しになっている），`uncall inc(X)` で戻る
- `ex_self_assign_stuck` — 副条件が効いていること：`X += X` には導出が存在しない

## 抽出した検証済みインタプリタ

`exec` は関係なので実行できない。`run : nat -> menv -> stm -> state -> option state`
は**実行可能なインタプリタ**で、`run_sound` により「`run` が返す状態は必ず意味論が
許す状態である」ことが証明されている。したがって上の可逆性・決定性の定理は
そのまま `run` にも移る（`run_injective` / `run_invert`）。

燃料で再帰を止めるので `run` は全域関数。`None` は「燃料切れ」か「導出が無い」の
どちらかで、可逆言語では後者（表明を満たさないプログラムの拒否）が普通の動作である。

```
make extract    # coq/extracted/rooplRun.ml{,i} を再生成
```

生成物はリポジトリにコミットしてあるので、OCaml 側のビルドに Rocq は要らない。
`test/extracted_test.ml` が**この抽出インタプリタと `lib/eval.ml` の差分テスト**で、
成功する計算だけでなく**失敗（出口表明の不一致・入口表明が偽・`delocal` の不一致）も
両者で一致する**ことを確認している。ここが「証明と実装をつなぐ」部分にあたる。

### run が扱わないもの

オブジェクトブロック・フィールド/配列更新・動的束縛では `run` は `None` を返す。
`destruct` の規則が「**すべての**フィールドがゼロ」を要求しており、フィールドが
無限にあるこのモデルでは決定不能なため。各オブジェクトにセル数 `hz : loc -> nat`
を持たせれば有界になり決定可能になる（状態の 6 成分目を足す改修）。

## 対応関係（実装との）

`roopl.v` の `exec` は `lib/eval.ml` の `eval_state` に対応する。

| 証明側 | 実装側の検査 |
|---|---|
| `E_if_t` / `E_if_f` の出口表明 | `ERROR:assertion is incorrect` |
| `E_loop` の入口表明，`L_step` の `eval e1 b = 0` | `Assertion should be true` / `should be false` |
| `E_local` の `vs b x = eval e2 b` | `delocal` の一致検査 |
| `E_obj` の `forall f, hp b (hn a) f = 0` | `delete` 前のゼロクリア検査（`All instance field is not zero-cleared`） |
| `E_copy` の `os a y = None` | `copy` の対象が nil であること |
| `E_uncopy` の `os a x = os a y` | `uncopy` の両者が同じ参照であること |

これらの表明が落ちる経路は `test/error_test.ml` で実装側からも検査している。
