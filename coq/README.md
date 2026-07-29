# ROOPL++ の可逆性の機械検証（Rocq）

このインタプリタが実装している **ROOPL++**（Cservenka 2018）について，可逆性を Rocq で
機械検証したもの。単一ファイル `roopl.v`（1043 行，外部ライブラリ依存なし）。

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

補助定理として `exec_loopx_eq`（実行は状態の点ごとの等しさを保つ），`loopx_exit`（ループ末尾は
出口表明を満たす）を経由する。`exec_invert` / `exec_det` はいずれも `exec` と補助関係 `loopx`
に対する**相互帰納法**（`Combined Scheme`）で証明している。

**公理はゼロ**。6 定理すべてについて `Print Assumptions` が `Closed under the global context`
を返す（ビルド時に表示される）。

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
| `Sswap x y` | 整数変数の `x <=> y` |
| `Soswap x y` | オブジェクト変数の `x <=> y` |
| `Scopy x y` / `Suncopy x y` | `copy C x y` / `uncopy C x y`（`x ≠ y` が副条件） |
| `Sseq` | 文の並び |
| `Sif e1 s1 s2 e2` | `if e1 then s1 else s2 fi e2` |
| `Sloop e1 s1 s2 e2` | `from e1 do s1 loop s2 until e2` |
| `Slocal x e1 s e2` | `local t x = e1  s  delocal t x = e2` |
| `Sobj x s` | `construct C x  s  destruct x`（確保・ゼロクリア検査・解放つき） |
| `Scall m args` / `Suncall m args` | **引数つきメソッド**の `call` / `uncall`（参照渡し） |

式は定数・整数変数・**フィールド参照 `x.f`**・二項演算。

メソッドは `MDecl ps body`（仮引数リストと本体）で，呼出しは**参照渡し**を
「仮引数を実引数の名前に置き換えて本体を実行する」として形式化している
（`rename (mk_ren ps args) body`）。`invert_rename` により名前替えは反転と可換。
名前替えは capture-avoiding ではない（束縛子も一様に置き換える）が，ROOPL++ の
スコープ規則の下では捕獲は起きず，定理はこの点に依存しない。

副条件について：整数代入は構文的な `x ∉ fv(e)`，フィールド代入は**意味的な条件**
`eval e b = eval e a`（書き込みが右辺の値を変えない）を使う。後者は別名（`x` と `y` が同じ
オブジェクトを指す場合など）を構文で近似せずに直接表現したもの。`copy`/`uncopy` の `x ≠ y`
は ROOPL++ の別名禁止規則そのもので，これが無いと `uncopy x x` が非可逆になる。

### 含まれていないもの（今後の課題）

- **配列**（`new int[n] xs` / `delete` / `xs[i]`）
- **継承・サブタイプ・動的ディスパッチ**
- **値渡し引数**（`call q(3)` のような式引数と，その不変性条件）
- **`new` / `delete`**（ブロック構造でない確保・解放。ヒープをスタックとして扱えなくなる）
- 型システム（Haulund 2017 の「反転で型が保存される」の機械化）

## 空虚でないことの確認

規則系が矛盾していれば定理はすべて空虚に真になるので，具体的な導出を置いてある：

- `ex_swap` — `X += 3; X <=> Y` が `X = 0, Y = 3` に至る
- `ex_loop` — `from X = 0 loop X += 1 until X = 2` が 2 まで数える
- `ex_loop_back` — その逆プログラムが元へ戻る（`exec_invert` の適用）
- `ex_local` — 局所ブロックが `X = 3` を残し局所変数を消す
- **`ex_object`** — `construct O ... destruct O` の中でフィールドに書き，読み戻し，消す。
  ブロック中はヒープが伸び，`destruct` で戻る
- **`ex_copy_uncopy`** — 参照の複製と取り消しが元に戻る
- **`ex_call_uncall`** — `method inc(int n) n += 1` を `call inc(X)` で呼ぶと
  呼出し側の `X` が 1 になり（＝参照渡しになっている），`uncall inc(X)` で戻る
- `ex_self_assign_stuck` — 副条件が効いていること：`X += X` には導出が存在しない

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
