# ROOPL++ の可逆性の機械検証（Rocq）

このインタプリタが実装している **ROOPL++**（Cservenka 2018）の**文のコア**について，
可逆性を Rocq で機械検証したもの。単一ファイル `roopl.v`（674 行）。

ROOPL / ROOPL++ には紙の操作的意味論と型システム（Haulund 2017, Cservenka 2018）があり，
「文の反転で well-typedness が保存される」ことなども証明されているが，**証明支援系による
機械検証はこれまで無かった**。姉妹プロジェクト PyJanus の vJanus（`PyJanus/coq/`）が Janus に
対して行っている検証の，ROOPL++ 版の第一歩にあたる。

## ビルド

```
make          # rocq c -Q . ROOPL roopl.v
make check    # rocq check（独立した検査器で .vo を再検査）
```

Rocq 9.1.1 で確認（`coqc` 互換）。外部ライブラリ依存なし（`Stdlib` のみ）。

## 証明した定理

| 定理 | 主張 |
|---|---|
| `invert_invert` | `invert (invert s) = s` — 反転は対合 |
| `exec_invert` | `exec s a b → exec (invert s) b a` — 反転プログラムは逆向きに走る |
| `exec_iff` | `exec s a b ↔ exec (invert s) b a` |
| `exec_det` | 前方決定性 |
| **`exec_inj`** | `exec s a1 b → exec s a2 b → a1 == a2` — **可逆性**：最終ストアが初期ストアを一意に定める（プログラムはストア上の単射部分関数を表す） |
| `exec_round_trip` | 実行してから逆を実行すると元に戻る |

補助定理として `exec_loopx_eq`（実行はストアの点ごとの等しさを保つ），`loopx_exit`（ループ末尾は
出口表明を満たす）を経由する。`exec_invert` / `exec_det` はいずれも `exec` と補助関係 `loopx`
に対する**相互帰納法**（`Combined Scheme`）で証明している。

**公理はゼロ**。6 定理すべてについて `Print Assumptions` が `Closed under the global context`
を返す（ビルド時に表示される）。ストアを関数で表しているため通常は関数外延性が必要になるが，
ストアを作る規則が結果を**点ごとの等値 `==`** で関係づける形にしてあるので公理を使わない。
その代償が `exec_loopx_eq`（実行が `==` を尊重する合同性）である。

## 形式化した範囲

`stm` は ROOPL++ の**文のコア**：

| 構成子 | ROOPL++ の構文 |
|---|---|
| `Sskip` | `skip` |
| `Sassign x o e` | `x += e` / `x -= e` / `x ^= e`（`x ∉ fv(e)` が規則の副条件） |
| `Sswap x y` | `x <=> y` |
| `Sseq` | 文の並び |
| `Sif e1 s1 s2 e2` | `if e1 then s1 else s2 fi e2`（入口条件＋出口表明） |
| `Sloop e1 s1 s2 e2` | `from e1 do s1 loop s2 until e2` |
| `Slocal x e1 s e2` | `local t x = e1  s  delocal t x = e2` |
| `Sobj x s` | `construct C x  s  destruct x` |
| `Scall m` / `Suncall m` | 引数なしメソッドの `call` / `uncall` |

`mapp`（`+= -= ^=`）の可逆性は `mapp_minv : mapp (minv o) (mapp o a b) b = a` として証明
（`^=` は `Z.lxor` で，`Z.lxor_assoc` / `Z.lxor_nilpotent` から従う）。

### 含まれていないもの（今後の課題）

**ROOPL++ の対象指向の部分は，この形式化の対象外である**：

- **フィールドを持つオブジェクト**（`Sobj` はオブジェクトを 1 セルとして扱う。`x.f` は無い）
- **配列**（`new int[n] xs` / `delete` / `xs[i]`）
- **継承・サブタイプ・動的ディスパッチ**
- **引数つきメソッド**（値引数の不変性条件を含む）と参照の複製（`copy` / `uncopy`）
- 型システム（Haulund 2017 の「反転で型が保存される」の機械化）

つまり本形式化が押さえているのは，**この処理系の `eval_state` が実装している制御構造と
局所状態の核**であって，ヒープと対象指向部分は紙の意味論のままである。ROOPLpp2HSSA が
HSSA 側で「最小のソース言語（Roopl++ ではない）」と明記しているのと同じ精度の主張である。

## 空虚でないことの確認

規則系が矛盾していれば定理はすべて空虚に真になるので，具体的な導出を置いてある：

- `ex_swap` — `X += 3; X <=> Y` が `X = 0, Y = 3` に至る
- `ex_loop` — `from X = 0 loop X += 1 until X = 2` が 2 まで数える
- `ex_loop_back` — その逆プログラムが元へ戻る（`exec_invert` の適用）
- `ex_local` — 局所ブロックが `X = 3` を残し局所変数を消す
- `ex_call_uncall` — `call` してから `uncall` すると元に戻る
- `ex_self_assign_stuck` — 副条件が効いていること：`X += X` には導出が存在しない

## 対応関係（実装との）

`roopl.v` の `exec` は `lib/eval.ml` の `eval_state` に対応する。特に

- `E_if_t` / `E_if_f` の出口表明 → `eval.ml` の `ERROR:assertion is incorrect`
- `E_loop` の入口表明と `L_step` の `eval e1 b = 0` → `Assertion should be true` / `should be false`
- `E_local` の `b x = eval e2 b` → `delocal` の一致検査
- `E_obj` の `b x = 0` → `delete` 前のゼロクリア検査

これらの表明が落ちる経路は `test/error_test.ml` で実装側からも検査している。
