# ROOPLPP-interpreter

OCaml で実装した **ROOPL++** インタプリタ。ROOPL++ は可逆オブジェクト指向プログラミング言語で，
すべての文が逆を持つ。本インタプリタはプログラムを順方向に実行できるほか，`-inverse` で逆プログラムを生成できる。

参考文献:

> [1] Cservenka, M.H.: *Design and Implementation of Dynamic Memory Management in a Reversible
> Object-Oriented Programming Language*, Master's thesis, Department of Computer Science,
> University of Copenhagen (2018).

## 必要なもの

- OCaml
- dune（>= 3.0）
- ounit2（テストを実行する場合）
- php / npm（オンラインインタプリタを使う場合）

OPAM を使う例:

```
sudo apt update
sudo apt install opam
opam init
opam install dune ounit2
```

## 構成

```
dune-project        プロジェクト定義
rooplpp.opam        パッケージ定義（依存・メタデータ）
lib/                ライブラリ（AST・字句/構文解析・評価・逆変換・整形）
bin/                実行ファイル rplpp（main.ml）
test/               OUnit2 テストスイート
example/            サンプルプログラム（*.rplpp）
library/            標準ライブラリ（Library.rplpp）
coq/                可逆性の機械検証（Rocq, roopl.v）
python/             Python への移植（差分テストで OCaml 実装と同期）
web/                オンラインインタプリタ（PHP + TypeScript）
```

## ビルド

リポジトリ直下で:

```
dune build          # lib/ と bin/ をビルド（字句/構文解析器も自動生成）
dune clean          # 生成物（_build/）を削除
```

`lib/lexer.mll`・`lib/parser.mly` から字句/構文解析器が自動生成される（編集対象は `.mll`/`.mly` 側）。

## 実行

リポジトリ直下で `dune exec` する（`-library` は `library/Library.rplpp` を直下から読む）。

```
dune exec rplpp -- example/fib.rplpp            # 順方向に実行
dune exec rplpp -- -inverse example/fib.rplpp   # 逆プログラムを表示
dune exec rplpp -- -library example/fib.rplpp   # 標準ライブラリを読み込んで実行
dune exec rplpp -- -no-zero-check example/fib.rplpp  # ゼロクリア検査の出力を抑止
```

ビルド済みバイナリは `_build/default/bin/main.exe` にある。

`-inverse` の出力は再度パースできる正しい ROOPL++ ソースになっている（逆プログラムをそのまま実行・再反転できる）。

### 実行後のゼロクリア検査

実行が終わると、`main` を持つクラスの変数のうち **0 / nil に戻っていないもの**を一覧する
（PyJanus の `Warning: non-zero values remain ...` に相当）。可逆プログラムは終了時に
すべてがゼロへ戻るのが「クリーン」で、残っているものは意図した出力かガーベジ（未回収の情報）
のどちらかである。`delete` されずに残ったオブジェクトや配列は中身まで展開して表示する。

```
$ dune exec rplpp -- example/algo_zagier.rplpp
x = 1
y = 9
z = 1
ROOPL++ zero-clear check: 3 of 3 value(s) are NOT zero-cleared:
  x = 1
  y = 9
  z = 1
  note: ...
```

出力を機械的に比較したいときは `-no-zero-check` で抑止できる。

### エラー出力

構文エラー・実行時エラーは、原因・場所・そのときの変数の値・修正のヒントを添えた
構造化テキストで表示し、**終了コード 1** で終わる（人が読むためと、LLM に修正させるための
両方を意図している）。

```
ROOPL++ execution error
  message: Array index a[4] is out of bounds
  file: example/foo.rplpp
  line: 11, columns 21-25

Source:
    10 |         for i in (0..4) do
  > 11 |             total += a[i]
       |                      ^^^^
    12 |         end

Trace (outermost first):
  1: total += a[i]
  2: a[i]

Values on entry to this statement:
  total = 6; a = <int[4]> { [1] = 1; [2] = 2; [3] = 3 }; i = 4

Fix hints:
  - `new int[n] xs` allocates xs[0] .. xs[n-1] only; ...
```

位置は**構文解析器が付ける**（文は `Positioned`、式は `EPos`）。同じ文字列の文が複数行に
あっても落ちた行を一意に言え、失敗した部分式にはキャレットを引く。位置を持たない文だけ、
従来どおり「pretty 表示した文とソース行の照合」で推定する。整形は `lib/diagnostics.ml`、
テストは `test/diagnostics_test.ml`。

## 言語の概要

プログラムはクラスの並び。`main` メソッドを持つクラスから実行が始まる。

```
// n 番目のフィボナッチ対を計算
class Fib
    int[] xs
    method init()
        new int[2] xs
    method fib(int n)
        if n = 0 then
            xs[0] ^= 1
            xs[1] ^= 1
        else
            n -= 1
            call fib(n)
            xs[0] += xs[1]
            xs[0] <=> xs[1]
        fi xs[0] = xs[1]

class Program
    int result
    method main()
        local Fib f = nil
        new Fib f
        call f::init()
        call f::fib(4)
        ...
```

主な構文（いずれも逆を持つ）:

| 種類 | 構文 |
|------|------|
| 可逆代入 | `x += e` / `x -= e` / `x ^= e` |
| 入れ替え | `x <=> y` |
| 条件分岐 | `if e then s else s fi e`（入口条件と出口表明） |
| ループ | `from e do s loop s until e` |
| for | `for x in (e1..e2) do s end` |
| switch | `switch x case e s esac e break ... default s break hctiws x` |
| 局所変数 | `local t x = e  s  delocal t x = e` |
| オブジェクト | `construct C x s destruct x` / `new C x` / `delete C x` |
| 配列 | `new C[n] xs` / `delete C[n] xs` |
| 参照複製 | `copy t x y` / `uncopy t x y` |
| メソッド呼出 | `call q(...)` / `uncall q(...)` / `call x::q(...)` / `uncall x::q(...)` |
| 入出力 | `show(e)` / `print("...")` |

式では算術（`+ - * / %`）・ビット（`& | ^`）・論理（`&& ||`）・比較（`< <= > >= = !=`）・`nil`・配列要素 `xs[i]` が使える。
整数リテラルは10進のほか **16進 `0xFF`，2進 `0b1010`，文字 `'A'`**（ASCII コード，`\n \t \\ \' \"` のエスケープ可）も書ける。
コメントは `//` 以降行末まで。

`call`/`uncall` の引数は変数で渡す（可逆呼出しの規約により，呼出し後に元へ戻せる必要があるため）。
配列要素（`a[i]`）は値渡しになるため，呼出し先で書き換えるものは「配列＋添字」を渡す。

`&&` `||` は**短絡評価しない**（両辺を必ず評価する）。`i < n && a[i] = 0` のような
「添字ガード」は書けないので，範囲外にならない添字へクランプするか，番兵の要素を
余分に確保する（`example/algo_dijkstra.rplpp`，`example/BinaryHeap.rplpp` を参照）。

## サンプルと標準ライブラリ

- `example/*.rplpp` — データ構造（`LinkedList` `BinaryTree` `DoublyLinkedList`
  `DynamicArray` `BinaryHeap` `TreeSort` など）と可逆アルゴリズム（`algo_*`）。
- 一部は姉妹プロジェクト **PyJanus**（Janus インタプリタ）のサンプルからの移植で，
  可逆計算の定石（Bennett の compute-copy-uncompute，決定ビットのログ化，
  対合・全単射の直接表現）を例示する。ファイル冒頭のコメントに出典を書いている。
- `library/Library.rplpp` — `-library` で読み込む標準ライブラリ。

## テスト

ユニットテストは OUnit2 を使う。リポジトリ直下で:

```
dune test               # 全スイートを実行
dune test --force       # キャッシュを無視して再実行
dune exec test/eval_test.exe   # 個別スイートを実行
```

スイートの内訳（`test/*_test.ml`）:

| スイート | 対象 |
|------------|------|
| `eval_test` | 式・文の評価（`eval_exp` / `eval_state`）と可逆性（順方向＋逆で状態が戻る） |
| `invert_test` | 文の逆変換と対合性（`invert (invert s) = s`） |
| `print_test` | 実行結果の表示（出力文字列を検証） |
| `pretty_test` | ソースへの整形（文字列エスケープ・式の括弧付け） |
| `env_store_test` | 環境とストア |
| `eval_prog_test` | ソース文字列からの parse → `eval_prog`（字句・構文解析を含むエンドツーエンド） |
| `diagnostics_test` | 診断メッセージの整形・行推定・修正ヒント・ゼロクリア検査 |
| `error_test` | エラー経路（可逆性の表明が正しく落ちること：ループ・条件分岐の表明，`delocal` の一致，`delete` 前のゼロクリア，値引数の不変性ほか） |
| `example_test` | `example/*.rplpp` 全件の回帰（パース／`invert(invert p)=p`／`parse(pretty p)=p`／実行）＋代表例の値 |
| `cli_test` | `rplpp` コマンド（フラグ・終了コード・ゼロクリア検査の出力・エラーの体裁） |

### Python ポートのテスト

`python/` は `lib/` の手作業移植なので、**両実装が同じ振る舞いをすることを差分テストで
検査する**（`example/` 全件の標準出力と終了コード、`-inverse` の出力、`-no-zero-check`）。

```
cd python && python3 -m pytest tests -q     # 202 件（差分163 / 診断22 / エラー経路17）
```

OCaml 側のバイナリ（`_build/default/bin/main.exe`）が無い場合、差分テストはスキップされる。

カバレッジは bisect_ppx で測れる（`opam install bisect_ppx` が必要）。

```
dune test --force --instrument-with bisect_ppx   # lib/dune に (instrumentation (backend bisect_ppx)) を追加して実行
bisect-ppx-report summary --per-file
```

## 可逆性の機械検証（Rocq）

`coq/roopl.v` に，ROOPL++ の操作的意味論と可逆性の機械検証がある
（Rocq 9.1.1，単一ファイル 1799 行，**公理ゼロ**）。

| 定理 | 主張 |
|---|---|
| `invert_invert` | `invert (invert s) = s` |
| `exec_invert` | `exec s a b → exec (invert s) b a` |
| `exec_iff` | `exec s a b ↔ exec (invert s) b a` |
| `exec_det` | 前方決定性 |
| **`exec_inj`** | **可逆性**：`exec s a1 b → exec s a2 b → a1 == a2` |
| **`run_sound`** | 抽出した実行可能インタプリタが意味論に対して健全 |
| `exec_round_trip` | 実行してから逆を実行すると元に戻る |
| **`wt_invert`** | 反転で型付けが保存される（Haulund 2017 の定理） |

**小ステップ意味論**（`coq/roopl_small.v`）もある。制御トークンを文に埋め込む方式で、
`step_det`（前方決定性）・**`step_inj`（後方決定性＝小ステップの可逆性）**・
**`exec_iff_steps`（大ステップ意味論との同値）** を証明。対象は原子文すべてと
制御構造・**局所ブロック**・**オブジェクトブロック**・**メソッド呼出し**（動的束縛を
含む）で、大ステップ側と同じ全構文をカバーする。
後者の証明では、ループの入口表明と条件分岐の出口表明が「どこから来たか」を一意に
決めており、**二重ガードという言語設計がそのまま可逆性の証明になっている**。

```
cd coq && make && make check     # 証明のビルドと再検査
cd coq && make extract           # 検証済みインタプリタを OCaml へ抽出
```

抽出物 `coq/extracted/rooplRun.ml` はコミット済みで、`test/extracted_test.ml` が
**検証済みインタプリタと `lib/eval.ml` の差分テスト**を行う（成功する計算だけでなく、
表明違反で失敗するプログラムも両者で一致することを確認）。**`run` は形式化した
全構文を実行できる**——オブジェクトブロックの「全フィールドがゼロ」という決定
不能な条件は、書き込みうるフィールド番号の上限を実行と一緒に持ち回ることで
有限の検査に落としている（`run_above`）。

対象は skip・可逆代入・**フィールド代入 `x.f += e`**・**配列代入 `x[e] += e`**・
整数/オブジェクト/**配列要素**の swap・
**`copy`/`uncopy`**・並び・条件分岐・ループ・局所ブロック・**オブジェクトブロック
（確保・ゼロクリア検査・解放つきの実ヒープ）**・**引数つきメソッドの call/uncall（参照渡し）**。
`construct`/`destruct` はブロック構造なのでヒープをスタックとして扱え，確保位置が決定的関数に
なることが可逆性の要になっている。**継承とサブタイプ多相・動的束縛も形式化済み**（`dispatch`／実行時クラス `hc`）。
インタプリタの追加構文 **`for` と `switch` は糖衣として形式化**してある
（`for_up`／`for_down`／`rev_switch`。可逆性は `exec_invert` から従う）。
糖衣は抽出して差分テストにかけてあり、これで**実装が検査を省いていた 2 か所**
（出口の値が重複する `switch`、体がループ変数を書き換える `for`）が見つかった。
どちらも修正済みで、いまは実装と意味論が一致する。
**値渡し引数，配列長の範囲検査，`new`/`delete` は対象外**（詳細と対応関係は `coq/README.md`）。

## オンラインインタプリタ

`web/` に PHP + TypeScript のフロントエンドがある。`execute.php` がビルド済みの
`rplpp`（`_build/default/bin/main.exe`，無ければ旧 `src/rplpp`）を呼び出して実行する。

```
dune build                             # インタプリタをビルド（リポジトリ直下）
cd web
mkdir programs && chmod 777 programs   # 実行プログラムの一時保存先
npm install
npm run build                          # フロント用ファイルを生成
php -S localhost:9000                  # ローカルサーバ起動
```

ブラウザで <http://localhost:9000> を開き，ページ下の Execute で実行する。

## ライセンス

`LICENSE` を参照。
