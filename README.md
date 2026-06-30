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
```

ビルド済みバイナリは `_build/default/bin/main.exe` にある。

`-inverse` の出力は再度パースできる正しい ROOPL++ ソースになっている（逆プログラムをそのまま実行・再反転できる）。

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

## サンプルと標準ライブラリ

- `example/*.rplpp` — データ構造（`LinkedList` `BinaryTree` `DoublyLinkedList` など）と可逆アルゴリズム（`algo_*`）。
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
