# ROOPLPP-interpreter

OCaml で実装した **ROOPL++** インタプリタ。ROOPL++ は可逆オブジェクト指向プログラミング言語で，
すべての文が逆を持つ。本インタプリタはプログラムを順方向に実行できるほか，`-inverse` で逆プログラムを生成できる。

参考文献:

> [1] Cservenka, M.H.: *Design and Implementation of Dynamic Memory Management in a Reversible
> Object-Oriented Programming Language*, Master's thesis, Department of Computer Science,
> University of Copenhagen (2018).

## 必要なもの

- OCaml（`ocamlc` / `ocamllex` / `ocamlyacc`）
- ocamlfind
- extlib
- ounit2（テストを実行する場合）
- php / npm（オンラインインタプリタを使う場合）

OPAM を使う例:

```
sudo apt update
sudo apt install opam
opam init
opam install ocamlfind extlib ounit2
```

## ビルド

```
cd src
make            # ./rplpp を生成（ocamlyacc/ocamllex を先に実行）
make clean      # 生成物（parser.ml/lexer.ml/*.cmo/rplpp）を削除
```

`parser.ml` `parser.mli` `lexer.ml` は `parser.mly` / `lexer.mll` から自動生成されるので直接編集しない。

## 実行

`src/` 内で実行する（標準ライブラリのパスが `../library/Library.rplpp` 固定のため）。

```
./rplpp ../example/fib.rplpp            # 順方向に実行
./rplpp -inverse ../example/fib.rplpp   # 逆プログラムを表示
./rplpp -library ../example/fib.rplpp   # 標準ライブラリ（library/Library.rplpp）を読み込んで実行
```

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

ユニットテストは OUnit2 を使う。`test/` で実行する。

```
cd test
make              # 全スイートをビルドして実行
make test_eval    # 個別実行（他に test_invert / test_print / test_pretty / test_env / test_eval_prog）
make veryclean    # *.byte と OUnit の *.log / *.cache を削除
```

各スイートは実行前に `../src` をビルドし直すので，`src/` の変更が反映される。
スイートの内訳:

| ターゲット | 対象 |
|------------|------|
| `test_eval` | 式・文の評価（`eval_exp` / `eval_state`）と可逆性（順方向＋逆で状態が戻る） |
| `test_invert` | 文の逆変換と対合性（`invert (invert s) = s`） |
| `test_print` | 実行結果の表示 |
| `test_pretty` | ソースへの整形（文字列エスケープ・式の括弧付け） |
| `test_env` | 環境とストア |
| `test_eval_prog` | ソース文字列からの parse → `eval_prog`（字句・構文解析を含むエンドツーエンド） |

## オンラインインタプリタ

`web/` に PHP + TypeScript のフロントエンドがある。コンパイル済みの `rplpp` を呼び出して実行する。

```
cd web
mkdir programs && chmod 777 programs   # 実行プログラムの一時保存先
npm install
npm run build                          # フロント用ファイルを生成
php -S localhost:9000                  # ローカルサーバ起動
```

ブラウザで <http://localhost:9000> を開き，ページ下の Execute で実行する。

## ライセンス

`LICENSE` を参照。
