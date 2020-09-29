# ROOPLPP-interpreter
An interpreter for ROOPLPP in OCaml

## Requirements
+ php
+ OCaml
 + ocamlfind
 + ocamlyacc
 + extlib
 + OUnit(テストケースを実行する場合)

## Linux Ubuntuにてインストール例
+ 本リポジトリをダウンロード
```
git clone https://github.com/yokoyama-lab/ROOPLPP-interpreter.git
```

+ phpをインストール
```
sudo apt-get install php
```

+ OCamlやその他の必要なものをインストール
```
sudo apt update
sudo apt install opam
opam init
opam update
opam switch
opam install extlib ocamlfind
```

+ ディレクトリsrcに移動し,makeする
```
cd ROOPLPP-interpreter/src
make
```
## オンラインインタープリタ実行方法
+ ディレクトリwebに移動し,ディレクトリprogramsを作成

```
cd ../web
mkdir programs
```

+ ディレクトリprogramsのアクセス権を変更

```
mkdir program
chmod 777 programs
```

+ ディレクトリweb内で以下のコマンドを打ち,ローカルサーバを起動
```
php -S localhost:9000
```

+ ブラウザで "http://localhost:9000" を開くと,ROOPL++のオンラインインタープリタが表示される

+ ページ下のExecuteを押すとプログラムを実行できる

## プログラム実行方法
+ ディレクトリsrc内で以下のコマンドを打つことでROOPL++プログラムが書かれたファイルを実行できる．(以下のコマンドでは，example/fib.rplppが実行される)
```
./rplpp ../example/fib.rplpp
```
+ 実行時にオプションで-inverseを指定することで逆プログラムを表示させることができる．(以下のコマンドでは，example/fib.rplppの逆プログラムが表示される)
```
./rplpp -inverse ../example/fib.rplpp
```

## テストケース実行方法
+ 単体テストフレームワークのOUnitをインストール
```
opam install ounit
```

+ ディレクトリtestに移動
```
cd test
```

+ 以下のようにタイプすると,ファイルeval_test.ml内のテストケースが実行される.
```
make test_eval
```
