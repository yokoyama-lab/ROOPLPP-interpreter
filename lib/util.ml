(**Parser_error*)

exception Parse_error of Lexing.position * Lexing.position

(**実行時エラーのうち、すでに「どの文で起きたか」が付いているもの。

   eval.ml は文脈のない失敗を [Failure] で投げ、文の実行を包むラッパが
   その文の pretty 表示を足して [Runtime_error] に格上げする。これにより
   入れ子の文ごとに同じ文が何度も積まれるのを防ぎ、いちばん内側の文だけが
   メッセージに残る（diagnostics.ml が整形する）。 *)
exception Runtime_error of string

(**式の中で起きた失敗。位置 (行, 列, 終了行, 終了列) を運ぶ。

   [eval_exp] の位置つき式のラッパがいちばん内側で付け、外側の式のラッパは
   メッセージだけを積み増して位置は保つ。文のラッパがこれを受け取って
   [Runtime_error] に格上げし、診断はその位置にキャレットを出す。 *)
exception Expr_error of (int * int * int * int) * string
