(**Parser_error*)

exception Parse_error of Lexing.position * Lexing.position

(**実行時エラーのうち、すでに「どの文で起きたか」が付いているもの。

   eval.ml は文脈のない失敗を [Failure] で投げ、文の実行を包むラッパが
   その文の pretty 表示を足して [Runtime_error] に格上げする。これにより
   入れ子の文ごとに同じ文が何度も積まれるのを防ぎ、いちばん内側の文だけが
   メッセージに残る（diagnostics.ml が整形する）。 *)
exception Runtime_error of string
