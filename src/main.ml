open Syntax 
open Print
open Eval

let parse str =
  Parser.main Lexer.token
    (Lexing.from_string str)
  
let eval prog = eval_prog(parse prog)
let evalp s = print_result(eval_prog(parse s))
              
let rec read () =
  flush stdout;
  let _ = Parser.main Lexer.token (Lexing.from_channel stdin)
  in
  read()
;;

(* read() *)
