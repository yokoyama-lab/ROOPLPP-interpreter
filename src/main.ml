open Print
open Eval

let parse str =
  Parser.main Lexer.token
    (Lexing.from_channel str)
  
let eval prog = print_result(eval_prog prog)
              
let () =
  let file_name = Printf.sprintf "%s" (Sys.argv.(1)) in
  let channel = open_in file_name in
  let prog = parse channel in
  let _ = close_in channel in
  eval prog

(*いろいろ試す用)
open Syntax 
open Print
open Eval

let parse str =
  Parser.main Lexer.token
    (Lexing.from_string str)
  
let eval prog = eval_prog(parse prog)
let evalp s = print_result(eval_prog(parse s))
 *)
