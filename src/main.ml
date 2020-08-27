open Print
open Eval

let parse str =
  Parser.main Lexer.token
    (Lexing.from_channel str)

let eval prog = print_result(eval_prog prog)

let () =
  let file_name = Printf.sprintf "%s" (Sys.argv.(1)) in
  let channel = open_in file_name in
  try let prog = parse channel in
      let _ = close_in channel in
      eval prog
  with Util.Parse_error (start_pos, end_pos) ->
    Printf.printf "Parse error at %d.%d-%d.%d\n"
      start_pos.pos_lnum (start_pos.pos_cnum - start_pos.pos_bol)
      end_pos.pos_lnum (end_pos.pos_cnum - end_pos.pos_bol);
    exit 1


(*open Syntax
open Print
open Eval

let parse str =
  Parser.main Lexer.token
    (Lexing.from_string str)

let eval prog = eval_prog(parse prog)
let evalp s = print_result(eval_prog(parse s))
 *)
