open Print
open Eval
open Pretty
open Invert
let parse str =
  Parser.main Lexer.token
    (Lexing.from_channel str)

let pretty prog = pretty_prog prog
  
let eval prog = print_result(eval_prog prog)
let eval_lib lib prog = print_result(eval_prog ~library0:lib prog)

let () =
  let files = ref [] in
  let inv = ref false in
  let lib = ref false in
  Arg.parse
    [("-inverse", Arg.Set inv, "inversion");
    ("-library", Arg.Set lib, "library")]
    (fun s -> files := !files @ [s])
    ("ROOPLPP interpreter");
  match !files with
  | [file_name] ->
     let channel = open_in file_name in
     let prog =
       try parse channel with
         Util.Parse_error (start_pos, end_pos) ->
         Printf.printf "Parse error at %d.%d-%d.%d\n"
           start_pos.pos_lnum (start_pos.pos_cnum - start_pos.pos_bol)
           end_pos.pos_lnum (end_pos.pos_cnum - end_pos.pos_bol);
         exit 1
     in
     let _ = close_in channel in
     if !inv then pretty_prog(invert_prog prog)
     else
      try if !lib then let channel2 = open_in "../library/Library.rplpp" in
      let library = parse channel2 in 
      eval_lib library prog
      else eval prog
      with
      Failure e ->  print_newline(); print_endline e

(*let () =
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


open Syntax
open Print
open Eval

let parse str =
  Parser.main Lexer.token
    (Lexing.from_string str)
*)
(*let eval prog = eval_prog(parse prog)
let evalp s = print_result(eval_prog(parse s))
 *)
