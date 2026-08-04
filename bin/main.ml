open Print
open Eval
open Pretty
open Invert

(**ファイル全体を文字列として読む（エラー時のソース抜粋に使う）*)
let read_file file_name =
  let channel = open_in_bin file_name in
  let len = in_channel_length channel in
  let s = really_input_string channel len in
  close_in channel;
  s

let parse_string src = Parser.main Lexer.token (Lexing.from_string src)

let () =
  let files = ref [] in
  let inv = ref false in
  let lib = ref false in
  let no_check = ref false in
  Arg.parse
    [("-inverse", Arg.Set inv, " print the inverted program instead of running it");
     ("-library", Arg.Set lib, " load library/Library.rplpp before the program");
     ("-no-zero-check", Arg.Set no_check,
      " suppress the zero-clear (garbage) report printed after execution")]
    (fun s -> files := !files @ [s])
    "ROOPLPP interpreter";
  match !files with
  | [file_name] ->
     let src =
       try read_file file_name with Sys_error e ->
         print_endline ("ROOPL++ error\n  message: " ^ e); exit 1
     in
     let prog =
       try parse_string src with
         Util.Parse_error (start_pos, end_pos) ->
         print_endline
           (Diagnostics.format_parse_error ~src ~file:file_name start_pos end_pos);
         exit 1
     in
     if !inv then pretty_prog (invert_prog prog)
     else
       begin
         try
           let result, st =
             if !lib
             then eval_prog_state ~library0:(parse_string (read_file "library/Library.rplpp")) prog
             else eval_prog_state prog
           in
           print_result result;
           if not !no_check then print_endline (Diagnostics.garbage_report result st)
         with
         | Failure e | Util.Runtime_error e ->
            print_newline ();
            print_endline (Diagnostics.format_runtime_error ~src ~file:file_name e);
            exit 1
         | Util.Parse_error (start_pos, end_pos) ->
            (* -library で読み込む標準ライブラリ側の構文エラー *)
            print_endline
              (Diagnostics.format_parse_error ~file:"library/Library.rplpp" start_pos end_pos);
            exit 1
       end
  | _ ->
     Printf.eprintf
       "Usage: rplpp [-inverse] [-library] [-no-zero-check] <file.rplpp>\n";
     exit 1
