open OUnit2

(* rplpp コマンド（bin/main.ml）のテスト。

   ライブラリ側のテストはインタプリタの中身しか見ないので、CLI の約束
   （フラグの効き方・終了コード・ゼロクリア検査の出力・エラーの体裁）は
   これまで誰も検査していなかった。ここでは実際にバイナリを起動して
   標準出力と終了コードを確かめる。 *)

let rplpp =
  let candidates =
    [ "../bin/main.exe"; "_build/default/bin/main.exe";
      "../../../_build/default/bin/main.exe" ]
  in
  try List.find Sys.file_exists candidates
  with Not_found -> assert_failure "rplpp executable not found"

let read_file path =
  let ch = open_in_bin path in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch; s

let write_file path content =
  let ch = open_out_bin path in
  output_string ch content;
  close_out ch

(* プログラムを一時ファイルに書き、引数付きで rplpp を起動して
   (終了コード, 標準出力+標準エラー) を返す *)
let run ?(args = []) src =
  let prog_file = Filename.temp_file "rplpp_cli" ".rplpp" in
  let out_file = Filename.temp_file "rplpp_out" ".txt" in
  write_file prog_file src;
  let cmd =
    Printf.sprintf "%s %s %s > %s 2>&1"
      (Filename.quote rplpp)
      (String.concat " " (List.map Filename.quote args))
      (Filename.quote prog_file) (Filename.quote out_file)
  in
  let code = Sys.command cmd in
  let out = read_file out_file in
  Sys.remove prog_file; Sys.remove out_file;
  (code, out)

let has needle s = Diagnostics.contains ~needle s

let assert_has needle s =
  assert_bool (Printf.sprintf "expected %S in:\n%s" needle s) (has needle s)

let assert_not_has needle s =
  assert_bool (Printf.sprintf "did not expect %S in:\n%s" needle s) (not (has needle s))

(* 結果が残るプログラム *)
let dirty = "class Program\n int x\n method main()\n  x += 7\n"

(* すべてゼロに戻るプログラム *)
let clean =
  "class Program\n int x\n method main()\n"
  ^ "  local int t = 0\n  t += 3\n  t -= 3\n  delocal int t = 0\n"

let runtime_error =
  "class Program\n int x\n int[] a\n method main()\n  new int[2] a\n  x += a[4]\n"

let parse_error = "class Program\n int x\n method main()\n  x = 1\n"

let suite = "test suite for the rplpp command" >::: [

      "a normal run exits 0 and prints the result" >:: (fun _ ->
        let code, out = run dirty in
        assert_equal ~printer:string_of_int 0 code;
        assert_has "x = 7" out);

      "the zero-clear report lists the non-zero variables" >:: (fun _ ->
        let _, out = run dirty in
        assert_has "zero-clear check" out;
        assert_has "1 of 1 value(s) are NOT zero-cleared" out);

      "a clean program is reported as clean" >:: (fun _ ->
        let code, out = run clean in
        assert_equal ~printer:string_of_int 0 code;
        assert_has "are zero-cleared (no garbage left)" out);

      "-no-zero-check suppresses the report" >:: (fun _ ->
        let code, out = run ~args:[ "-no-zero-check" ] dirty in
        assert_equal ~printer:string_of_int 0 code;
        assert_has "x = 7" out;
        assert_not_has "zero-clear check" out);

      "a runtime error exits 1 with a structured report" >:: (fun _ ->
        let code, out = run runtime_error in
        assert_equal ~printer:string_of_int 1 code;
        assert_has "ROOPL++ execution error" out;
        assert_has "message: Array index a[4] is out of bounds" out;
        assert_has "Source:" out;
        assert_has "Values on entry to this statement:" out;
        assert_has "Fix hints:" out);

      "a parse error exits 1 with an excerpt and a caret" >:: (fun _ ->
        let code, out = run parse_error in
        assert_equal ~printer:string_of_int 1 code;
        assert_has "ROOPL++ parse error" out;
        assert_has "line 4, column" out;
        assert_has "^" out;
        (* ocamlyacc 既定の裸のメッセージは出さない *)
        assert_not_has "syntax error" out);

      "-inverse prints a program that parses back" >:: (fun _ ->
        let code, out = run ~args:[ "-inverse" ] dirty in
        assert_equal ~printer:string_of_int 0 code;
        assert_not_has "zero-clear check" out;
        ignore (Parser.main Lexer.token (Lexing.from_string out)));

      "-inverse of an erroneous program still only inverts" >:: (fun _ ->
        (* 反転は評価しないので、実行時エラーになるプログラムでも成功する *)
        let code, _ = run ~args:[ "-inverse" ] runtime_error in
        assert_equal ~printer:string_of_int 0 code);

      "a missing file exits 1" >:: (fun _ ->
        let out_file = Filename.temp_file "rplpp_out" ".txt" in
        let code =
          Sys.command
            (Printf.sprintf "%s %s > %s 2>&1" (Filename.quote rplpp)
               (Filename.quote "no_such_file_here.rplpp") (Filename.quote out_file))
        in
        let out = read_file out_file in
        Sys.remove out_file;
        assert_equal ~printer:string_of_int 1 code;
        assert_has "No such file" out);

      "no argument exits 1 with the usage line" >:: (fun _ ->
        let out_file = Filename.temp_file "rplpp_out" ".txt" in
        let code =
          Sys.command
            (Printf.sprintf "%s > %s 2>&1" (Filename.quote rplpp) (Filename.quote out_file))
        in
        let out = read_file out_file in
        Sys.remove out_file;
        assert_equal ~printer:string_of_int 1 code;
        assert_has "Usage: rplpp" out);
    ]

let _ = run_test_tt_main suite
