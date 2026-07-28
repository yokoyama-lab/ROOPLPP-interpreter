open OUnit2
open Syntax

(* example/ のコーパス回帰テスト。

   example/*.rplpp（150本以上）はこの処理系の事実上の振る舞い仕様なのに、
   これまで dune test では 1 本も実行されていなかった。ここで全ファイルに
   対して次の 3 つを毎回確認する。

     1. パースできる
     2. 二重反転が恒等: invert (invert p) = p
     3. pretty 出力が同じ AST に読み戻せる: parse (pretty p) = p
     4. 実行が最後まで走る（意図的なエラー例を除く）

   3 は -inverse の出力をそのまま実行・再反転できるという保証（2026-06-03 の
   pretty.ml の文字列エスケープ回帰がここに当たる）。 *)

let parse src = Parser.main Lexer.token (Lexing.from_string src)

let read_file path =
  let ch = open_in_bin path in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch; s

(* dune test は _build/default/test から、dune exec はリポジトリ直下から
   走るので、example/ を上に向かって探す *)
let example_dir =
  let candidates =
    [ "../example"; "example"; "../../example"; "../../../example" ]
  in
  try List.find (fun d -> Sys.file_exists (Filename.concat d "fib.rplpp")) candidates
  with Not_found -> assert_failure "example/ directory not found"

let examples =
  Sys.readdir example_dir
  |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f ".rplpp")
  |> List.sort compare

(* 実行するとエラーになることが意図されている例 *)
let expected_to_fail = [ "callValue_error.rplpp" ]

let src_of (Prog cl) = Pretty.pretty_cl cl

let check_file name =
  let path = Filename.concat example_dir name in
  let src = read_file path in
  let prog = try parse src with
    | Util.Parse_error _ -> assert_failure (name ^ ": parse error")
  in
  (* 二重反転は恒等 *)
  assert_equal ~msg:(name ^ ": invert (invert p) <> p") prog
    (Invert.invert_prog (Invert.invert_prog prog));
  (* pretty 出力は同じ AST に読み戻せる *)
  let reparsed = try parse (src_of prog) with
    | Util.Parse_error _ -> assert_failure (name ^ ": pretty output does not re-parse")
  in
  assert_equal ~msg:(name ^ ": parse (pretty p) <> p") prog reparsed;
  (* 実行 *)
  if List.mem name expected_to_fail then
    (match (try ignore (Eval.eval_prog prog); None with
            | Util.Runtime_error e | Failure e -> Some e) with
     | None -> assert_failure (name ^ ": expected this example to fail")
     | Some _ -> ())
  else
    (try ignore (Eval.eval_prog prog) with
     | Util.Runtime_error e | Failure e ->
        assert_failure (name ^ ": unexpected runtime error:\n" ^ e))

(* 代表的な例の結果は値まで固定する（回帰の網） *)
let result_of name =
  Eval.eval_prog (parse (read_file (Filename.concat example_dir name)))

let assert_values name expected =
  let result = result_of name in
  List.iter
    (fun (id, v) ->
      match List.assoc_opt id result with
      | None -> assert_failure (Printf.sprintf "%s: %s is not in the result" name id)
      | Some got ->
         assert_equal ~printer:Print.show_val ~msg:(name ^ ": " ^ id) (Value.IntVal v) got)
    expected

let suite = "test suite for the example corpus" >::: [
      "the corpus is not empty" >:: (fun _ ->
        assert_bool "found example programs" (List.length examples > 100));

      "every example parses, inverts, re-parses and runs"
      >::: List.map (fun name -> name >:: (fun _ -> check_file name)) examples;

      (* 値まで固定する代表例（PyJanus 由来の移植は移植元の期待値と一致） *)
      "fib.rplpp" >:: (fun _ -> assert_values "fib.rplpp" [ ("result", 8) ]);

      "algo_zagier.rplpp" >:: (fun _ ->
        assert_values "algo_zagier.rplpp" [ ("x", 1); ("y", 9); ("z", 1) ]);

      "algo_cantor_pair.rplpp" >:: (fun _ ->
        assert_values "algo_cantor_pair.rplpp"
          [ ("z", 41); ("x2", 3); ("y2", 5) ]);

      "algo_bennett.rplpp" >:: (fun _ ->
        assert_values "algo_bennett.rplpp"
          [ ("x", 17); ("q", 3); ("r", 2); ("q2", 0); ("r2", 3) ]);

      "algo_bwt.rplpp" >:: (fun _ ->
        assert_values "algo_bwt.rplpp"
          [ ("s[0]", 0); ("s[5]", 0); ("L[0]", 2); ("L[3]", 1); ("primary", 0) ]);

      "algo_kmp.rplpp" >:: (fun _ ->
        assert_values "algo_kmp.rplpp"
          [ ("cnt", 4); ("f[0]", 0); ("f[1]", 0); ("f[2]", 1) ]);

      "algo_dijkstra.rplpp" >:: (fun _ ->
        assert_values "algo_dijkstra.rplpp"
          [ ("dist[0]", 0); ("dist[1]", 8); ("dist[2]", 9); ("dist[3]", 5);
            ("dist[4]", 7) ]);

      "algo_selection_sort.rplpp" >:: (fun _ ->
        assert_values "algo_selection_sort.rplpp"
          [ ("a[0]", 10); ("a[5]", 60); ("ftab[0]", 4); ("ftab[2]", 3) ]);

      "algo_quick_sort.rplpp" >:: (fun _ ->
        assert_values "algo_quick_sort.rplpp"
          [ ("a[0]", 1); ("a[7]", 8); ("ord[0]", 3); ("ord[7]", 1) ]);

      "DynamicArray.rplpp" >:: (fun _ ->
        assert_values "DynamicArray.rplpp"
          [ ("len", 3); ("cap", 8); ("q", 30); ("x1", 50); ("x2", 40) ]);

      "BinaryHeap.rplpp" >:: (fun _ ->
        assert_values "BinaryHeap.rplpp"
          [ ("min1", 1); ("min2", 2); ("min3", 3); ("size", 4) ]);

      "TreeSort.rplpp" >:: (fun _ ->
        assert_values "TreeSort.rplpp"
          [ ("out[0]", 1); ("out[3]", 5); ("out[6]", 9) ]);
    ]

let _ = run_test_tt_main suite
