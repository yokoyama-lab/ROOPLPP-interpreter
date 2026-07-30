open OUnit2
open Syntax
open Value
open Diagnostics

(* diagnostics.ml のテスト。

   eval.ml が failwith / Util.Runtime_error に積む生メッセージは
     <文や式の pretty 表示（外側から内側へ）>
     ERROR:<本体> in this statement in this expression ...
     WHERE:<変数 = 値; ...>
   という形をしている。ここではその分解・修正ヒント・整形・
   終了時のゼロクリア検査を、文字列として検査する。 *)

let s = String.concat "\n"

(* 実際に eval.ml が作る形の生メッセージ *)
let raw_oob =
  s [ "total += a[i]";
      "a[i]";
      "ERROR:Array index a[4] is out of bounds in this statement in this expression";
      "WHERE:total = 6; i = 4" ]

let src_oob =
  s [ "class Program";
      "    int[] a";
      "    int total";
      "    method main()";
      "        new int[4] a";
      "        for i in (0..4) do";
      "            total += a[i]";
      "        end" ]

let contains_sub ~needle hay = Diagnostics.contains ~needle hay

(* ---- 位置情報つきの行報告 --------------------------------------------

   AST が位置を持つので、同じ文字列の文が複数行にあっても実際に落ちた行を
   一意に言える（持つ前は「候補」を並べるしかなかった）。 *)

let parse src = Parser.main Lexer.token (Lexing.from_string src)

let run_and_format src =
  match ignore (Eval.eval_prog (parse src)) with
  | () -> None
  | exception (Util.Runtime_error e | Failure e) ->
     Some (Diagnostics.format_runtime_error ~src e)

(* 同じ文 "x += 1 / y" が 6 行目と 8 行目にあり、落ちるのは 8 行目 *)
let src_ambiguous =
  s [ "class Program";
      "    int x";
      "    int y";
      "    method main()";
      "        if y = 1 then";
      "            x += 1 / y";
      "        else";
      "            x += 1 / y";
      "        fi x = 1" ]

(* 式の中で落ちる：a[4] は範囲外（7 行目、21〜25 桁） *)
let src_oob_run =
  s [ "class Program";
      "    int[] a";
      "    int total";
      "    method main()";
      "        new int[4] a";
      "        for i in (0..4) do";
      "            total += a[i]";
      "        end" ]

(* 式の中で落ちる：1 / y のゼロ除算 *)
let src_div =
  s [ "class Program";
      "    int x";
      "    int y";
      "    method main()";
      "        x += 1 / y" ]

(* 入れ子（for の中の if の中）で落ちる。落ちるのは 8 行目 *)
let src_nested =
  s [ "class Program";
      "    int x";
      "    int y";
      "    method main()";
      "        for i in (0..2) do";
      "            if i = 1 then";
      "                x += 1";
      "                x += 1 / y";
      "            else";
      "                skip";
      "            fi i = 1";
      "        end" ]

let suite = "test suite for diagnostics.ml" >::: [
      (* ---- 位置情報つきの行報告 -------------------------------------- *)
      "the reported line is exact even when the statement text repeats"
      >:: (fun _ ->
        match run_and_format src_ambiguous with
        | None -> assert_failure "expected a runtime error"
        | Some out ->
           assert_bool ("expected line 8 in:\n" ^ out)
             (contains_sub ~needle:"line: 8" out);
           assert_bool ("did not expect candidates in:\n" ^ out)
             (not (contains_sub ~needle:"candidates" out)));

      "the reported line is exact for a nested statement" >:: (fun _ ->
        match run_and_format src_nested with
        | None -> assert_failure "expected a runtime error"
        | Some out ->
           assert_bool ("expected line 8 in:\n" ^ out)
             (contains_sub ~needle:"line: 8" out));

      (* 式にも位置があるので、落ちた部分式にキャレットを引ける *)
      "a failure inside an expression gets a caret under it" >:: (fun _ ->
        match run_and_format src_oob_run with
        | None -> assert_failure "expected a runtime error"
        | Some out ->
           assert_bool ("expected a column range in:\n" ^ out)
             (contains_sub ~needle:"line: 7, columns 21-25" out);
           (* キャレットは a[i] の 4 文字分 *)
           assert_bool ("expected a caret span in:\n" ^ out)
             (contains_sub ~needle:"^^^^" out));

      "the caret covers the failing sub-expression, not the whole statement"
      >:: (fun _ ->
        match run_and_format src_div with
        | None -> assert_failure "expected a runtime error"
        | Some out ->
           assert_bool ("expected 1 / y to be underlined in:\n" ^ out)
             (contains_sub ~needle:"^^^^^" out);
           assert_bool ("did not expect the whole line underlined in:\n" ^ out)
             (not (contains_sub ~needle:"^^^^^^^^^^" out)));

      "the position marker does not leak into the message" >:: (fun _ ->
        match run_and_format src_ambiguous with
        | None -> assert_failure "expected a runtime error"
        | Some out ->
           assert_bool ("marker leaked into:\n" ^ out)
             (not (contains_sub ~needle:"AT:" out)));

      (* ---- 生メッセージの分解 ---------------------------------------- *)
      "split_runtime_message: trace" >:: (fun _ ->
        let trace, _, _ = split_runtime_message raw_oob in
        assert_equal ~printer:(String.concat " | ") [ "total += a[i]"; "a[i]" ] trace);

      "split_runtime_message: core message drops ERROR: and context suffixes" >:: (fun _ ->
        let _, msg, _ = split_runtime_message raw_oob in
        assert_equal ~printer:(fun x -> x) "Array index a[4] is out of bounds" msg);

      "split_runtime_message: WHERE line" >:: (fun _ ->
        let _, _, where = split_runtime_message raw_oob in
        assert_equal ~printer:(String.concat " | ") [ "total = 6; i = 4" ] where);

      "split_runtime_message: no ERROR marker keeps the text as the message" >:: (fun _ ->
        let _, msg, _ = split_runtime_message "something went wrong" in
        assert_equal ~printer:(fun x -> x) "something went wrong" msg);

      "split_runtime_message: empty WHERE is dropped" >:: (fun _ ->
        let _, _, where = split_runtime_message (s [ "ERROR:boom"; "WHERE:" ]) in
        assert_equal ~printer:string_of_int 0 (List.length where));

      "strip_context removes repeated suffixes" >:: (fun _ ->
        assert_equal ~printer:(fun x -> x) "boom"
          (strip_context "boom in this statement in this expression in this expression"));

      "has_where" >:: (fun _ ->
        assert_equal true (has_where raw_oob);
        assert_equal false (has_where "ERROR:boom"));

      (* ---- 行の推定 -------------------------------------------------- *)
      "contains_at_boundary: local does not match delocal" >:: (fun _ ->
        assert_equal true (contains_at_boundary ~needle:"localintt=0" "localintt=0");
        assert_equal false (contains_at_boundary ~needle:"localintt=0" "delocalintt=0"));

      "candidate_lines finds the statement line" >:: (fun _ ->
        assert_equal ~printer:(fun l -> String.concat "," (List.map string_of_int l))
          [ 7 ] (candidate_lines src_oob "total += a[i]"));

      "candidate_lines ignores added parentheses and spacing" >:: (fun _ ->
        assert_equal ~printer:(fun l -> String.concat "," (List.map string_of_int l))
          [ 6 ] (candidate_lines src_oob "for i in ( 0 .. 4 ) do"));

      (* ---- 修正ヒント ------------------------------------------------ *)
      "fix_hints: out of bounds mentions new int[n]" >:: (fun _ ->
        let hints = fix_hints "Array index a[4] is out of bounds" in
        assert_bool "mentions new int[n]"
          (List.exists (contains_sub ~needle:"allocates") hints));

      "fix_hints: delete needs zero-cleared elements" >:: (fun _ ->
        let hints = fix_hints "All array elements is not zero-cleared" in
        assert_bool "mentions delete"
          (List.exists (contains_sub ~needle:"`delete int[n] xs`") hints));

      "fix_hints: delocal mismatch" >:: (fun _ ->
        let hints = fix_hints "Variable t = 3, But it should be 0" in
        assert_bool "mentions delocal"
          (List.exists (contains_sub ~needle:"delocal") hints));

      "fix_hints: value argument" >:: (fun _ ->
        let hints = fix_hints "formal argument and actual argument are not same value" in
        assert_bool "mentions passing the array and the index"
          (List.exists (contains_sub ~needle:"index separately") hints));

      "fix_hints: unknown message still gets a generic hint" >:: (fun _ ->
        assert_bool "non-empty" (fix_hints "totally unknown failure" <> []));

      (* ---- 実行時エラーの整形 ---------------------------------------- *)
      "format_runtime_error: has all sections" >:: (fun _ ->
        let out = format_runtime_error ~src:src_oob ~file:"p.rplpp" raw_oob in
        List.iter
          (fun needle ->
            assert_bool ("contains " ^ needle) (contains_sub ~needle out))
          [ "ROOPL++ execution error";
            "message: Array index a[4] is out of bounds";
            "file: p.rplpp";
            "line: 7";
            "Source:";
            "> 7 |";
            "Trace (outermost first):";
            "Values on entry to this statement:";
            "total = 6; i = 4";
            "Fix hints:" ]);

      "format_runtime_error: works without a source file" >:: (fun _ ->
        let out = format_runtime_error raw_oob in
        assert_bool "still reports the message"
          (contains_sub ~needle:"Array index a[4] is out of bounds" out);
        assert_bool "no Source section" (not (contains_sub ~needle:"Source:" out)));

      (* ---- 構文エラーの整形 ------------------------------------------ *)
      "format_parse_error: excerpt with caret" >:: (fun _ ->
        let pos line bol cnum : Lexing.position =
          { pos_fname = "p.rplpp"; pos_lnum = line; pos_bol = bol; pos_cnum = cnum }
        in
        let src = s [ "class Program"; "    int x"; "    method main()"; "        x = 1" ] in
        let out = format_parse_error ~src ~file:"p.rplpp" (pos 4 40 50) (pos 4 40 51) in
        List.iter
          (fun needle -> assert_bool ("contains " ^ needle) (contains_sub ~needle out))
          [ "ROOPL++ parse error"; "file: p.rplpp"; "line 4, column 10"; "^"; "Fix hints:" ]);

      (* ---- 文に現れる識別子 ------------------------------------------ *)
      "ids_of_stm: assignment" >:: (fun _ ->
        assert_equal ~printer:(String.concat ",") [ "total"; "a"; "i" ]
          (ids_of_stm (Assign (VarArray ("total", None), ModAdd,
                               ArrayElement ("a", Var "i")))));

      "ids_of_stm: loop looks at the guards only" >:: (fun _ ->
        assert_equal ~printer:(String.concat ",") [ "i"; "n" ]
          (ids_of_stm (Loop (Var "i", [ Skip ], [ Skip ], Var "n"))));

      (* ---- 終了時のゼロクリア検査 ------------------------------------ *)
      "garbage_report: clean program" >:: (fun _ ->
        let out = garbage_report [ ("x", IntVal 0); ("y", IntVal 0) ] [ (1, IntVal 0) ] in
        assert_equal ~printer:(fun x -> x)
          "ROOPL++ zero-clear check: all 2 value(s) are zero-cleared (no garbage left)." out);

      "garbage_report: lists the non-zero values" >:: (fun _ ->
        let out = garbage_report [ ("x", IntVal 0); ("y", IntVal 3) ] [] in
        assert_bool "counts" (contains_sub ~needle:"1 of 2 value(s) are NOT zero-cleared" out);
        assert_bool "shows y" (contains_sub ~needle:"y = 3" out));

      "garbage_report: expands an object that was never deleted" >:: (fun _ ->
        let st = [ (5, ObjVal ("Counter", [ ("this", 5); ("n", 6) ])); (6, IntVal 7) ] in
        let out = garbage_report [ ("c", LocsVal 5) ] st in
        assert_bool "names the class" (contains_sub ~needle:"<Counter @5>" out);
        assert_bool "shows the field" (contains_sub ~needle:"n = 7" out));

      "garbage_report: expands an array that was never deleted" >:: (fun _ ->
        let st = [ (2, LocsVec [ 3; 4 ]); (3, IntVal 0); (4, IntVal 9) ] in
        let out = garbage_report [ ("xs", LocsVec [ 3; 4 ]) ] st in
        assert_bool "shows the element" (contains_sub ~needle:"[1] = 9" out));

      "garbage_report: cycles do not loop forever" >:: (fun _ ->
        let st =
          [ (1, ObjVal ("Node", [ ("next", 2) ])); (2, LocsVal 1) ]
        in
        let out = garbage_report [ ("head", LocsVal 1) ] st in
        assert_bool "mentions the repeat" (contains_sub ~needle:"already shown" out));
    ]

let _ = run_test_tt_main suite
