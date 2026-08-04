open OUnit2
open Value
open Print

(* issue #39: the previous tests were of the form
     assert_equal (print_endline "...") (print_result [...])
   Both sides evaluate to unit (the functions print as a side effect and
   return ()), so every test reduced to assert_equal () () and passed no
   matter what was printed -- the output content was never checked.

   These tests assert on the strings the printers actually produce, using
   show_val / show_vec / show_val_rec and the string_of_result seam extracted
   from print_result, so a wrong rendering now fails. *)

let str = "test suite for print.ml" >::: [
      "show_val IntVal" >:: (fun _ ->
        assert_equal ~printer:(fun x -> x) "1" (show_val (IntVal 1)));

      "show_val LocsVal" >:: (fun _ ->
        assert_equal ~printer:(fun x -> x) "<location> 1" (show_val (LocsVal 1)));

      "show_vec" >:: (fun _ ->
        assert_equal ~printer:(fun x -> x) "[123]" (show_vec [1; 2; 3]));

      "show_val LocsVec" >:: (fun _ ->
        assert_equal ~printer:(fun x -> x) "[123]" (show_val (LocsVec [1; 2; 3])));

      "show_val_rec IntVal" >:: (fun _ ->
        assert_equal ~printer:(fun x -> x) "<int> 5" (show_val_rec (IntVal 5)));

      "show_val_rec ObjVal" >:: (fun _ ->
        assert_equal ~printer:(fun x -> x) "<object> C{; a:1; b:2}"
          (show_val_rec (ObjVal ("C", [("a", 1); ("b", 2)]))));

      "show_val_rec ObjVal empty" >:: (fun _ ->
        assert_equal ~printer:(fun x -> x) "<object> C{}"
          (show_val_rec (ObjVal ("C", []))));

      "show_val_rec LocsVal" >:: (fun _ ->
        assert_equal ~printer:(fun x -> x) "<location> 7" (show_val_rec (LocsVal 7)));

      "show_val_rec LocsVec" >:: (fun _ ->
        assert_equal ~printer:(fun x -> x) "<vector>  1 2 3" (show_val_rec (LocsVec [1; 2; 3])));

      (* print_result's full rendering: "id = value\n" per binding *)
      "string_of_result single int" >:: (fun _ ->
        assert_equal ~printer:(fun x -> x) "result = 1\n"
          (string_of_result [("result", IntVal 1)]));

      "string_of_result location" >:: (fun _ ->
        assert_equal ~printer:(fun x -> x) "x = <location> 1\n"
          (string_of_result [("x", LocsVal 1)]));

      "string_of_result three lines" >:: (fun _ ->
        assert_equal ~printer:(fun x -> x) "xs[0] = 1\nxs[1] = 2\nxs[2] = 3\n"
          (string_of_result [("xs[0]", IntVal 1); ("xs[1]", IntVal 2); ("xs[2]", IntVal 3)]));

      "string_of_result empty" >:: (fun _ ->
        assert_equal ~printer:(fun x -> x) "" (string_of_result []));
    ]

let _ = run_test_tt_main str
