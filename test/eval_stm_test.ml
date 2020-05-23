open OUnit
open Syntax
open Value
open Eval

let tests = "test suite for sum" >::: [
      "skip"    >:: (fun _ ->
        assert_equal [] (eval_state [Skip] [] [] []) );
      "x +="    >:: (fun _ ->
        assert_equal [] (eval_state [Skip] [] [] []) );

]

let _ = run_test_tt_main tests
