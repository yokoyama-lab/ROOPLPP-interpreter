open OUnit
open Syntax
open Value
open Eval

let tests = "test suite for sum" >::: [
      "skip"    >:: (fun _ ->
        assert_equal [] (eval_state [Skip] [] [] []) );
      "x += x"    >:: (fun _ ->
        assert_equal [(1, IntVal 2)] (eval_state [Assign(("x", None), ModAdd, Var "x")] [("x", 1)] [] [(1, IntVal 1)]) );
      "x += 1"    >:: (fun _ ->
        assert_equal [(1, IntVal 2)] (eval_state [Assign(("x", None), ModAdd, Const 1)] [("x", 1)] [] [(1, IntVal 1)]) );
      "x -= 1"    >:: (fun _ ->
        assert_equal [(1, IntVal 0)] (eval_state [Assign(("x", None), ModSub, Const 1)] [("x", 1)] [] [(1, IntVal 1)]) );
      "x ^= 1"    >:: (fun _ ->
        assert_equal [(1, IntVal 1)] (eval_state [Assign(("x", None), ModXor, Const 0)] [("x", 1)] [] [(1, IntVal 1)]) );
]

let _ = run_test_tt_main tests
