open OUnit
open Syntax
open Value
open Eval

let tests = "test suite for sum" >::: [
      "Const 1"  >:: (fun _ -> assert_equal (IntVal 1) (eval_exp (Const 1) [] []) );
      "Var x"    >:: (fun _ -> assert_equal (IntVal 1) (eval_exp (Var "x") [("x", 1)] [(1, IntVal 1)]) );
      "x[0]"     >:: (fun _ ->
        assert_equal (IntVal 3) (eval_exp (ArrayElement("x", Const 0))
                                   [("x", 1)]
                                   [(1, LocsVec [2; 3; 4]); (2, IntVal 3); (3, IntVal 1); (4, IntVal 0)]) );
      "x[1]"     >:: (fun _ ->
        assert_equal (IntVal 1) (eval_exp (ArrayElement("x", Const 1))
                                   [("x", 1)]
                                   [(1, LocsVec [2; 3; 4]); (2, IntVal 3); (3, IntVal 1); (4, IntVal 0)]) );
      "x[2]"     >:: (fun _ ->
        assert_equal (IntVal 0) (eval_exp (ArrayElement("x", Const 2))
                                   [("x", 1)]
                                   [(1, LocsVec [2; 3; 4]); (2, IntVal 3); (3, IntVal 1); (4, IntVal 0)]) );
      "Nil"      >:: (fun _ ->
        assert_equal (IntVal 0) (eval_exp Nil [] []) );
      "1 + 1"    >:: (fun _ ->
        assert_equal (IntVal 2) (eval_exp (Binary(Add, Const 1, Const 1)) [] []) );
      "x + x"    >:: (fun _ ->
        assert_equal (IntVal 4) (eval_exp (Binary(Add, Var "x", Var "x")) [("x", 1)] [(1, IntVal 2)]) );

      "1 - 1"    >:: (fun _ ->
                      assert_equal (IntVal 0) (eval_exp (Binary(Sub, Const 1, Const 1)) [] []) );
      "1 ^ 1"    >:: (fun _ ->
                      assert_equal (IntVal 0) (eval_exp (Binary(Xor, Const 1, Const 1)) [] []) );
      "1 * 1"    >:: (fun _ ->
        assert_equal (IntVal 1) (eval_exp (Binary(Mul, Const 1, Const 1)) [] []) );
      "1 / 1"    >:: (fun _ ->
        assert_equal (IntVal 1) (eval_exp (Binary(Div, Const 1, Const 1)) [] []) );
      "1 % 1"    >:: (fun _ ->
        assert_equal (IntVal 0) (eval_exp (Binary(Mod, Const 1, Const 1)) [] []) );
      "10 & 11"    >:: (fun _ ->
        assert_equal (IntVal 10) (eval_exp (Binary(Band, Const 10, Const 11)) [] []) );
      "1 | 0"    >:: (fun _ ->
        assert_equal (IntVal 15) (eval_exp (Binary(Bor, Const 10, Const 5)) [] []) );
      "1 && 1"    >:: (fun _ ->
        assert_equal (IntVal 1) (eval_exp (Binary(And, Const 1, Const 1)) [] []) );
      "1 && 0"    >:: (fun _ ->
        assert_equal (IntVal 0) (eval_exp (Binary(And, Const 1, Const 0)) [] []) );
      "1 || 0"    >:: (fun _ ->
        assert_equal (IntVal 1) (eval_exp (Binary(Or, Const 1, Const 0)) [] []) );
      "1 || 1"    >:: (fun _ ->
        assert_equal (IntVal 0) (eval_exp (Binary(Or, Const 0, Const 0)) [] []) );
      "10 < 5"    >:: (fun _ ->
        assert_equal (IntVal 0) (eval_exp (Binary(Lt, Const 10, Const 5)) [] []) );
      "5 < 10"    >:: (fun _ ->
        assert_equal (IntVal 1) (eval_exp (Binary(Lt, Const 5, Const 10)) [] []) );
      "1 < 1"    >:: (fun _ ->
        assert_equal (IntVal 0) (eval_exp (Binary(Lt, Const 1, Const 1)) [] []) );
      "1 > 2"    >:: (fun _ ->
        assert_equal (IntVal 0) (eval_exp (Binary(Gt, Const 1, Const 2)) [] []) );
      "2 > 1"    >:: (fun _ ->
        assert_equal (IntVal 1) (eval_exp (Binary(Gt, Const 2, Const 1)) [] []) );
      "1 > 1"    >:: (fun _ ->
        assert_equal (IntVal 0) (eval_exp (Binary(Gt, Const 1, Const 1)) [] []) );
      "1 = 1"    >:: (fun _ ->
        assert_equal (IntVal 1) (eval_exp (Binary(Eq, Const 1, Const 1)) [] []) );
      "1 = 0"    >:: (fun _ ->
        assert_equal (IntVal 0) (eval_exp (Binary(Eq, Const 1, Const 0)) [] []) );
      "1 <> 1"    >:: (fun _ ->
        assert_equal (IntVal 0) (eval_exp (Binary(Ne, Const 1, Const 1)) [] []) );
      "1 <> 0"    >:: (fun _ ->
        assert_equal (IntVal 1) (eval_exp (Binary(Ne, Const 1, Const 0)) [] []) );
      "1 <= 1"    >:: (fun _ ->
        assert_equal (IntVal 1) (eval_exp (Binary(Le, Const 1, Const 1)) [] []) );
      "2 <= 1"    >:: (fun _ ->
        assert_equal (IntVal 0) (eval_exp (Binary(Le, Const 2, Const 1)) [] []) );
      "1 <= 2"    >:: (fun _ ->
        assert_equal (IntVal 1) (eval_exp (Binary(Le, Const 1, Const 2)) [] []) );
      "1 >= 1"    >:: (fun _ ->
        assert_equal (IntVal 1) (eval_exp (Binary(Ge, Const 1, Const 1)) [] []) );
      "2 >= 1"    >:: (fun _ ->
        assert_equal (IntVal 1) (eval_exp (Binary(Ge, Const 2, Const 1)) [] []) );
      "1 >= 2"    >:: (fun _ ->
        assert_equal (IntVal 0) (eval_exp (Binary(Ge, Const 1, Const 2)) [] []) );
]

let _ = run_test_tt_main tests
