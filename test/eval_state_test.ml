open OUnit2
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
      "x[0] + 1"    >:: (fun _ ->
        assert_equal [(1, LocsVec[2; 3]); (2, IntVal 1); (3, IntVal 0)]
          (eval_state [Assign(("x", Some(Const 0)), ModAdd, Const 1)] [("x", 1)] [] [(1, LocsVec[2; 3]); (2, IntVal 0); (3, IntVal 0)]) );
      "x[0] - 1"    >:: (fun _ ->
        assert_equal [(1, LocsVec[2; 3]); (2, IntVal (-1)); (3, IntVal 0)]
          (eval_state [Assign(("x", Some(Const 0)), ModSub, Const 1)] [("x", 1)] [] [(1, LocsVec[2; 3]); (2, IntVal 0); (3, IntVal 0)]) );
      "x[0] ^ 1"    >:: (fun _ ->
        assert_equal [(1, LocsVec[2; 3]); (2, IntVal 1); (3, IntVal 0)]
          (eval_state [Assign(("x", Some(Const 0)), ModXor, Const 1)] [("x", 1)] [] [(1, LocsVec[2; 3]); (2, IntVal 0); (3, IntVal 0)]) );
      "x <=> y"    >:: (fun _ ->
        assert_equal [(1, IntVal 5); (2, IntVal 10)]
          (eval_state [Swap(("x", None), ("y", None))] [("x", 1); ("y", 2)] [] [(1, IntVal 10); (2, IntVal 5)]) );
      "x[0] <=> x[1]"    >:: (fun _ ->
        assert_equal [(1, LocsVec[2; 3]); (2, IntVal 100); (3, IntVal 10)]
          (eval_state [Swap(("x", Some(Const 0)), ("x", Some(Const 1)))] [("x", 1)] [] [(1, LocsVec[2; 3]); (2, IntVal 10); (3, IntVal 100)]) );
      "from x = 0 do skip loop x += 1 until x = 10"    >:: (fun _ ->
        assert_equal [(1, IntVal 10)]
          (eval_state [Loop(Binary(Eq, Var "x", Const 0), [Skip], [Assign(("x", None), ModAdd, Const 1)], Binary(Eq, Var "x", Const 10))] [("x", 1)] [] [(1, IntVal 0)]) );

      "from x = 0 do x += 1 loop skip until x = 10"    >:: (fun _ ->
        assert_equal [(1, IntVal 10)]
          (eval_state [Loop(Binary(Eq, Var "x", Const 0), [Assign(("x", None), ModAdd, Const 1)], [Skip], Binary(Eq, Var "x", Const 10))] [("x", 1)] [] [(1, IntVal 0)]) );

      "if x = 0 then x += 1 else x -= 1 fi x = 1"    >:: (fun _ ->
        assert_equal [(1, IntVal 1)]
          (eval_state [Conditional(Binary(Eq, Var "x", Const 0), [Assign(("x", None), ModAdd, Const 1)], [Assign(("x", None), ModSub, Const 1)], Binary(Eq, Var "x", Const 1))] [("x", 1)] [] [(1, IntVal 0)]) );

      "if x = 0 then x += 1 else x -= 1 fi x = 1(true)"    >:: (fun _ ->
        assert_equal [(1, IntVal 0)]
          (eval_state [Conditional(Binary(Eq, Var "x", Const 0), [Assign(("x", None), ModAdd, Const 1)], [Assign(("x", None), ModSub, Const 1)], Binary(Eq, Var "x", Const 1))] [("x", 1)] [] [(1, IntVal 1)]) );      
  ]

let _ = run_test_tt_main tests
