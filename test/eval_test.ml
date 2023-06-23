open OUnit2
open Syntax
open Value
open Eval
open Pretty
   
let tests = "test suite for eval.ml" >::: [
      "Const 1"  >:: (fun _ -> assert_equal (IntVal 1) (eval_exp (Const 1) [] []) );
      "Var x"    >:: (fun _ -> assert_equal (IntVal 1) (eval_exp (Var "x") ["x", 1] [1, IntVal 1]) );
      "x[0]"     >:: (fun _ ->
        assert_equal (IntVal 3) (eval_exp (ArrayElement("x", Const 0))
                                   ["x", 1]
                                   [1, LocsVec [2; 3; 4]; 2, IntVal 3; 3, IntVal 1; 4, IntVal 0]) );
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
      "x - 1"    >:: (fun _ ->
                      assert_equal (IntVal 2) (eval_exp (Binary(Sub, Var "x", Const 1)) [("x" , 1)] [(1, IntVal 3)]) );                
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
      "LocsVal 1 < LovsVal 2"  >:: (fun _ ->
        assert_equal (IntVal 1) (comp_op (<) (LocsVal 1) (LocsVal 2)) );
      "LocsVal 1 < IntVal 2"  >:: (fun _ ->
        assert_equal (IntVal 0) (comp_op (<) (LocsVal 1) (IntVal 2)) );      
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
      "1 = nil"    >:: (fun _ ->
        assert_equal (IntVal 1) (eval_exp (Binary(Eq, Const 0, Nil)) [] []) );
      "0 = nil"    >:: (fun _ ->
        assert_equal (IntVal 0) (eval_exp (Binary(Eq, Const 1, Nil)) [] []) );
      "LocsVal 1 = LovsVal 1"  >:: (fun _ ->
        assert_equal (IntVal 1) (comp_op (=) (LocsVal 1) (LocsVal 1)) );
      "LocsVal 1 = IntVal 1"  >:: (fun _ ->
        assert_equal (IntVal 0) (comp_op (=) (LocsVal 1) (IntVal 1)) );
      "ObjVal(this, []) = IntVal 1"  >:: (fun _ ->
        assert_equal (IntVal 0) (comp_op (=) (ObjVal("this", [])) (IntVal 1)) );
      "ObjVal(this, []) = ObjVal(this, [])"  >:: (fun _ ->
        assert_equal (IntVal 1) (comp_op (=) (ObjVal("this", [])) (ObjVal("this", []))) );
      "1 <> 1"    >:: (fun _ ->
        assert_equal (IntVal 0) (eval_exp (Binary(Ne, Const 1, Const 1)) [] []) );
      "1 <> 0"    >:: (fun _ ->
        assert_equal (IntVal 1) (eval_exp (Binary(Ne, Const 1, Const 0)) [] []) );
      "LocsVal 0 <> LovsVal 1"  >:: (fun _ ->
        assert_equal (IntVal 1) (comp_op (<>) (LocsVal 0) (LocsVal 1)) );
      "IntVal 1 <> LovsVal 1"  >:: (fun _ ->
        assert_equal (IntVal 1) (comp_op (<>) (IntVal 1) (LocsVal 1)) );      
      "1 <= 1"    >:: (fun _ ->
        assert_equal (IntVal 1) (eval_exp (Binary(Le, Const 1, Const 1)) [] []) );
      "2 <= 1"    >:: (fun _ ->
        assert_equal (IntVal 0) (eval_exp (Binary(Le, Const 2, Const 1)) [] []) );
      "1 <= 2"    >:: (fun _ ->
        assert_equal (IntVal 1) (eval_exp (Binary(Le, Const 1, Const 2)) [] []) );
      "LocsVal 2 <= LocsVal 2"    >:: (fun _ ->
        assert_equal (IntVal 1) (comp_op (<=) (LocsVal 1) (LocsVal 1)) );
      "1 >= 1"    >:: (fun _ ->
        assert_equal (IntVal 1) (eval_exp (Binary(Ge, Const 1, Const 1)) [] []) );
      "2 >= 1"    >:: (fun _ ->
        assert_equal (IntVal 1) (eval_exp (Binary(Ge, Const 2, Const 1)) [] []) );
      "1 >= 2"    >:: (fun _ ->
        assert_equal (IntVal 0) (eval_exp (Binary(Ge, Const 1, Const 2)) [] []) );
      "LocsVal 2 >= LocsVal 2"    >:: (fun _ ->
        assert_equal (IntVal 1) (comp_op (>=) (LocsVal 1) (LocsVal 1)) );
      "t.a"      >:: (fun _ ->
        assert_equal (IntVal 10) (eval_exp (Dot (Var "t", Var "a")) 
          [("t", 1); ("this", 2)] [(1, LocsVal 4); (2, LocsVal 3);
          (3, ObjVal ("Programs", [("t", 1); ("this", 2)])); (4, ObjVal ("Test", [("t2", 5); ("a", 6); ("b", 7)])); (5, IntVal 0);
          (6, IntVal 10); (7, IntVal 0)]) );
      "t.t2.a"      >:: (fun _ ->
        assert_equal (IntVal 100) (eval_exp (Dot (Dot (Var "t", Var "t2"), Var "a")) 
          [("t", 1); ("this", 2)] [(1, LocsVal 4); (2, LocsVal 3); (3, ObjVal ("Programs", [("t", 1); ("this", 2)]));
          (4, ObjVal ("Test", [("t2", 5); ("a", 6); ("b", 7)])); (5, LocsVal 8);
          (6, IntVal 0); (7, IntVal 0); (8, ObjVal ("Test", [("t2", 9); ("a", 10); ("b", 11)])); (9, IntVal 0);
          (10, IntVal 100); (11, IntVal 0)]) );

      
      "skip"    >:: (fun _ ->
        assert_equal [] (eval_state [Skip] [] [] []) );
      "x += x"    >:: (fun _ ->
        assert_equal [(1, IntVal 2)] (eval_state [Assign(VarArray("x", None), ModAdd, Var "x")] [("x", 1)] [] [(1, IntVal 1)]) );
      "x += 1"    >:: (fun _ ->
        assert_equal [(1, IntVal 2)] (eval_state [Assign(VarArray("x", None), ModAdd, Const 1)] [("x", 1)] [] [(1, IntVal 1)]) );
      "x -= 1"    >:: (fun _ ->
        assert_equal [(1, IntVal 0)] (eval_state [Assign(VarArray("x", None), ModSub, Const 1)] [("x", 1)] [] [(1, IntVal 1)]) );
      "x ^= 1"    >:: (fun _ ->
        assert_equal [(1, IntVal 1)] (eval_state [Assign(VarArray("x", None), ModXor, Const 0)] [("x", 1)] [] [(1, IntVal 1)]) );
      "x[0] += 1"    >:: (fun _ ->
        assert_equal [(1, LocsVec[2; 3]); (2, IntVal 1); (3, IntVal 0)]
          (eval_state [Assign(VarArray("x", Some(Const 0)), ModAdd, Const 1)] [("x", 1)] [] [(1, LocsVec[2; 3]); (2, IntVal 0); (3, IntVal 0)]) );
      "x[0] -= 1"    >:: (fun _ ->
        assert_equal [(1, LocsVec[2; 3]); (2, IntVal (-1)); (3, IntVal 0)]
          (eval_state [Assign(VarArray("x", Some(Const 0)), ModSub, Const 1)] [("x", 1)] [] [(1, LocsVec[2; 3]); (2, IntVal 0); (3, IntVal 0)]) );
      "x[0] ^= 1"    >:: (fun _ ->
        assert_equal [(1, LocsVec[2; 3]); (2, IntVal 1); (3, IntVal 0)]
          (eval_state [Assign(VarArray("x", Some(Const 0)), ModXor, Const 1)] [("x", 1)] [] [(1, LocsVec[2; 3]); (2, IntVal 0); (3, IntVal 0)]) );
      "t.a ^= 10"    >:: (fun _ ->
        assert_equal [(1, LocsVal 4); (2, LocsVal 3);
        (3, ObjVal ("Programs", [("t", 1); ("this", 2)])); (4, ObjVal ("Test", [("t2", 5); ("a", 6); ("b", 7)])); (5, IntVal 0);
        (6, IntVal 10); (7, IntVal 0)]
          (eval_state [Assign (InstVar (VarArray ("t", None), VarArray ("a", None)), ModXor,
                               Const 10)  ] [("t", 1); ("this", 2)] [] 
                               [(1, LocsVal 4); (2, LocsVal 3); (3, ObjVal ("Programs", [("t", 1); ("this", 2)]));
                               (4, ObjVal ("Test", [("t2", 5); ("a", 6); ("b", 7)])); (5, IntVal 0); (6, IntVal 0); (7, IntVal 0)]) );
      "t.a.a ^= 100"    >:: (fun _ ->
          assert_equal [(1, LocsVal 4); (2, LocsVal 3); (3, ObjVal ("Programs", [("t", 1); ("this", 2)]));
          (4, ObjVal ("Test", [("t2", 5); ("a", 6); ("b", 7)])); (5, LocsVal 8);
          (6, IntVal 0); (7, IntVal 0); (8, ObjVal ("Test", [("t2", 9); ("a", 10); ("b", 11)])); (9, IntVal 0);
          (10, IntVal 100); (11, IntVal 0)]
          (eval_state [Assign (InstVar (InstVar (VarArray ("t", None), VarArray ("t2", None)), VarArray ("a", None)), ModXor, Const 100)] 
           [("t", 1); ("this", 2)] [] 
           [(1, LocsVal 4); (2, LocsVal 3); (3, ObjVal ("Programs", [("t", 1); ("this", 2)]));
           (4, ObjVal ("Test", [("t2", 5); ("a", 6); ("b", 7)])); (5, LocsVal 8); (6, IntVal 0); (7, IntVal 0);
           (8, ObjVal ("Test", [("t2", 9); ("a", 10); ("b", 11)])); (9, IntVal 0); (10, IntVal 0); (11, IntVal 0)]) );
      "x <=> y"    >:: (fun _ ->
        assert_equal [(1, IntVal 5); (2, IntVal 10)]
          (eval_state [Swap(VarArray("x", None), VarArray("y", None))] [("x", 1); ("y", 2)] [] [(1, IntVal 10); (2, IntVal 5)]) );
      "x[0] <=> x[1]"    >:: (fun _ ->
        assert_equal [(1, LocsVec[2; 3]); (2, IntVal 100); (3, IntVal 10)]
          (eval_state [Swap(VarArray("x", Some(Const 0)), VarArray("x", Some(Const 1)))] [("x", 1)] [] 
           [(1, LocsVec[2; 3]); (2, IntVal 10); (3, IntVal 100)]) );
      "if x = 0 then x += 1 else x -= 1 fi x = 1"    >:: (fun _ ->
        assert_equal [(1, IntVal 1)]
          (eval_state [Conditional(Binary(Eq, Var "x", Const 0), [Assign(VarArray("x", None), ModAdd, Const 1)], [Assign(VarArray("x", None), ModSub, Const 1)], Binary(Eq, Var "x", Const 1))] [("x", 1)] [] [(1, IntVal 0)]) );
      "if x = 0 then x += 1 else x -= 1 fi x = 1(true)"    >:: (fun _ ->
        assert_equal [(1, IntVal 0)]
          (eval_state [Conditional(Binary(Eq, Var "x", Const 0), [Assign(VarArray("x", None), ModAdd, Const 1)], [Assign(VarArray("x", None), ModSub, Const 1)], Binary(Eq, Var "x", Const 1))] [("x", 1)] [] [(1, IntVal 1)]) );      
      "from x = 0 do skip loop x += 1 until x = 10"    >:: (fun _ ->
        assert_equal [(1, IntVal 10)]
          (eval_state [Loop(Binary(Eq, Var "x", Const 0), [Skip], [Assign(VarArray("x", None), ModAdd, Const 1)], Binary(Eq, Var "x", Const 10))] [("x", 1)] [] [(1, IntVal 0)]) );
      "from x = 0 do x += 1 loop skip until x = 10"    >:: (fun _ ->
        assert_equal [(1, IntVal 10)]
          (eval_state [Loop(Binary(Eq, Var "x", Const 0), [Assign(VarArray("x", None), ModAdd, Const 1)], [Skip], Binary(Eq, Var "x", Const 10))] [("x", 1)] [] [(1, IntVal 0)]) );
      "for i in (1..10) do x += i end"    >:: (fun _ ->
        assert_equal [(1, IntVal 55)]
          (eval_state [For ("i", Const 1, Const 10, [Assign (VarArray ("x", None), ModAdd, Var "i")])] [("x", 1)] [] [(1, IntVal 0)]) );
      "for i in (10..1) do x -= i end"    >:: (fun _ ->
        assert_equal [(1, IntVal 0)]
          (eval_state [For ("i", Const 10, Const 1, [Assign (VarArray ("x", None), ModSub, Var "i")])] [("x", 1)] [] [(1, IntVal 55)]) );
      "switch x case 0 x += 1 esac 1 break case 1 x += 1 case 3 x += 1 case 5 x += 1 esac 6:5:4 break default skip break hctiws x"
          >:: (fun _ ->
        assert_equal [(1, IntVal 1)]
          (eval_state [Switch (VarArray ("x", None), [((Case, [Const 0]), [Assign (VarArray ("x", None), ModAdd, Const 1)], (Esac, [Const 1], Break));
        ((Case, [Const 1]), [Assign (VarArray ("x", None), ModAdd, Const 1)], (NoEsac, [], NoBreak));
        ((Case, [Const 3]), [Assign (VarArray ("x", None), ModAdd, Const 1)], (NoEsac, [], NoBreak));
        ((Case, [Const 5]), [Assign (VarArray ("x", None), ModAdd, Const 1)], (Esac, [Const 6; Const 5; Const 4], Break));
        ((Case, [Const 6]), [Assign (VarArray ("x", None), ModAdd, Const 1)], (Esac, [Const 7], Break))], [Skip], VarArray ("x", None))] 
        [("x", 1)] [] [(1, IntVal 0)]) );
      "switch x case 0 x += 1 esac 1 break case 1 x += 1 case 3 x += 1 case 5 x += 1 esac 6:5:4 break default skip break hctiws x"
          >:: (fun _ ->
        assert_equal [(1, IntVal 4)]
          (eval_state [Switch (VarArray ("x", None), [((Case, [Const 0]), [Assign (VarArray ("x", None), ModAdd, Const 1)], (Esac, [Const 1], Break));
        ((Case, [Const 1]), [Assign (VarArray ("x", None), ModAdd, Const 1)], (NoEsac, [], NoBreak));
        ((Case, [Const 3]), [Assign (VarArray ("x", None), ModAdd, Const 1)], (NoEsac, [], NoBreak));
        ((Case, [Const 5]), [Assign (VarArray ("x", None), ModAdd, Const 1)], (Esac, [Const 6; Const 5; Const 4], Break));
        ((Case, [Const 6]), [Assign (VarArray ("x", None), ModAdd, Const 1)], (Esac, [Const 7], Break))], [Skip], VarArray ("x", None))] 
        [("x", 1)] [] [(1, IntVal 1)]) );
      "switch x case 0 x += 1 esac 1 break case 1 x += 1 case 3 x += 1 case 5 x += 1 esac 6:5:4 break default skip break hctiws x"
          >:: (fun _ ->
        assert_equal [(1, IntVal 5)]
          (eval_state [Switch (VarArray ("x", None), [((Case, [Const 0]), [Assign (VarArray ("x", None), ModAdd, Const 1)], (Esac, [Const 1], Break));
        ((Case, [Const 1]), [Assign (VarArray ("x", None), ModAdd, Const 1)], (NoEsac, [], NoBreak));
        ((Case, [Const 3]), [Assign (VarArray ("x", None), ModAdd, Const 1)], (NoEsac, [], NoBreak));
        ((Case, [Const 5]), [Assign (VarArray ("x", None), ModAdd, Const 1)], (Esac, [Const 6; Const 5; Const 4], Break));
        ((Case, [Const 6]), [Assign (VarArray ("x", None), ModAdd, Const 1)], (Esac, [Const 7], Break))], [Skip], VarArray ("x", None))] 
        [("x", 1)] [] [(1, IntVal 3)]) );
      "switch x case 0 x += 1 esac 1 break case 1 x += 1 case 3 x += 1 case 5 x += 1 esac 6:5:4 break default skip break hctiws x"
          >:: (fun _ ->
        assert_equal [(1, IntVal 6)]
          (eval_state [Switch (VarArray ("x", None), [((Case, [Const 0]), [Assign (VarArray ("x", None), ModAdd, Const 1)], (Esac, [Const 1], Break));
        ((Case, [Const 1]), [Assign (VarArray ("x", None), ModAdd, Const 1)], (NoEsac, [], NoBreak));
        ((Case, [Const 3]), [Assign (VarArray ("x", None), ModAdd, Const 1)], (NoEsac, [], NoBreak));
        ((Case, [Const 5]), [Assign (VarArray ("x", None), ModAdd, Const 1)], (Esac, [Const 6; Const 5; Const 4], Break));
        ((Case, [Const 6]), [Assign (VarArray ("x", None), ModAdd, Const 1)], (Esac, [Const 7], Break))], [Skip], VarArray ("x", None))] 
        [("x", 1)] [] [(1, IntVal 5)]) );
      "switch x case 0 x += 1 esac 1 break case 1 x += 1 case 3 x += 1 case 5 x += 1 esac 6:5:4 break default skip break hctiws x"
          >:: (fun _ ->
        assert_equal [(1, IntVal 7)]
          (eval_state [Switch (VarArray ("x", None), [((Case, [Const 0]), [Assign (VarArray ("x", None), ModAdd, Const 1)], (Esac, [Const 1], Break));
        ((Case, [Const 1]), [Assign (VarArray ("x", None), ModAdd, Const 1)], (NoEsac, [], NoBreak));
        ((Case, [Const 3]), [Assign (VarArray ("x", None), ModAdd, Const 1)], (NoEsac, [], NoBreak));
        ((Case, [Const 5]), [Assign (VarArray ("x", None), ModAdd, Const 1)], (Esac, [Const 6; Const 5; Const 4], Break));
        ((Case, [Const 6]), [Assign (VarArray ("x", None), ModAdd, Const 1)], (Esac, [Const 7], Break))], [Skip], VarArray ("x", None))] 
        [("x", 1)] [] [(1, IntVal 6)]) );
      "switch x case 0 x += 1 esac 1 break case 1 x += 1 case 3 x += 1 case 5 x += 1 esac 6:5:4 break default skip break hctiws x"
          >:: (fun _ ->
        assert_equal [(1, IntVal 8)]
          (eval_state [Switch (VarArray ("x", None), [((Case, [Const 0]), [Assign (VarArray ("x", None), ModAdd, Const 1)], (Esac, [Const 1], Break));
        ((Case, [Const 1]), [Assign (VarArray ("x", None), ModAdd, Const 1)], (NoEsac, [], NoBreak));
        ((Case, [Const 3]), [Assign (VarArray ("x", None), ModAdd, Const 1)], (NoEsac, [], NoBreak));
        ((Case, [Const 5]), [Assign (VarArray ("x", None), ModAdd, Const 1)], (Esac, [Const 6; Const 5; Const 4], Break));
        ((Case, [Const 6]), [Assign (VarArray ("x", None), ModAdd, Const 1)], (Esac, [Const 7], Break))], [Skip], VarArray ("x", None))] 
        [("x", 1)] [] [(1, IntVal 8)]) );
      "switch x case 1 x -= 1 esac 0 break case 6:5:4 x -= 1 esac 5 x -= 1 esac 3 x -= 1 esac 1 break case 
      7 x -= 1 esac 6 break default skip break hctiws x"
          >:: (fun _ ->
        assert_equal [(1, IntVal 0)]
          (eval_state [Switch (VarArray ("x", None), [((Case, [Const 1]),
         [Assign (VarArray ("x", None), ModSub,
           Const 1)], (Esac, [Const 0], Break));
        ((Case, [Const 6; Const 5; Const 4]), [Assign (VarArray ("x", None), ModSub,
           Const 1)], (Esac, [Const 5], NoBreak));
        ((NoCase, []), [Assign (VarArray ("x", None), ModSub,
           Const 1)], (Esac, [Const 3], NoBreak));
        ((NoCase, []), [Assign (VarArray ("x", None), ModSub,
           Const 1)], (Esac, [Const 1], Break));
        ((Case, [Const 7]), [Assign (VarArray ("x", None), ModSub,
           Const 1)], (Esac, [Const 6], Break))], [Skip], VarArray ("x", None))] 
        [("x", 1)] [] [(1, IntVal 1)]) );
      "switch x case 1 x -= 1 esac 0 break case 6:5:4 x -= 1 esac 5 x -= 1 esac 3 x -= 1 esac 1 break case 
      7 x -= 1 esac 6 break default skip break hctiws x"
          >:: (fun _ ->
        assert_equal [(1, IntVal 5)]
          (eval_state [Switch (VarArray ("x", None), [((Case, [Const 1]),
         [Assign (VarArray ("x", None), ModSub,
           Const 1)], (Esac, [Const 0], Break));
        ((Case, [Const 6; Const 5; Const 4]), [Assign (VarArray ("x", None), ModSub,
           Const 1)], (Esac, [Const 5], NoBreak));
        ((NoCase, []), [Assign (VarArray ("x", None), ModSub,
           Const 1)], (Esac, [Const 3], NoBreak));
        ((NoCase, []), [Assign (VarArray ("x", None), ModSub,
           Const 1)], (Esac, [Const 1], Break));
        ((Case, [Const 7]), [Assign (VarArray ("x", None), ModSub,
           Const 1)], (Esac, [Const 6], Break))], [Skip], VarArray ("x", None))] 
        [("x", 1)] [] [(1, IntVal 6)]) );
      "switch x case 1 x -= 1 esac 0 break case 6:5:4 x -= 1 esac 5 x -= 1 esac 3 x -= 1 esac 1 break case 
      7 x -= 1 esac 6 break default skip break hctiws x"
          >:: (fun _ ->
        assert_equal [(1, IntVal 3)]
          (eval_state [Switch (VarArray ("x", None), [((Case, [Const 1]),
         [Assign (VarArray ("x", None), ModSub,
           Const 1)], (Esac, [Const 0], Break));
        ((Case, [Const 6; Const 5; Const 4]), [Assign (VarArray ("x", None), ModSub,
           Const 1)], (Esac, [Const 5], NoBreak));
        ((NoCase, []), [Assign (VarArray ("x", None), ModSub,
           Const 1)], (Esac, [Const 3], NoBreak));
        ((NoCase, []), [Assign (VarArray ("x", None), ModSub,
           Const 1)], (Esac, [Const 1], Break));
        ((Case, [Const 7]), [Assign (VarArray ("x", None), ModSub,
           Const 1)], (Esac, [Const 6], Break))], [Skip], VarArray ("x", None))] 
        [("x", 1)] [] [(1, IntVal 5)]) );
      "switch x case 1 x -= 1 esac 0 break case 6:5:4 x -= 1 esac 5 x -= 1 esac 3 x -= 1 esac 1 break case 
      7 x -= 1 esac 6 break default skip break hctiws x"
          >:: (fun _ ->
        assert_equal [(1, IntVal 1)]
          (eval_state [Switch (VarArray ("x", None), [((Case, [Const 1]),
         [Assign (VarArray ("x", None), ModSub,
           Const 1)], (Esac, [Const 0], Break));
        ((Case, [Const 6; Const 5; Const 4]), [Assign (VarArray ("x", None), ModSub,
           Const 1)], (Esac, [Const 5], NoBreak));
        ((NoCase, []), [Assign (VarArray ("x", None), ModSub,
           Const 1)], (Esac, [Const 3], NoBreak));
        ((NoCase, []), [Assign (VarArray ("x", None), ModSub,
           Const 1)], (Esac, [Const 1], Break));
        ((Case, [Const 7]), [Assign (VarArray ("x", None), ModSub,
           Const 1)], (Esac, [Const 6], Break))], [Skip], VarArray ("x", None))] 
        [("x", 1)] [] [(1, IntVal 4)]) );
      "switch x case 1 x -= 1 esac 0 break case 6:5:4 x -= 1 esac 5 x -= 1 esac 3 x -= 1 esac 1 break case 
      7 x -= 1 esac 6 break default skip break hctiws x"
          >:: (fun _ ->
        assert_equal [(1, IntVal 6)]
          (eval_state [Switch (VarArray ("x", None), [((Case, [Const 1]),
         [Assign (VarArray ("x", None), ModSub,
           Const 1)], (Esac, [Const 0], Break));
        ((Case, [Const 6; Const 5; Const 4]), [Assign (VarArray ("x", None), ModSub,
           Const 1)], (Esac, [Const 5], NoBreak));
        ((NoCase, []), [Assign (VarArray ("x", None), ModSub,
           Const 1)], (Esac, [Const 3], NoBreak));
        ((NoCase, []), [Assign (VarArray ("x", None), ModSub,
           Const 1)], (Esac, [Const 1], Break));
        ((Case, [Const 7]), [Assign (VarArray ("x", None), ModSub,
           Const 1)], (Esac, [Const 6], Break))], [Skip], VarArray ("x", None))] 
        [("x", 1)] [] [(1, IntVal 7)]) );
      "switch x case 1 x -= 1 esac 0 break case 6:5:4 x -= 1 esac 5 x -= 1 esac 3 x -= 1 esac 1 break case 
      7 x -= 1 esac 6 break default skip break hctiws x"
          >:: (fun _ ->
        assert_equal [(1, IntVal 8)]
          (eval_state [Switch (VarArray ("x", None), [((Case, [Const 1]),
         [Assign (VarArray ("x", None), ModSub,
           Const 1)], (Esac, [Const 0], Break));
        ((Case, [Const 6; Const 5; Const 4]), [Assign (VarArray ("x", None), ModSub,
           Const 1)], (Esac, [Const 5], NoBreak));
        ((NoCase, []), [Assign (VarArray ("x", None), ModSub,
           Const 1)], (Esac, [Const 3], NoBreak));
        ((NoCase, []), [Assign (VarArray ("x", None), ModSub,
           Const 1)], (Esac, [Const 1], Break));
        ((Case, [Const 7]), [Assign (VarArray ("x", None), ModSub,
           Const 1)], (Esac, [Const 6], Break))], [Skip], VarArray ("x", None))] 
        [("x", 1)] [] [(1, IntVal 8)]) );
      "call Plus1(result)"    >:: (fun _ ->
        assert_equal [(1, IntVal 1); (2, LocsVal 3); (3, ObjVal ("Program", [("result", 1); ("this", 2)]))]
          (eval_state [LocalCall("Plus1", [Id "result"])]
             [("result", 1); ("this", 2)]
             [("Program",
    ([Decl (IntegerType, "result")],
     [MDecl ("main", [], [LocalCall ("Plus1", [Id "result"])]);
      MDecl ("Plus1", [Decl (IntegerType, "n")],
       [Assign (VarArray("n", None), ModAdd, Const 1)])]))]
  [(1, IntVal 0);  (2, LocsVal 3); (3, ObjVal ("Program", [("result", 1); ("this", 2)]))] ) );
      "uncall Plus1(result)"    >:: (fun _ ->
        assert_equal [(1, IntVal 0);  (2, LocsVal 3); (3, ObjVal ("Program", [("result", 1); ("this", 2)]))]
          (eval_state [LocalUncall("Plus1", [Id "result"])]
             [("result", 1); ("this", 2)]
             [("Program",
    ([Decl (IntegerType, "result")],
     [MDecl ("main", [], [LocalCall ("Plus1", [Id "result"])]);
      MDecl ("Plus1", [Decl (IntegerType, "n")],
       [Assign (VarArray("n", None), ModAdd, Const 1)])]))]
  [(1, IntVal 1); (2, LocsVal 3); (3, ObjVal ("Program", [("result", 1); ("this", 2)]))] ) );

      "t::call Plus1(result)"    >:: (fun _ ->
        assert_equal [(1, IntVal 1); (2, LocsVal 3); (3, ObjVal ("Program", [("result", 1); ("this", 2)])); (4, LocsVal 5); (5, ObjVal ("Test", []))]
          (eval_state [ObjectCall (VarArray("t", None), "Plus1", [Id "result"])]
             [("t", 4); ("result", 1); ("this", 2)]
          [("Test",
            ([],
             [MDecl ("Plus1", [Decl (IntegerType, "n")],
                     [Assign (VarArray("n", None), ModAdd, Const 1)])]));
           ("Program",
            ([Decl (IntegerType, "result")],
             [MDecl ("main", [],
                     [LocalBlock (ObjectType "Test", "t", Nil,
                                  [ObjectConstruction ("Test", VarArray("t", None));
                                   ObjectCall (VarArray("t", None), "Plus1", [Id "result"]);
                                   ObjectDestruction ("Test", VarArray("t", None))],
                                  Nil)])]))]
          [(1, IntVal 0); (2, LocsVal 3);
           (3, ObjVal ("Program", [("result", 1); ("this", 2)])); (4, LocsVal 5);
           (5, ObjVal ("Test", []))]
 ) );

      "t::uncall Plus1(result)"    >:: (fun _ ->
        assert_equal [(1, IntVal 0); (2, LocsVal 3);
                      (3, ObjVal ("Program", [("result", 1); ("this", 2)])); (4, LocsVal 5);
                      (5, ObjVal ("Test", []))]
          (eval_state [ObjectUncall (VarArray("t", None), "Plus1", [Id "result"])]
             [("t", 4); ("result", 1); ("this", 2)]
             [("Test",
               ([],
                [MDecl ("Plus1", [Decl (IntegerType, "n")],
                        [Assign (VarArray("n", None), ModAdd, Const 1)])]));
              ("Program",
               ([Decl (IntegerType, "result")],
                [MDecl ("main", [],
                        [LocalBlock (ObjectType "Test", "t", Nil,
                                     [ObjectConstruction ("Test", VarArray("t", None));
                                      ObjectCall (VarArray("t", None), "Plus1", [Id "result"]);
                                      ObjectDestruction ("Test", VarArray("t", None))],
                                     Nil)])]))]
             [(1, IntVal 1); (2, LocsVal 3); (3, ObjVal ("Program", [("result", 1); ("this", 2)])); (4, LocsVal 5); (5, ObjVal ("Test", []))]
      ) );
      
      "call ts[0]::Plus1(result)"    >:: (fun _ ->
        assert_equal [(1, LocsVec [5; 6]); (2, IntVal 1); (3, LocsVal 4);
                      (4, ObjVal ("Program", [("xs", 1); ("result", 2); ("this", 3)]));
                      (5, LocsVal 7); (6, IntVal 0); (7, ObjVal ("Test", []))]
          (eval_state [ObjectCall (VarArray ("xs", Some (Const 0)), "Plus1", [Id "result"])]
             [("xs", 1); ("result", 2); ("this", 3)]
             [("Test",
               ([],
                [MDecl ("Plus1", [Decl (IntegerType, "n")],
                        [Assign (VarArray ("n", None), ModAdd, Const 1)])]));
              ("Program",
               ([Decl (ObjectArrayType "Test", "xs"); Decl (IntegerType, "result")],
                [MDecl ("main", [],
                        [ArrayConstruction (("Test", Const 2), VarArray ("xs", None));
                         ObjectConstruction ("Test", VarArray ("xs", Some (Const 0)));
                         ObjectCall (VarArray ("xs", Some (Const 0)), "Plus1", [Id "result"])])]))]
             
             [(1, LocsVec [5; 6]); (2, IntVal 0); (3, LocsVal 4);
              (4, ObjVal ("Program", [("xs", 1); ("result", 2); ("this", 3)]));
              (5, LocsVal 7); (6, IntVal 0); (7, ObjVal ("Test", []))]             
      ) );
      
      "uncall xs[0]::Plus1(result)"    >:: (fun _ ->
        assert_equal [(1, LocsVec [5; 6]); (2, IntVal 0); (3, LocsVal 4);
                      (4, ObjVal ("Program", [("xs", 1); ("result", 2); ("this", 3)]));
                      (5, LocsVal 7); (6, IntVal 0); (7, ObjVal ("Test", []))]
          (eval_state [ObjectUncall (VarArray ("xs", Some (Const 0)), "Plus1", [Id "result"])]
             
             [("xs", 1); ("result", 2); ("this", 3)]
             [("Test",
               ([],
                [MDecl ("Plus1", [Decl (IntegerType, "n")],
                        [Assign (VarArray ("n", None), ModAdd, Const 1)])]));
              ("Program",
               ([Decl (ObjectArrayType "Test", "xs"); Decl (IntegerType, "result")],
                [MDecl ("main", [],
                        [ArrayConstruction (("Test", Const 2), VarArray ("xs", None));
                         ObjectConstruction ("Test", VarArray ("xs", Some (Const 0)));
                         ObjectCall (VarArray ("xs", Some (Const 0)), "Plus1", [Id "result"]);
                         ObjectUncall (VarArray ("xs", Some (Const 0)), "Plus1", [Id "result"])])]))]
             
             [(1, LocsVec [5; 6]); (2, IntVal 1); (3, LocsVal 4);
              (4, ObjVal ("Program", [("xs", 1); ("result", 2); ("this", 3)]));
              (5, LocsVal 7); (6, IntVal 0); (7, ObjVal ("Test", []))]
      ) );
      
      "construct Test t  t::call Plus1(result) destruct t"    >:: (fun _ ->
        assert_equal   [(1, IntVal 1); (2, LocsVal 3);
                        (3, ObjVal ("Program", [("result", 1); ("this", 2)])); (4, LocsVal 5);
                        (5, ObjVal ("Test", []))]
          (eval_state [ObjectBlock ("Test", "t",[ObjectCall (VarArray("t", None), "Plus1", [Id "result"])])]
             [("result", 1); ("this", 2)]
             [("Test",
               ([],
                [MDecl ("Plus1", [Decl (IntegerType, "n")],
                        [Assign (VarArray("n", None), ModAdd, Const 1)])]));
              ("Program",
               ([Decl (IntegerType, "result")],
                [MDecl ("main", [],
                        [ObjectBlock ("Test", "t",
                                      [ObjectCall (VarArray("t", None), "Plus1", [Id "result"])])])]))]
             [(1, IntVal 0); (2, LocsVal 3);
              (3, ObjVal ("Program", [("result", 1); ("this", 2)]))]
      ) );

      "new Test t"    >:: (fun _ ->
        assert_equal [(1, IntVal 0); (2, LocsVal 3);
                      (3, ObjVal ("Program", [("result", 1); ("this", 2)])); (4, LocsVal 5);
                      (5, ObjVal ("Test", []))]
          (eval_state [ObjectConstruction ("Test", VarArray("t", None))]
             [("t", 4); ("result", 1); ("this", 2)]
             [("Test",
               ([],
                [MDecl ("Plus1", [Decl (IntegerType, "n")],
                        [Assign (VarArray("n", None), ModAdd, Const 1)])]));
              ("Program",
               ([Decl (IntegerType, "result")],
                [MDecl ("main", [],
                        [LocalBlock (ObjectType "Test", "t", Nil,
                                     [ObjectConstruction ("Test", VarArray("t", None));
                                      ObjectDestruction ("Test", VarArray("t", None))],
                                     Nil)])]))]             
             [(1, IntVal 0); (2, LocsVal 3);
              (3, ObjVal ("Program", [("result", 1); ("this", 2)])); (4, IntVal 0)] 
      ) );
      
      "delete Test t"    >:: (fun _ ->
        assert_equal [(1, IntVal 0); (2, LocsVal 3);
                      (3, ObjVal ("Program", [("result", 1); ("this", 2)])); (4, IntVal 0)] 
          (eval_state [ObjectDestruction ("Test", VarArray("t", None))]
             [("t", 4); ("result", 1); ("this", 2)]
             [("Test",
               ([],
                [MDecl ("Plus1", [Decl (IntegerType, "n")],
                        [Assign (VarArray("n", None), ModAdd, Const 1)])]));
              ("Program",
               ([Decl (IntegerType, "result")],
                [MDecl ("main", [],
                        [LocalBlock (ObjectType "Test", "t", Nil,
                                     [ObjectConstruction ("Test", VarArray("t", None));
                                      ObjectDestruction ("Test", VarArray("t", None))],
                                     Nil)])]))]
             [(1, IntVal 0); (2, LocsVal 3);
              (3, ObjVal ("Program", [("result", 1); ("this", 2)])); (4, LocsVal 5);
              (5, ObjVal ("Test", []))]          
      ) );
      
      
      "copy Test t tcopy"    >:: (fun _ ->
        assert_equal [(1, LocsVal 5); (2, LocsVal 5); (3, LocsVal 4);
                      (4, ObjVal ("Program", [("t", 1); ("tcopy", 2); ("this", 3)]));
                      (5, ObjVal ("Test", []))]
          (eval_state [CopyReference (ObjectType "Test", VarArray ("t", None),
                                      VarArray ("tcopy", None))]
             [("t", 1); ("tcopy", 2); ("this", 3)]
             []
             [(1, LocsVal 5); (2, IntVal 0); (3, LocsVal 4);
              (4, ObjVal ("Program", [("t", 1); ("tcopy", 2); ("this", 3)]));
              (5, ObjVal ("Test", []))]
      ) );

      "uncopy Test t tcopy"    >:: (fun _ ->
        assert_equal [(1, LocsVal 5); (2, IntVal 0); (3, LocsVal 4);
                      (4, ObjVal ("Program", [("t", 1); ("tcopy", 2); ("this", 3)]));
                      (5, ObjVal ("Test", []))]
          (eval_state [UncopyReference (ObjectType "Test", VarArray ("t", None),
                                        VarArray ("tcopy", None))]
             [("t", 1); ("tcopy", 2); ("this", 3)]
             []
             [(1, LocsVal 5); (2, LocsVal 5); (3, LocsVal 4);
              (4, ObjVal ("Program", [("t", 1); ("tcopy", 2); ("this", 3)]));
              (5, ObjVal ("Test", []))]
            
      ) );

      "copy Test xs[0] tcopy"    >:: (fun _ ->
        assert_equal [(1, LocsVec [5; 6]); (2, LocsVal 7); (3, LocsVal 4);
                      (4, ObjVal ("Program", [("xs", 1); ("tcopy", 2); ("this", 3)]));
                      (5, LocsVal 7); (6, IntVal 0); (7, ObjVal ("Test", [("a", 8); ("b", 9)]));
                      (8, IntVal 0); (9, IntVal 0)]
          (eval_state [CopyReference (ObjectType "Test", VarArray ("xs", Some (Const 0)),
  VarArray ("tcopy", None))]
             [("xs", 1); ("tcopy", 2); ("this", 3)]             
             []             
             [(1, LocsVec [5; 6]); (2, IntVal 0); (3, LocsVal 4);
              (4, ObjVal ("Program", [("xs", 1); ("tcopy", 2); ("this", 3)]));
              (5, LocsVal 7); (6, IntVal 0); (7, ObjVal ("Test", [("a", 8); ("b", 9)]));
              (8, IntVal 0); (9, IntVal 0)]          
      ) );

      "uncopy Test xs[0] tcopy"    >:: (fun _ ->
        assert_equal [(1, LocsVec [5; 6]); (2, IntVal 0); (3, LocsVal 4);
                      (4, ObjVal ("Program", [("xs", 1); ("tcopy", 2); ("this", 3)]));
                      (5, LocsVal 7); (6, IntVal 0); (7, ObjVal ("Test", [("a", 8); ("b", 9)]));
                      (8, IntVal 0); (9, IntVal 0)]          
          (eval_state [UncopyReference (ObjectType "Test", VarArray ("xs", Some (Const 0)),
  VarArray ("tcopy", None))]
             [("xs", 1); ("tcopy", 2); ("this", 3)]                          
             []
             [(1, LocsVec [5; 6]); (2, LocsVal 7); (3, LocsVal 4);
              (4, ObjVal ("Program", [("xs", 1); ("tcopy", 2); ("this", 3)]));
              (5, LocsVal 7); (6, IntVal 0); (7, ObjVal ("Test", [("a", 8); ("b", 9)]));
              (8, IntVal 0); (9, IntVal 0)]           
      ) );
      
      "copy Test xs[0] xs[1]"    >:: (fun _ ->
        assert_equal [(1, LocsVec [5; 6]); (2, IntVal 0); (3, LocsVal 4);
                      (4, ObjVal ("Program", [("xs", 1); ("tcopy", 2); ("this", 3)]));
                      (5, LocsVal 7); (6, LocsVal 7); (7, ObjVal ("Test", [("a", 8); ("b", 9)]));
                      (8, IntVal 0); (9, IntVal 0)]

         
          (eval_state [CopyReference (ObjectType "Test", VarArray ("xs", Some (Const 0)),
                                      VarArray ("xs", Some (Const 1)))]
             [("xs", 1); ("tcopy", 2); ("this", 3)]             
             []             
             [(1, LocsVec [5; 6]); (2, IntVal 0); (3, LocsVal 4);
              (4, ObjVal ("Program", [("xs", 1); ("tcopy", 2); ("this", 3)]));
              (5, LocsVal 7); (6, IntVal 0); (7, ObjVal ("Test", [("a", 8); ("b", 9)]));
              (8, IntVal 0); (9, IntVal 0)]
      ) );
      
      "uncopy Test xs[0] xs[1]"    >:: (fun _ ->
        assert_equal [(1, LocsVec [5; 6]); (2, IntVal 0); (3, LocsVal 4);
                      (4, ObjVal ("Program", [("xs", 1); ("tcopy", 2); ("this", 3)]));
                      (5, LocsVal 7); (6, IntVal 0); (7, ObjVal ("Test", [("a", 8); ("b", 9)]));
                      (8, IntVal 0); (9, IntVal 0)]
          
          (eval_state [UncopyReference (ObjectType "Test", VarArray ("xs", Some (Const 0)),
                                      VarArray ("xs", Some (Const 1)))]
             [("xs", 1); ("tcopy", 2); ("this", 3)]
             []
             [(1, LocsVec [5; 6]); (2, IntVal 0); (3, LocsVal 4);
              (4, ObjVal ("Program", [("xs", 1); ("tcopy", 2); ("this", 3)]));
              (5, LocsVal 7); (6, LocsVal 7); (7, ObjVal ("Test", [("a", 8); ("b", 9)]));
              (8, IntVal 0); (9, IntVal 0)]
      ) );

      "copy Test t ts[0]"    >:: (fun _ ->
        assert_equal [(1, LocsVal 5); (2, LocsVec [6; 7]); (3, LocsVal 4);
                      (4, ObjVal ("Program", [("t", 1); ("ts", 2); ("this", 3)]));
                      (5, ObjVal ("Test", [])); (6, LocsVal 5); (7, IntVal 0)]         
          (eval_state [CopyReference (ObjectType "Test", VarArray ("t", None),
                                      VarArray ("ts", Some (Const 0)))]
             [("t", 1); ("ts", 2); ("this", 3)]
             []
             [(1, LocsVal 5); (2, LocsVec [6; 7]); (3, LocsVal 4);
              (4, ObjVal ("Program", [("t", 1); ("ts", 2); ("this", 3)]));
              (5, ObjVal ("Test", [])); (6, IntVal 0); (7, IntVal 0)]         
      ) );

      "uncopy Test t ts[0]"    >:: (fun _ ->
        assert_equal [(1, LocsVal 5); (2, LocsVec [6; 7]); (3, LocsVal 4);
                      (4, ObjVal ("Program", [("t", 1); ("ts", 2); ("this", 3)]));
                      (5, ObjVal ("Test", [])); (6, IntVal 0); (7, IntVal 0)]
          (eval_state [UncopyReference (ObjectType "Test", VarArray ("t", None),
                                       VarArray ("ts", Some (Const 0)))]
             [("t", 1); ("ts", 2); ("this", 3)]
             []
             [(1, LocsVal 5); (2, LocsVec [6; 7]); (3, LocsVal 4);
              (4, ObjVal ("Program", [("t", 1); ("ts", 2); ("this", 3)]));
              (5, ObjVal ("Test", [])); (6, LocsVal 5); (7, IntVal 0)]
      ) );
      
      
      "new int[2] xs"    >:: (fun _ ->
        assert_equal [(1, LocsVec [4; 5]); (2, ObjVal ("Program", [("xs", 1); ("this", 3)])); (3, LocsVal 2); (4, IntVal 0); (5, IntVal 0)]
          (eval_state [ArrayConstruction (("int", Const 2), VarArray ("xs", None))]
             [("xs", 1); ("this", 3)]
             [("Program",
               ([Decl (IntegerArrayType, "xs")],
                [MDecl ("main", [],
                        [ArrayConstruction (("int", Const 2), VarArray ("xs", None));
                         ArrayDestruction (("int", Const 2), VarArray ("xs", None))])]))]             
             [(1, IntVal 0); (2, ObjVal ("Program", [("xs", 1); ("this", 3)]));(3, LocsVal 2)]
      ) );

      "delete int[2] xs"    >:: (fun _ ->
        assert_equal [(1, IntVal 0); (2, ObjVal ("Program", [("xs", 1); ("this", 3)]));(3, LocsVal 2)]
          (eval_state [ArrayDestruction (("int", Const 2), VarArray ("xs", None))]
             [("xs", 1); ("this", 3)]
             []
             [(1, LocsVec [4; 5]); (2, ObjVal ("Program", [("xs", 1); ("this", 3)])); (3, LocsVal 2); (4, IntVal 0); (5, IntVal 0)]) );

      "new Test[2] ts"    >:: (fun _ ->
        assert_equal [(1, LocsVec [4; 5]); (2, LocsVal 3);
                      (3, ObjVal ("Program", [("xs", 1); ("this", 2)])); (4, IntVal 0);
                      (5, IntVal 0)]
          (eval_state [ArrayConstruction (("Test", Const 2), VarArray ("xs", None))]
             [("xs", 1); ("this", 2)]
             []    
             [(1, IntVal 0); (2, LocsVal 3);
              (3, ObjVal ("Program", [("xs", 1); ("this", 2)]))]
      ) );
      
      "delete Test[2] ts"    >:: (fun _ ->
        assert_equal [(1, IntVal 0); (2, LocsVal 3);
                      (3, ObjVal ("Program", [("xs", 1); ("this", 2)]))]          
          (eval_state [ArrayDestruction (("Test", Const 2), VarArray ("xs", None))]
             [("xs", 1); ("this", 2)]
             []             
             [(1, LocsVec [4; 5]); (2, LocsVal 3);
              (3, ObjVal ("Program", [("xs", 1); ("this", 2)])); (4, IntVal 0);
              (5, IntVal 0)]
      ) ); 

      "new Test ts[0]"    >:: (fun _ ->
        assert_equal   [(1, LocsVec [4; 5]); (2, LocsVal 3);
                        (3, ObjVal ("Program", [("xs", 1); ("this", 2)])); (4, LocsVal 6);
                        (5, IntVal 0); (6, ObjVal ("Test", []))]
          (eval_state [ObjectConstruction ("Test", VarArray ("xs", Some (Const 0)))]
             [("xs", 1); ("this", 2)]
             [("Test",
               ([],
                [MDecl ("Plus1", [Decl (IntegerType, "n")],
                        [Assign (VarArray("n", None), ModAdd, Const 1)])]));
              ("Program",
               ([Decl (IntegerType, "result")],
                [MDecl ("main", [],
                        [LocalBlock (ObjectType "Test", "t", Nil,
                                     [ObjectConstruction ("Test", VarArray("t", None));
                                      ObjectDestruction ("Test", VarArray("t", None))],
                                     Nil)])]))]             
             
             [(1, LocsVec [4; 5]); (2, LocsVal 3);
              (3, ObjVal ("Program", [("xs", 1); ("this", 2)])); (4, IntVal 0);
              (5, IntVal 0)]
      ) );

      "delete Test ts[0]"    >:: (fun _ ->
        assert_equal [(1, LocsVec [4; 5]); (2, LocsVal 3);
                      (3, ObjVal ("Program", [("xs", 1); ("this", 2)])); (4, IntVal 0);
                      (5, IntVal 0)]
          (eval_state [ObjectDestruction ("Test", VarArray ("xs", Some (Const 0)))]
             [("xs", 1); ("this", 2)]
             [("Test",
               ([],
                [MDecl ("Plus1", [Decl (IntegerType, "n")],
                        [Assign (VarArray("n", None), ModAdd, Const 1)])]));
              ("Program",
               ([Decl (IntegerType, "result")],
                [MDecl ("main", [],
                        [LocalBlock (ObjectType "Test", "t", Nil,
                                     [ObjectConstruction ("Test", VarArray("t", None));
                                      ObjectDestruction ("Test", VarArray("t", None))],
                                     Nil)])]))]                          
             [(1, LocsVec [4; 5]); (2, LocsVal 3);
              (3, ObjVal ("Program", [("xs", 1); ("this", 2)])); (4, LocsVal 6);
              (5, IntVal 0); (6, ObjVal ("Test", [("a", 7); ("b", 8)]))]
      ) );      
      
      "local ingt i = 0 call::Plusi(result) delocal int i = 0"    >:: (fun _ ->
        assert_equal [(1, LocsVal 4); (2, LocsVal 3);
                      (3, ObjVal ("Program", [("t", 1); ("this", 2)]));
                      (4, ObjVal ("Test", []))]
          (eval_state [LocalBlock (IntegerType, "i", Const 0,
                                   [ObjectCall (VarArray ("t", None), "Plus1", [Id "i"])], Const 1)]
            [("t", 1); ("this", 2)]
            [("Test",
              ([],
               [MDecl ("Plus1", [Decl (IntegerType, "n")],
                       [Assign (VarArray ("n", None), ModAdd, Const 1)])]));
             ("Program",
              ([Decl (ObjectType "Test", "t")],
               [MDecl ("main", [],
                       [ObjectConstruction ("Test", VarArray ("t", None));
                        LocalBlock (IntegerType, "i", Const 0,
                                    [ObjectCall (VarArray ("t", None), "Plus1", [Id "i"])], Const 1)])]))]            
            [(1, LocsVal 4); (2, LocsVal 3);
             (3, ObjVal ("Program", [("t", 1); ("this", 2)]));
             (4, ObjVal ("Test", []))]          
      ) );

      "class Program
       int result
       method main()
       call Plus1(result)
       method Plus1(int n)
       n += 1"
      >:: (fun _ ->
        assert_equal [("result", IntVal 1)]
          (eval_prog  (Prog
                         [CDecl ("Program", None, [Decl (IntegerType, "result")],
                                 [MDecl ("main", [], [LocalCall ("Plus1", [Id "result"])]);
                                  MDecl ("Plus1", [Decl (IntegerType, "n")],
                                         [Assign (VarArray ("n", None), ModAdd, Const 1)])])]) ) );
      "class Test
       method Plus1(int n)
       n += 1

       class Program
       int result
       method main()
       local Test t = nil
       new Test t
       call t::Plus1(result)
       delete Test t
       delocal Test t = nil"
      >:: (fun _ ->
        assert_equal [("result", IntVal 1)]
          (eval_prog (Prog
                        [CDecl ("Test", None, [],
                                [MDecl ("Plus1", [Decl (IntegerType, "n")],
                                        [Assign (VarArray ("n", None), ModAdd, Const 1)])]);
                         CDecl ("Program", None, [Decl (IntegerType, "result")],
                                [MDecl ("main", [],
                                        [LocalBlock (ObjectType "Test", "t", Nil,
                                                     [ObjectConstruction ("Test", VarArray ("t", None));
                                                      ObjectCall (VarArray ("t", None), "Plus1", [Id"result"]);
                                                      ObjectDestruction ("Test", VarArray ("t", None))],
                                                     Nil)])])])
      ) );

       "class Super
        method Plus1(int n)
        n += 1

        class Sub inherits Super
        method Plus2(int n)
        n += 2

        class Program
        int result
        method main()
        local Sub s = nil
        new Sub s
        call s::Plus1(result)
        call s::Plus2(result)
        delete Sub s
        delocal Sub s = nil"
      >:: (fun _ ->
        assert_equal [("result", IntVal 3)]
          (eval_prog  (  Prog
                           [CDecl ("Super", None, [],
                                   [MDecl ("Plus1", [Decl (IntegerType, "n")],
                                           [Assign (VarArray ("n", None), ModAdd, Const 1)])]);
                            CDecl ("Sub", Some "Super", [],
                                   [MDecl ("Plus2", [Decl (IntegerType, "n")],
                                           [Assign (VarArray ("n", None), ModAdd, Const 2)])]);
                            CDecl ("Program", None, [Decl (IntegerType, "result")],
                                   [MDecl ("main", [],
                                           [LocalBlock (ObjectType "Sub", "s", Nil,
                                                        [ObjectConstruction ("Sub", VarArray ("s", None));
                                                         ObjectCall (VarArray ("s", None), "Plus1", [Id "result"]);
                                                         ObjectCall (VarArray ("s", None), "Plus2", [Id "result"]);
                                                         ObjectDestruction ("Sub", VarArray ("s", None))],
                                                        Nil)])])]
             )
      ) );
       
       "class Super
        method Plus1(int n)
        n += 1

        class Sub inherits Super
        int x
        int y
        method Plus2(int n)
        n += 2

        class Program
        Test[] ts
        int result
        method main()
        new Super[2] ts
        new Sub ts[0]
        call ts[0]::Plus1(result)
        call ts[0]::Plus2(result)
        delete Sub ts[0]        
        delete Super[2] ts"
      >:: (fun _ ->
         assert_equal [("ts", IntVal 0); ("result", IntVal 3)]
           (eval_prog  (  Prog
   [CDecl ("Super", None, [],
     [MDecl ("Plus1", [Decl (IntegerType, "n")],
       [Assign (VarArray ("n", None), ModAdd, Const 1)])]);
    CDecl ("Sub", Some "Super",
     [Decl (IntegerType, "x"); Decl (IntegerType, "y")],
     [MDecl ("Plus2", [Decl (IntegerType, "n")],
       [Assign (VarArray ("n", None), ModAdd, Const 2)])]);
    CDecl ("Program", None,
     [Decl (ObjectArrayType "Test", "ts"); Decl (IntegerType, "result")],
     [MDecl ("main", [],
       [ArrayConstruction (("Super", Const 2), VarArray("ts", None));
        ObjectConstruction ("Sub", VarArray ("ts", Some (Const 0)));
        ObjectCall (VarArray ("ts", Some (Const 0)), "Plus1", [Id "result"]);
        ObjectCall (VarArray ("ts", Some (Const 0)), "Plus2", [Id "result"]);
        ObjectDestruction ("Sub", VarArray ("ts", Some (Const 0)));
        ArrayDestruction (("Super", Const 2), VarArray("ts", None))])]
)]
       ) )) 
]    
let _ = run_test_tt_main tests
