open OUnit
open Syntax
open Value
open Eval
   
let tests = "test suite for eval.ml" >::: [
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
      "x[0] += 1"    >:: (fun _ ->
        assert_equal [(1, LocsVec[2; 3]); (2, IntVal 1); (3, IntVal 0)]
          (eval_state [Assign(("x", Some(Const 0)), ModAdd, Const 1)] [("x", 1)] [] [(1, LocsVec[2; 3]); (2, IntVal 0); (3, IntVal 0)]) );
      "x[0] -= 1"    >:: (fun _ ->
        assert_equal [(1, LocsVec[2; 3]); (2, IntVal (-1)); (3, IntVal 0)]
          (eval_state [Assign(("x", Some(Const 0)), ModSub, Const 1)] [("x", 1)] [] [(1, LocsVec[2; 3]); (2, IntVal 0); (3, IntVal 0)]) );
      "x[0] ^= 1"    >:: (fun _ ->
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

      "call Plus1(result)"    >:: (fun _ ->
        assert_equal [(1, IntVal 1); (2, LocsVal 3); (3, ObjVal ("Program", [("result", 1); ("this", 2)]))]
          (eval_state [LocalCall("Plus1", [("result", None)])]
             [("result", 1); ("this", 2)]
             [("Program",
    ([Decl (IntegerType, "result")],
     [MDecl ("main", [], [LocalCall ("Plus1", [("result", None)])]);
      MDecl ("Plus1", [Decl (IntegerType, "n")],
       [Assign (("n", None), ModAdd, Const 1)])]))]
  [(1, IntVal 0);  (2, LocsVal 3); (3, ObjVal ("Program", [("result", 1); ("this", 2)]))] ) );

      "uncall Plus1(result)"    >:: (fun _ ->
        assert_equal [(1, IntVal 0);  (2, LocsVal 3); (3, ObjVal ("Program", [("result", 1); ("this", 2)]))]
          (eval_state [LocalUncall("Plus1", [("result", None)])]
             [("result", 1); ("this", 2)]
             [("Program",
    ([Decl (IntegerType, "result")],
     [MDecl ("main", [], [LocalCall ("Plus1", [("result", None)])]);
      MDecl ("Plus1", [Decl (IntegerType, "n")],
       [Assign (("n", None), ModAdd, Const 1)])]))]
  [(1, IntVal 1); (2, LocsVal 3); (3, ObjVal ("Program", [("result", 1); ("this", 2)]))] ) );

      "t::call Plus1(result)"    >:: (fun _ ->
        assert_equal [(1, IntVal 1); (2, LocsVal 3); (3, ObjVal ("Program", [("result", 1); ("this", 2)])); (4, LocsVal 5); (5, ObjVal ("Test", []))]
          (eval_state [ObjectCall (("t", None), "Plus1", [("result", None)])]
             [("t", 4); ("result", 1); ("this", 2)]
          [("Test",
            ([],
             [MDecl ("Plus1", [Decl (IntegerType, "n")],
                     [Assign (("n", None), ModAdd, Const 1)])]));
           ("Program",
            ([Decl (IntegerType, "result")],
             [MDecl ("main", [],
                     [LocalBlock (ObjectType "Test", "t", Nil,
                                  [ObjectConstruction ("Test", ("t", None));
                                   ObjectCall (("t", None), "Plus1", [("result", None)]);
                                   ObjectDestruction ("Test", ("t", None))],
                                  Nil)])]))]
          [(1, IntVal 0); (2, LocsVal 3);
           (3, ObjVal ("Program", [("result", 1); ("this", 2)])); (4, LocsVal 5);
           (5, ObjVal ("Test", []))]
 ) );

           "t::uncall Plus1(result)"    >:: (fun _ ->
        assert_equal [(1, IntVal 0); (2, LocsVal 3);
                      (3, ObjVal ("Program", [("result", 1); ("this", 2)])); (4, LocsVal 5);
                      (5, ObjVal ("Test", []))]
          (eval_state [ObjectUncall (("t", None), "Plus1", [("result", None)])]
             [("t", 4); ("result", 1); ("this", 2)]
          [("Test",
            ([],
             [MDecl ("Plus1", [Decl (IntegerType, "n")],
                     [Assign (("n", None), ModAdd, Const 1)])]));
           ("Program",
            ([Decl (IntegerType, "result")],
             [MDecl ("main", [],
                     [LocalBlock (ObjectType "Test", "t", Nil,
                                  [ObjectConstruction ("Test", ("t", None));
                                   ObjectCall (("t", None), "Plus1", [("result", None)]);
                                   ObjectDestruction ("Test", ("t", None))],
                                  Nil)])]))]
          [(1, IntVal 1); (2, LocsVal 3); (3, ObjVal ("Program", [("result", 1); ("this", 2)])); (4, LocsVal 5); (5, ObjVal ("Test", []))]
 ) );
             
      "call ts[0]::Plus1(result)"    >:: (fun _ ->
        assert_equal [(1, IntVal 1); (2, LocsVec [5; 6]); (3, LocsVal 4);
                      (4, ObjVal ("Program", [("result", 1); ("ts", 2); ("this", 3)]));
                      (5, LocsVal 7); (6, IntVal 0); (7, LocsVal 8); (8, ObjVal ("Test", []))]
          (eval_state [ObjectCall (("ts", Some (Const 0)), "Plus1", [("result", None)])]
             [("result", 1); ("ts", 2); ("this", 3)]
             [("Test",
               ([],
                [MDecl ("Plus1", [Decl (IntegerType, "n")],
                        [Assign (("n", None), ModAdd, Const 1)])]));
              ("Program",
               ([Decl (IntegerType, "result"); Decl (ObjectArrayType "Test", "ts")],
                [MDecl ("main", [],
                        [ArrayConstruction (("Test", Const 2), "ts");
                         ObjectConstruction ("Test", ("ts", Some (Const 0)));
                         ObjectCall (("ts", Some (Const 0)), "Plus1", [("result", None)]);
                         ObjectUncall (("ts", Some (Const 0)), "Plus1", [("result", None)])])]))]             
             [(1, IntVal 0); (2, LocsVec [5; 6]); (3, LocsVal 4);
              (4, ObjVal ("Program", [("result", 1); ("ts", 2); ("this", 3)]));
              (5, LocsVal 7); (6, IntVal 0); (7, LocsVal 8); (8, ObjVal ("Test", []))]          
      ) );
      
      "uncall ts[0]::Plus1(result)"    >:: (fun _ ->
             assert_equal [(1, IntVal 0); (2, LocsVec [5; 6]); (3, LocsVal 4);
                           (4, ObjVal ("Program", [("result", 1); ("ts", 2); ("this", 3)]));
                           (5, LocsVal 7); (6, IntVal 0); (7, LocsVal 8); (8, ObjVal ("Test", []))]
               (eval_state [ ObjectUncall (("ts", Some (Const 0)), "Plus1", [("result", None)])]
                  [("result", 1); ("ts", 2); ("this", 3)]
                  [("Test",
                    ([],
                     [MDecl ("Plus1", [Decl (IntegerType, "n")],
                        [Assign (("n", None), ModAdd, Const 1)])]));
                   ("Program",
                    ([Decl (IntegerType, "result"); Decl (ObjectArrayType "Test", "ts")],
                     [MDecl ("main", [],
                             [ArrayConstruction (("Test", Const 2), "ts");
                              ObjectConstruction ("Test", ("ts", Some (Const 0)));
                              ObjectCall (("ts", Some (Const 0)), "Plus1", [("result", None)]);
                              ObjectUncall (("ts", Some (Const 0)), "Plus1", [("result", None)])])]))]
                  [(1, IntVal 1); (2, LocsVec [5; 6]); (3, LocsVal 4);
                   (4, ObjVal ("Program", [("result", 1); ("ts", 2); ("this", 3)]));
                   (5, LocsVal 7); (6, IntVal 0); (7, LocsVal 8); (8, ObjVal ("Test", []))]  
      ) );
      
      "construct Test t  t::call Plus1(result) destruct t"    >:: (fun _ ->
        assert_equal   [(1, IntVal 1); (2, LocsVal 3);
                        (3, ObjVal ("Program", [("result", 1); ("this", 2)])); (4, LocsVal 5);
                        (5, ObjVal ("Test", []))]
          (eval_state [ObjectBlock ("Test", "t",[ObjectCall (("t", None), "Plus1", [("result", None)])])]
             [("result", 1); ("this", 2)]
             [("Test",
               ([],
                [MDecl ("Plus1", [Decl (IntegerType, "n")],
                        [Assign (("n", None), ModAdd, Const 1)])]));
              ("Program",
               ([Decl (IntegerType, "result")],
                [MDecl ("main", [],
                        [ObjectBlock ("Test", "t",
                                      [ObjectCall (("t", None), "Plus1", [("result", None)])])])]))]
             [(1, IntVal 0); (2, LocsVal 3);
              (3, ObjVal ("Program", [("result", 1); ("this", 2)]))]
      ) );

      "new Test t"    >:: (fun _ ->
        assert_equal [(1, IntVal 0); (2, LocsVal 3);
                      (3, ObjVal ("Program", [("result", 1); ("this", 2)])); (4, LocsVal 5);
                      (5, ObjVal ("Test", []))]
          (eval_state [ObjectConstruction ("Test", ("t", None))]
             [("t", 4); ("result", 1); ("this", 2)]
             [("Test",
               ([],
                [MDecl ("Plus1", [Decl (IntegerType, "n")],
                        [Assign (("n", None), ModAdd, Const 1)])]));
              ("Program",
               ([Decl (IntegerType, "result")],
                [MDecl ("main", [],
                        [LocalBlock (ObjectType "Test", "t", Nil,
                                     [ObjectConstruction ("Test", ("t", None));
                                      ObjectDestruction ("Test", ("t", None))],
                                     Nil)])]))]             
             [(1, IntVal 0); (2, LocsVal 3);
              (3, ObjVal ("Program", [("result", 1); ("this", 2)])); (4, IntVal 0)] 
      ) );
      
      "delete Test t"    >:: (fun _ ->
        assert_equal [(1, IntVal 0); (2, LocsVal 3);
                      (3, ObjVal ("Program", [("result", 1); ("this", 2)])); (4, IntVal 0)] 
          (eval_state [ObjectDestruction ("Test", ("t", None))]
             [("t", 4); ("result", 1); ("this", 2)]
             [("Test",
               ([],
                [MDecl ("Plus1", [Decl (IntegerType, "n")],
                        [Assign (("n", None), ModAdd, Const 1)])]));
              ("Program",
               ([Decl (IntegerType, "result")],
                [MDecl ("main", [],
                        [LocalBlock (ObjectType "Test", "t", Nil,
                                     [ObjectConstruction ("Test", ("t", None));
                                      ObjectDestruction ("Test", ("t", None))],
                                     Nil)])]))]
             [(1, IntVal 0); (2, LocsVal 3);
              (3, ObjVal ("Program", [("result", 1); ("this", 2)])); (4, LocsVal 5);
              (5, ObjVal ("Test", []))]          
      ) );
      
      "new int[2] xs"    >:: (fun _ ->
        assert_equal [(1, LocsVec [4; 5]); (2, ObjVal ("Program", [("xs", 1); ("this", 3)])); (3, LocsVal 2); (4, IntVal 0); (5, IntVal 0)]
          (eval_state [ArrayConstruction (("int", Const 2), "xs")]
             [("xs", 1); ("this", 3)]
             [("Program",
               ([Decl (IntegerArrayType, "xs")],
                [MDecl ("main", [],
                        [ArrayConstruction (("int", Const 2), "xs");
                         ArrayDestruction (("int", Const 2), "xs")])]))]             
             [(1, IntVal 0); (2, ObjVal ("Program", [("xs", 1); ("this", 3)]));(3, LocsVal 2)]
      ) );

      "delete int[2] xs"    >:: (fun _ ->
        assert_equal [(1, IntVal 0); (2, ObjVal ("Program", [("xs", 1); ("this", 3)]));(3, LocsVal 2)]
          (eval_state [ArrayDestruction (("int", Const 2), "xs")]
             [("xs", 1); ("this", 3)]
             [("Program",
               ([Decl (IntegerArrayType, "xs")],
                [MDecl ("main", [],
                        [ArrayConstruction (("int", Const 2), "xs");
                         ArrayDestruction (("int", Const 2), "xs")])]))]
             [(1, LocsVec [4; 5]); (2, ObjVal ("Program", [("xs", 1); ("this", 3)])); (3, LocsVal 2); (4, IntVal 0); (5, IntVal 0)]) );

      "new Test[2] ts"    >:: (fun _ ->
        assert_equal [(1, IntVal 0); (2, LocsVec [5; 6]); (3, LocsVal 4);
                      (4, ObjVal ("Program", [("result", 1); ("ts", 2); ("this", 3)]));
                      (5, IntVal 0); (6, IntVal 0)]
          (eval_state [ArrayConstruction (("Test", Const 2), "ts")]
             [("result", 1); ("ts", 2); ("this", 3)]
             [("Test",
               ([],
                [MDecl ("Plus1", [Decl (IntegerType, "n")],
                        [Assign (("n", None), ModAdd, Const 1)])]));
              ("Program",
               ([Decl (IntegerType, "result"); Decl (ObjectArrayType "Test", "ts")],
                [MDecl ("main", [],
                        [ArrayConstruction (("Test", Const 2), "ts");
                         ObjectConstruction ("Test", ("ts", Some (Const 0)));
                         ObjectDestruction ("Test", ("ts", Some (Const 0)));
                         ArrayDestruction (("Test", Const 2), "ts");
                         ArrayDestruction (("Test", Const 2), "ts")])]))]
             [(1, IntVal 0); (2, IntVal 0); (3, LocsVal 4);
              (4, ObjVal ("Program", [("result", 1); ("ts", 2); ("this", 3)]))]
      ) );

      "delete Test[2] ts"    >:: (fun _ ->
        assert_equal [(1, IntVal 0); (2, IntVal 0); (3, LocsVal 4);
                      (4, ObjVal ("Program", [("result", 1); ("ts", 2); ("this", 3)]))]
          (eval_state [ArrayDestruction (("Test", Const 2), "ts")]
             [("result", 1); ("ts", 2); ("this", 3)]
             [("Test",
               ([],
                [MDecl ("Plus1", [Decl (IntegerType, "n")],
                        [Assign (("n", None), ModAdd, Const 1)])]));
              ("Program",
               ([Decl (IntegerType, "result"); Decl (ObjectArrayType "Test", "ts")],
                [MDecl ("main", [],
                        [ArrayConstruction (("Test", Const 2), "ts");
                         ObjectConstruction ("Test", ("ts", Some (Const 0)));
                         ObjectDestruction ("Test", ("ts", Some (Const 0)));
                         ArrayDestruction (("Test", Const 2), "ts");
                         ArrayDestruction (("Test", Const 2), "ts")])]))]
             [(1, IntVal 0); (2, LocsVec [5; 6]); (3, LocsVal 4);
              (4, ObjVal ("Program", [("result", 1); ("ts", 2); ("this", 3)]));
              (5, IntVal 0); (6, IntVal 0)]
      ) ); 

      "new Test ts[0]"    >:: (fun _ ->
        assert_equal   [(1, IntVal 0); (2, LocsVec [5; 6]); (3, LocsVal 4);
                        (4, ObjVal ("Program", [("result", 1); ("ts", 2); ("this", 3)]));
                        (5, LocsVal 7); (6, IntVal 0); (7, LocsVal 8); (8, ObjVal ("Test", []))]
          (eval_state [ObjectConstruction ("Test", ("ts", Some (Const 0)))]
             [("result", 1); ("ts", 2); ("this", 3)]
             [("Test",
               ([],
                [MDecl ("Plus1", [Decl (IntegerType, "n")],
                        [Assign (("n", None), ModAdd, Const 1)])]));
              ("Program",
               ([Decl (IntegerType, "result"); Decl (ObjectArrayType "Test", "ts")],
                [MDecl ("main", [],
                        [ArrayConstruction (("Test", Const 2), "ts");
                         ObjectConstruction ("Test", ("ts", Some (Const 0)));
                         ObjectDestruction ("Test", ("ts", Some (Const 0)));
                         ArrayDestruction (("Test", Const 2), "ts");
                         ArrayDestruction (("Test", Const 2), "ts")])]))]
             [(1, IntVal 0); (2, LocsVec [5; 6]); (3, LocsVal 4);
              (4, ObjVal ("Program", [("result", 1); ("ts", 2); ("this", 3)]));
              (5, IntVal 0); (6, IntVal 0)]
      ) );

      "delete Test ts[0]"    >:: (fun _ ->
        assert_equal [(1, IntVal 0); (2, LocsVec [5; 6]); (3, LocsVal 4);
                      (4, ObjVal ("Program", [("result", 1); ("ts", 2); ("this", 3)]));
                      (5, IntVal 0); (6, IntVal 0)]
          (eval_state [ObjectDestruction ("Test", ("ts", Some (Const 0)))]
             [("result", 1); ("ts", 2); ("this", 3)]
             [("Test",
               ([],
                [MDecl ("Plus1", [Decl (IntegerType, "n")],
                        [Assign (("n", None), ModAdd, Const 1)])]));
              ("Program",
               ([Decl (IntegerType, "result"); Decl (ObjectArrayType "Test", "ts")],
                [MDecl ("main", [],
                        [ArrayConstruction (("Test", Const 2), "ts");
                         ObjectConstruction ("Test", ("ts", Some (Const 0)));
                         ObjectDestruction ("Test", ("ts", Some (Const 0)));
                         ArrayDestruction (("Test", Const 2), "ts");
                         ArrayDestruction (("Test", Const 2), "ts")])]))]
             [(1, IntVal 0); (2, LocsVec [5; 6]); (3, LocsVal 4);
              (4, ObjVal ("Program", [("result", 1); ("ts", 2); ("this", 3)]));
              (5, LocsVal 7); (6, IntVal 0); (7, LocsVal 8); (8, ObjVal ("Test", []))]             
      ) );
      
      "copy Test1 t1 t2"    >:: (fun _ ->
        assert_equal [(1, IntVal 0); (2, ObjVal ("Program", [("result", 1); ("this", 3)]));
                      (3, LocsVal 2); (4, ObjVal ("Test1", [])); (5, LocsVal 4);
                      (6, ObjVal ("Test2", [])); (7, LocsVal 4)]
          (eval_state [CopyReference (ObjectType "Test1", ("t1", None), ("t2", None))]
            [("t2", 7); ("t1", 5); ("result", 1); ("this", 3)]
            [("Test1",
              ([],
               [MDecl ("Plus1", [Decl (IntegerType, "n")],
                       [Assign (("n", None), ModAdd, Const 1)])]));
             ("Test2",
              ([],
               [MDecl ("Plus2", [Decl (IntegerType, "n")],
                       [Assign (("n", None), ModAdd, Const 2)])]));
             ("Program",
              ([Decl (IntegerType, "result")],
               [MDecl ("main", [],
                       [ObjectConstruction ("Test1", ("t1", None));
                        ObjectConstruction ("Test2", ("t2", None));
                        CopyReference (ObjectType "Test1", ("t1", None), ("t2", None));
                        UncopyReference (ObjectType "Test1", ("t1", None), ("t2", None))])]))]            
            [(1, IntVal 0); (2, ObjVal ("Program", [("result", 1); ("this", 3)]));
             (3, LocsVal 2); (4, ObjVal ("Test1", [])); (5, LocsVal 4);
             (6, ObjVal ("Test2", [])); (7, LocsVal 6)]          
      ) );

      "uncopy Test1 t1 t2"    >:: (fun _ ->
        assert_equal [(1, IntVal 0); (2, ObjVal ("Program", [("result", 1); ("this", 3)]));
             (3, LocsVal 2); (4, ObjVal ("Test1", [])); (5, LocsVal 4);
             (6, ObjVal ("Test2", [])); (7, LocsVal 6)]          
          (eval_state [UncopyReference (ObjectType "Test1", ("t1", None), ("t2", None))]
            [("t2", 7); ("t1", 5); ("result", 1); ("this", 3)]
            [("Test1",
              ([],
               [MDecl ("Plus1", [Decl (IntegerType, "n")],
                       [Assign (("n", None), ModAdd, Const 1)])]));
             ("Test2",
              ([],
               [MDecl ("Plus2", [Decl (IntegerType, "n")],
                       [Assign (("n", None), ModAdd, Const 2)])]));
             ("Program",
              ([Decl (IntegerType, "result")],
               [MDecl ("main", [],
                       [ObjectConstruction ("Test1", ("t1", None));
                        ObjectConstruction ("Test2", ("t2", None));
                        CopyReference (ObjectType "Test1", ("t1", None), ("t2", None));
                        UncopyReference (ObjectType "Test1", ("t1", None), ("t2", None))])]))]
            [(1, IntVal 0); (2, ObjVal ("Program", [("result", 1); ("this", 3)]));
                      (3, LocsVal 2); (4, ObjVal ("Test1", [])); (5, LocsVal 4);
                      (6, ObjVal ("Test2", [])); (7, LocsVal 4)]            
      ) );

      "local int i = 0 call::Plusi(result) delocal int i = 0"    >:: (fun _ ->
        assert_equal [(1, IntVal 1); (2, ObjVal ("Program", [("result", 1); ("this", 3)]));
                      (3, LocsVal 2); (4, IntVal 1)]
          (eval_state [LocalCall ("Plusi", [("result", None)])]
            [("result", 1); ("this", 3)]
            [("Program",
              ([Decl (IntegerType, "result")],
               [MDecl ("main", [], [LocalCall ("Plusi", [("result", None)])]);
                MDecl ("Plusi", [Decl (IntegerType, "n")],
                       [LocalBlock (IntegerType, "i", Const 1,
                                    [Assign (("n", None), ModAdd, Var "i")], Const 1)])]))]
            [(1, IntVal 0); (2, ObjVal ("Program", [("result", 1); ("this", 3)]));
             (3, LocsVal 2)]          
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
                                 [MDecl ("main", [], [LocalCall ("Plus1", [("result", None)])]);
                                  MDecl ("Plus1", [Decl (IntegerType, "n")],
                                         [Assign (("n", None), ModAdd, Const 1)])])]) ) );
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
                                        [Assign (("n", None), ModAdd, Const 1)])]);
                         CDecl ("Program", None, [Decl (IntegerType, "result")],
                                [MDecl ("main", [],
                                        [LocalBlock (ObjectType "Test", "t", Nil,
                                                     [ObjectConstruction ("Test", ("t", None));
                                                      ObjectCall (("t", None), "Plus1", [("result", None)]);
                                                      ObjectDestruction ("Test", ("t", None))],
                                                     Nil)])])])
      ) );

       "class Program
        int[] xs
        method main()
        new int[2] xs
        xs[0] ^= 1
        xs[1] ^= 2"
      >:: (fun _ ->
        assert_equal [("xs[0]", Value.IntVal 1); ("xs[1]", Value.IntVal 2)]
          (eval_prog  (Prog
                            [CDecl ("Program", None, [Decl (IntegerArrayType, "xs")],
                                    [MDecl ("main", [],
                                            [ArrayConstruction (("int", Const 2), "xs");
                                             Assign (("xs", Some (Const 0)), ModXor, Const 1);
                                             Assign (("xs", Some (Const 1)), ModXor, Const 2)])])])
      ) );

       "class Test
        int test
        method Plus1()
        test += 1
        method get(int n)
        n ^= test

        class Program
        int result
        method main()
        local Test t = nil
        new Test t
        call t::Plus1()
        call t::Plus1()
        call t::get(result)
        delete Test t
        delocal Test t = nil"
      >:: (fun _ ->
        assert_equal [("result", IntVal 2)]
          (eval_prog  (Prog
                         [CDecl ("Test", None, [Decl (IntegerType, "test")],
                                 [MDecl ("Plus1", [], [Assign (("test", None), ModAdd, Const 1)]);
                                  MDecl ("get", [Decl (IntegerType, "n")],
                                         [Assign (("n", None), ModXor, Var "test")])]);
                          CDecl ("Program", None, [Decl (IntegerType, "result")],
                                 [MDecl ("main", [],
                                         [LocalBlock (ObjectType "Test", "t", Nil,
                                                      [ObjectConstruction ("Test", ("t", None));
                                                       ObjectCall (("t", None), "Plus1", []);
                                                       ObjectCall (("t", None), "Plus1", []);
                                                       ObjectCall (("t", None), "get", [("result", None)]);
                                                       ObjectDestruction ("Test", ("t", None))],
                                                      Nil)])])])
       ) );

       "class Super
        int test
        method Plus1()
        test += 1
        method get(int n)
        n ^= test

        class Sub inherits Super
        method Plus2()
        test += 2

        class Program
        int result
        method main()
        local Sub s = nil
        new Sub s
        call s::Plus1()
        call s::Plus2()
        call s::get(result)
        delete Sub s
        delocal Sub s = nil"
      >:: (fun _ ->
        assert_equal [("result", IntVal 3)]
          (eval_prog  (Prog
                         [CDecl ("Super", None, [Decl (IntegerType, "test")],
                                 [MDecl ("Plus1", [], [Assign (("test", None), ModAdd, Const 1)]);
                                  MDecl ("get", [Decl (IntegerType, "n")],
                                         [Assign (("n", None), ModXor, Var "test")])]);
                          CDecl ("Sub", Some "Super", [],
                                 [MDecl ("Plus2", [], [Assign (("test", None), ModAdd, Const 2)])]);
                          CDecl ("Program", None, [Decl (IntegerType, "result")],
                                 [MDecl ("main", [],
                                         [LocalBlock (ObjectType "Sub", "s", Nil,
                                                      [ObjectConstruction ("Sub", ("s", None));
                                                       ObjectCall (("s", None), "Plus1", []);
                                                       ObjectCall (("s", None), "Plus2", []);
                                                       ObjectCall (("s", None), "get", [("result", None)]);
                                                       ObjectDestruction ("Sub", ("s", None))],
                                                      Nil)])])]
             )
       ) );

       "class Super
        method Plus1(int n)
        n += 1

        class Sub inherits Super
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
           (eval_prog  (Prog
                          [CDecl ("Super", None, [],
                                  [MDecl ("Plus1", [Decl (IntegerType, "n")],
                                          [Assign (("n", None), ModAdd, Const 1)])]);
                           CDecl ("Sub", Some "Super", [],
                                  [MDecl ("Plus2", [Decl (IntegerType, "n")],
                                          [Assign (("n", None), ModAdd, Const 2)])]);
                           CDecl ("Program", None,
                                  [Decl (ObjectArrayType "Test", "ts"); Decl (IntegerType, "result")],
                                  [MDecl ("main", [],
                                          [ArrayConstruction (("Super", Const 2), "ts");
                                           ObjectConstruction ("Sub", ("ts", Some (Const 0)));
                                           ObjectCall (("ts", Some (Const 0)), "Plus1", [("result", None)]);
                                           ObjectCall (("ts", Some (Const 0)), "Plus2", [("result", None)]);
                                           ObjectDestruction ("Sub", ("ts", Some (Const 0)));
                                           ArrayDestruction (("Super", Const 2), "ts")])])]
              )
       ) );
       
      ]
          
let _ = run_test_tt_main tests
