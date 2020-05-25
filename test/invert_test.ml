open OUnit
open Syntax
open Invert

let tests = "test suite for invert.ml" >::: [
      "Skip"    >:: (fun _ -> assert_equal [Skip] (invert [Skip]) );
      
      "x += 1"  >:: (fun _ -> assert_equal [Assign(("x", None), ModSub, Const 1)] (invert [Assign(("x", None), ModAdd, Const 1)]) );

      "x -= 1"  >:: (fun _ -> assert_equal [Assign(("x", None), ModAdd, Const 1)] (invert [Assign(("x", None), ModSub, Const 1)]) );

      "x += 1"  >:: (fun _ -> assert_equal [Assign(("x", None), ModXor, Const 1)] (invert [Assign(("x", None), ModXor, Const 1)]) );

      "x <=> y"  >:: (fun _ -> assert_equal [Swap(("x", None), ("y", None))] (invert [Swap(("x", None), ("y", None))]) );

      "x[0] <=> x[1]"  >:: (fun _ -> assert_equal [Swap(("x", Some(Const 0)), ("x", Some(Const 1)))] (invert [Swap(("x", Some(Const 0)), ("x", Some(Const 1)))]) );

      "if x = 0 then x += 1 else x -= 1 fi x = 1"  >::
        (fun _ -> assert_equal [Conditional (Binary (Eq, Var "x", Const 1),
                                             [Assign (("x", None), ModSub, Const 1)],
                                             [Assign (("x", None), ModAdd, Const 1)],
                                             Binary (Eq, Var "x", Const 0))]
                    (invert [Conditional (Binary (Eq, Var "x", Const 0),
                                          [Assign (("x", None), ModAdd, Const 1)],
                                          [Assign (("x", None), ModSub, Const 1)],
                                          Binary (Eq, Var "x", Const 1))] ) );

      "from x = 0 do x += 1 loop x += 2 until x > 10"  >::
        (fun _ -> assert_equal [Loop (Binary (Gt, Var "x", Const 10),
                                      [Assign (("x", None), ModSub, Const 1)],
                                      [Assign (("x", None), ModSub, Const 2)],
                                      Binary (Eq, Var "x", Const 0))]
                    (invert [Loop (Binary (Eq, Var "x", Const 0),
                                   [Assign (("x", None), ModAdd, Const 1)],
                                   [Assign (("x", None), ModAdd, Const 2)],
                                   Binary (Gt, Var "x", Const 10))] ) );

      "construct Test t x += 1 destruct Test"  >::
        (fun _ -> assert_equal [ObjectBlock ("Test", "t", [Assign (("x", None), ModSub, Const 1)])]
                    (invert [ObjectBlock ("Test", "t", [Assign (("x", None), ModAdd, Const 1)])] ) );

      "local int i = 1 + 1 i += 1 delocal int i = 3 - 1"  >::
        (fun _ -> assert_equal [LocalBlock (IntegerType, "i", Binary (Sub, Const 3, Const 1),
                                            [Assign (("i", None), ModSub, Const 1)], Binary (Add, Const 1, Const 1))]
                    (invert [LocalBlock (IntegerType, "i", Binary (Add, Const 1, Const 1),
                                         [Assign (("i", None), ModAdd, Const 1)], Binary (Sub, Const 3, Const 1))] ) );

      "call Plus1(result)"  >::
        (fun _ -> assert_equal [LocalUncall ("Plus1", [("result", None)])]
                    (invert [LocalCall ("Plus1", [("result", None)])] ) );

      "uncall Plus1(result)"  >::
        (fun _ -> assert_equal [LocalCall ("Plus1", [("result", None)])]
                    (invert [LocalUncall ("Plus1", [("result", None)])] ) );

      "call t::Plus1(result)"  >::
        (fun _ -> assert_equal [ObjectUncall (("t", None), "Plus1", [("result", None)])]
                    (invert [ObjectCall (("t", None), "Plus1", [("result", None)])]) );

      "uncall t::Plus1(result)"  >::
        (fun _ -> assert_equal [ObjectCall (("t", None), "Plus1", [("result", None)])]
                    (invert [ObjectUncall (("t", None), "Plus1", [("result", None)])]) );
      
      "new Test t"  >::
        (fun _ -> assert_equal [ObjectDestruction ("Test", ("t", None))]
                    (invert [ObjectConstruction ("Test", ("t", None))]) );

      "delete Test t"  >::
        (fun _ -> assert_equal [ObjectConstruction ("Test", ("t", None))]
                    (invert [ObjectDestruction ("Test", ("t", None))]) );

      "copy Test t1 t2"  >::
        (fun _ -> assert_equal [UncopyReference (ObjectType "Test", ("t1", None), ("t2", None))]
                    (invert [CopyReference (ObjectType "Test", ("t1", None), ("t2", None))] ) );

      "uncopy Test t1 t2"  >::
        (fun _ -> assert_equal [CopyReference (ObjectType "Test", ("t1", None), ("t2", None))]
                    (invert [UncopyReference (ObjectType "Test", ("t1", None), ("t2", None))] ) );

      "new int[2] xs"  >::
        (fun _ -> assert_equal [ArrayDestruction (("int", Const 2), "xs")]
                    (invert [ArrayConstruction (("int", Const 2), "xs")] ) );
      "delete int[2] xs"  >::
        (fun _ -> assert_equal [ArrayConstruction (("int", Const 2), "xs")]
                    (invert [ArrayDestruction (("int", Const 2), "xs")] ) );
      "x += 1 x += 2 x += 3"  >::
        (fun _ -> assert_equal [Assign (("x", None), ModSub, Const 3);
                                Assign (("x", None), ModSub, Const 2);
                                Assign (("x", None), ModSub, Const 1)]
                    (invert [Assign (("x", None), ModAdd, Const 1);
                             Assign (("x", None), ModAdd, Const 2);
                             Assign (("x", None), ModAdd, Const 3)] ) );      
]
          
let _ = run_test_tt_main tests
