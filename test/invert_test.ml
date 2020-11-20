open OUnit
open Syntax
open Invert

let tests = "test suite for invert.ml" >::: [
      "Skip"    >:: (fun _ -> assert_equal [Skip] (invert [Skip]) );
      
      "x += 1"  >:: (fun _ -> assert_equal [Assign(VarArray("x", None), ModSub, Const 1)] (invert [Assign(VarArray("x", None), ModAdd, Const 1)]) );

      "x -= 1"  >:: (fun _ -> assert_equal [Assign(VarArray("x", None), ModAdd, Const 1)] (invert [Assign(VarArray("x", None), ModSub, Const 1)]) );

      "x += 1"  >:: (fun _ -> assert_equal [Assign(VarArray("x", None), ModXor, Const 1)] (invert [Assign(VarArray("x", None), ModXor, Const 1)]) );

      "x <=> y"  >:: (fun _ -> assert_equal [Swap(VarArray("x", None), VarArray("y", None))] (invert [Swap(VarArray("x", None), VarArray("y", None))]) );

      "x[0] <=> x[1]"  >:: (fun _ -> assert_equal [Swap(VarArray("x", Some(Const 0)), VarArray("x", Some(Const 1)))] (invert [Swap(VarArray("x", Some(Const 0)), VarArray("x", Some(Const 1)))]) );

      "if x = 0 then x += 1 else x -= 1 fi x = 1"  >::
        (fun _ -> assert_equal [Conditional (Binary (Eq, Var "x", Const 1),
                                             [Assign (VarArray("x", None), ModSub, Const 1)],
                                             [Assign (VarArray("x", None), ModAdd, Const 1)],
                                             Binary (Eq, Var "x", Const 0))]
                    (invert [Conditional (Binary (Eq, Var "x", Const 0),
                                          [Assign (VarArray("x", None), ModAdd, Const 1)],
                                          [Assign (VarArray("x", None), ModSub, Const 1)],
                                          Binary (Eq, Var "x", Const 1))] ) );

      "from x = 0 do x += 1 loop x += 2 until x > 10"  >::
        (fun _ -> assert_equal [Loop (Binary (Gt, Var "x", Const 10),
                                      [Assign (VarArray("x", None), ModSub, Const 1)],
                                      [Assign (VarArray("x", None), ModSub, Const 2)],
                                      Binary (Eq, Var "x", Const 0))]
                    (invert [Loop (Binary (Eq, Var "x", Const 0),
                                   [Assign (VarArray("x", None), ModAdd, Const 1)],
                                   [Assign (VarArray("x", None), ModAdd, Const 2)],
                                   Binary (Gt, Var "x", Const 10))] ) );
      "for i in (1..10) do x += i end"  >::
        (fun _ -> assert_equal [For ("i", Const 10, Const 1, [Assign (VarArray ("x", None), ModSub, Var "i")])]
         (invert [For ("i", Const 1, Const 10, [Assign (VarArray ("x", None), ModAdd, Var "i")])] ) );
      "switch x(1) case 1: x += 1 esac 2 break case 2: x += 2 esac 4 break default: x += 10 break"  >::
      (fun _ -> assert_equal [Switch (VarArray ("x", None), [(Case, Const 1, [Assign (VarArray ("x", None), ModAdd, Const 1)], Const 2, Break);
                                      (Case, Const 2, [Assign (VarArray ("x", None), ModAdd, Const 2)], Const 4, Break)], 
                                      [Assign (VarArray ("x", None), ModAdd, Const 10)], VarArray ("x", None))    ]
         (invert [Switch (VarArray ("x", None), [(Case, Const 2, [Assign (VarArray ("x", None), ModSub, Const 1)], Const 1, Break);
                          (Case, Const 4, [Assign (VarArray ("x", None), ModSub, Const 2)], Const 2, Break)],
                          [Assign (VarArray ("x", None), ModSub, Const 10)], VarArray ("x", None))] ) );
      "switch x fcase 1: x += 2 esac 11 fcase 2: x += 3 esac 10 ecase 3: x += 5 esac 8 break default : skip break hctiws x"  >::
      (fun _ -> assert_equal [Switch (VarArray ("x", None), [(Ecase, Const 8, [Assign (VarArray ("x", None), ModSub, Const 5)],
                                                              Const 3, NoBreak); (Fcase, Const 10, [Assign (VarArray ("x", None), ModSub, Const 3)],
                                                                Const 2, NoBreak); (Fcase, Const 11, [Assign (VarArray ("x", None), ModSub, Const 2)], Const 1, Break)], [Skip], VarArray ("x", None))]
       (invert [Switch (VarArray ("x", None), [(Fcase, Const 1, [Assign (VarArray ("x", None), ModAdd, Const 2)],
                                                Const 11, NoBreak); (Fcase, Const 2, [Assign (VarArray ("x", None), ModAdd, Const 3)],
                                                  Const 10, NoBreak); (Ecase, Const 3, [Assign (VarArray ("x", None), ModAdd, Const 5)],
                                                    Const 8, Break)], [Skip], VarArray ("x", None))] ) );

      "construct Test t x += 1 destruct Test"  >::
        (fun _ -> assert_equal [ObjectBlock ("Test", "t", [Assign (VarArray("x", None), ModSub, Const 1)])]
                    (invert [ObjectBlock ("Test", "t", [Assign (VarArray("x", None), ModAdd, Const 1)])] ) );

      "local int i = 1 + 1 i += 1 delocal int i = 3 - 1"  >::
        (fun _ -> assert_equal [LocalBlock (IntegerType, "i", Binary (Sub, Const 3, Const 1),
                                            [Assign (VarArray("i", None), ModSub, Const 1)], Binary (Add, Const 1, Const 1))]
                    (invert [LocalBlock (IntegerType, "i", Binary (Add, Const 1, Const 1),
                                         [Assign (VarArray("i", None), ModAdd, Const 1)], Binary (Sub, Const 3, Const 1))] ) );

      "call Plus1(result)"  >::
        (fun _ -> assert_equal [LocalUncall ("Plus1", [Id "result"])]
                    (invert [LocalCall ("Plus1", [Id "result"])] ) );

      "uncall Plus1(result)"  >::
        (fun _ -> assert_equal [LocalCall ("Plus1", [Id "result"])]
                    (invert [LocalUncall ("Plus1", [Id "result"])] ) );

      "call t::Plus1(result)"  >::
        (fun _ -> assert_equal [ObjectUncall (VarArray("t", None), "Plus1", [Id "result"])]
                    (invert [ObjectCall (VarArray("t", None), "Plus1", [Id "result"])]) );

      "uncall t::Plus1(result)"  >::
        (fun _ -> assert_equal [ObjectCall (VarArray("t", None), "Plus1", [Id "result"])]
                    (invert [ObjectUncall (VarArray("t", None), "Plus1", [Id "result"])]) );
      
      "new Test t"  >::
        (fun _ -> assert_equal [ObjectDestruction ("Test", VarArray("t", None))]
                    (invert [ObjectConstruction ("Test", VarArray("t", None))]) );

      "delete Test t"  >::
        (fun _ -> assert_equal [ObjectConstruction ("Test", VarArray("t", None))]
                    (invert [ObjectDestruction ("Test", VarArray("t", None))]) );

      "copy Test t1 t2"  >::
        (fun _ -> assert_equal [UncopyReference (ObjectType "Test", VarArray("t1", None), VarArray("t2", None))]
                    (invert [CopyReference (ObjectType "Test", VarArray("t1", None), VarArray("t2", None))] ) );

      "uncopy Test t1 t2"  >::
        (fun _ -> assert_equal [CopyReference (ObjectType "Test", VarArray("t1", None), VarArray("t2", None))]
                    (invert [UncopyReference (ObjectType "Test", VarArray("t1", None), VarArray("t2", None))] ) );

      "new int[2] xs"  >::
        (fun _ -> assert_equal [ArrayDestruction (("int", Const 2), VarArray ("xs", None))]
                    (invert [ArrayConstruction (("int", Const 2), VarArray ("xs", None))] ) );
      "delete int[2] xs"  >::
        (fun _ -> assert_equal [ArrayConstruction (("int", Const 2), VarArray ("xs", None))]
                    (invert [ArrayDestruction (("int", Const 2), VarArray ("xs", None))] ) );
      "x += 1 x += 2 x += 3"  >::
        (fun _ -> assert_equal [Assign (VarArray("x", None), ModSub, Const 3);
                                Assign (VarArray("x", None), ModSub, Const 2);
                                Assign (VarArray("x", None), ModSub, Const 1)]
                    (invert [Assign (VarArray("x", None), ModAdd, Const 1);
                             Assign (VarArray("x", None), ModAdd, Const 2);
                             Assign (VarArray("x", None), ModAdd, Const 3)] ) );      
]
          
let _ = run_test_tt_main tests
