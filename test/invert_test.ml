open OUnit2
open Syntax
open Invert

let tests2 = "test suite for invert.ml" >:::
  (List.map
    (fun (title,stm,invstm) ->
      title >:: (fun _ -> assert_equal [stm] (invert [invstm]) ))
    ["Skip", Skip, Skip;
     "x += 1", Assign(VarArray("x", None), ModSub, Const 1), Assign(VarArray("x", None), ModAdd, Const 1);
     "x -= 1", Assign(VarArray("x", None), ModAdd, Const 1), Assign(VarArray("x", None), ModSub, Const 1);
     "x ^= 1", Assign(VarArray("x", None), ModXor, Const 1), Assign(VarArray("x", None), ModXor, Const 1);
     "x <=> y", Swap(VarArray("x", None), VarArray("y", None)), Swap(VarArray("x", None), VarArray("y", None));
     "x[0] <=> x[1]",
     Swap(VarArray("x", Some(Const 0)), VarArray("x", Some(Const 1))),
     Swap(VarArray("x", Some(Const 0)), VarArray("x", Some(Const 1)));
     "if x = 0 then x += 1 else x -= 1 fi x = 1", 
     Conditional (Binary (Eq, Var "x", Const 1),
                  [Assign (VarArray("x", None), ModSub, Const 1)],
                  [Assign (VarArray("x", None), ModAdd, Const 1)],
                  Binary (Eq, Var "x", Const 0)),
     Conditional (Binary (Eq, Var "x", Const 0),
                  [Assign (VarArray("x", None), ModAdd, Const 1)],
                  [Assign (VarArray("x", None), ModSub, Const 1)],
                  Binary (Eq, Var "x", Const 1));
     "from x = 0 do x += 1 loop x += 2 until x > 10",
     Loop (Binary (Gt, Var "x", Const 10),
           [Assign (VarArray("x", None), ModSub, Const 1)],
           [Assign (VarArray("x", None), ModSub, Const 2)],
           Binary (Eq, Var "x", Const 0)),
     Loop (Binary (Eq, Var "x", Const 0),
           [Assign (VarArray("x", None), ModAdd, Const 1)],
           [Assign (VarArray("x", None), ModAdd, Const 2)],
           Binary (Gt, Var "x", Const 10));
     "for i in (1..10) do x += i end",
     For ("i", Const 10, Const 1, [Assign (VarArray ("x", None), ModSub, Var "i")]),
     For ("i", Const 1, Const 10, [Assign (VarArray ("x", None), ModAdd, Var "i")]);
     "construct Test t x += 1 destruct Test",
     ObjectBlock ("Test", "t", [Assign (VarArray("x", None), ModSub, Const 1)]),
     ObjectBlock ("Test", "t", [Assign (VarArray("x", None), ModAdd, Const 1)]);
     "local int i = 1 + 1 i += 1 delocal int i = 3 - 1",
     LocalBlock (IntegerType, "i", Binary (Sub, Const 3, Const 1),
                 [Assign (VarArray("i", None), ModSub, Const 1)], Binary (Add, Const 1, Const 1)),
     LocalBlock (IntegerType, "i", Binary (Add, Const 1, Const 1),
                 [Assign (VarArray("i", None), ModAdd, Const 1)], Binary (Sub, Const 3, Const 1));
     "call Plus1(result)",
     LocalUncall ("Plus1", [Id "result"]),
     LocalCall ("Plus1", [Id "result"]);
     "uncall Plus1(result)", LocalCall ("Plus1", [Id "result"]), LocalUncall ("Plus1", [Id "result"]);
     "call t::Plus1(result)",
     ObjectUncall (VarArray("t", None), "Plus1", [Id "result"]),
     ObjectCall (VarArray("t", None), "Plus1", [Id "result"]);
     "uncall t::Plus1(result)",
     ObjectCall (VarArray("t", None), "Plus1", [Id "result"]),
     ObjectUncall (VarArray("t", None), "Plus1", [Id "result"]);
     "new Test t",
     ObjectDestruction ("Test", VarArray("t", None)),
     ObjectConstruction ("Test", VarArray("t", None));
     "delete Test t",
     ObjectConstruction ("Test", VarArray("t", None)),
     ObjectDestruction ("Test", VarArray("t", None));
     "copy Test t1 t2",
     UncopyReference (ObjectType "Test", VarArray("t1", None), VarArray("t2", None)),
     CopyReference (ObjectType "Test", VarArray("t1", None), VarArray("t2", None));
     "uncopy Test t1 t2",
     CopyReference (ObjectType "Test", VarArray("t1", None), VarArray("t2", None)),
     UncopyReference (ObjectType "Test", VarArray("t1", None), VarArray("t2", None));
     "new int[2] xs",
     ArrayDestruction (("int", Const 2), VarArray ("xs", None)),
     ArrayConstruction (("int", Const 2), VarArray ("xs", None));
     "delete int[2] xs",
     ArrayConstruction (("int", Const 2), VarArray ("xs", None)),
     ArrayDestruction (("int", Const 2), VarArray ("xs", None));
    ])

let tests1 = "test suite for invert.ml" >::: [
      (*"switch x(1) case 1: x += 1 esac 2 break case 2: x += 2 esac 4 break default: x += 10 break"  >::
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
*)
      "x += 1 x += 2 x += 3"  >::
        (fun _ -> assert_equal [Assign (VarArray("x", None), ModSub, Const 3);
                                Assign (VarArray("x", None), ModSub, Const 2);
                                Assign (VarArray("x", None), ModSub, Const 1)]
                    (invert [Assign (VarArray("x", None), ModAdd, Const 1);
                             Assign (VarArray("x", None), ModAdd, Const 2);
                             Assign (VarArray("x", None), ModAdd, Const 3)] ) );      
]
          
(* Involution: inverting twice is the identity. This is the defining law of
   the source-to-source inverter and the basis of the -inverse feature
   (running a program then its inverse must restore the start). tests2 above
   pins down each constructor's single inverse; this suite checks every
   constructor survives a round trip, including the non-trivial Switch
   rewrite (case fall-through / break bookkeeping in invert.ml). *)
let tests3 = "involution: invert (invert s) = s" >:::
  List.map
    (fun (title, stml) ->
      title >:: (fun _ -> assert_equal stml (invert (invert stml))))
    [ "skip", [Skip];
      "assign +=", [Assign(VarArray("x", None), ModAdd, Const 1)];
      "assign ^=", [Assign(VarArray("x", None), ModXor, Var "y")];
      "swap", [Swap(VarArray("x", None), VarArray("y", None))];
      "conditional",
      [Conditional(Binary(Eq, Var "x", Const 0),
                   [Assign(VarArray("x", None), ModAdd, Const 1)],
                   [Skip],
                   Binary(Eq, Var "x", Const 1))];
      "loop",
      [Loop(Binary(Eq, Var "x", Const 0),
            [Skip],
            [Assign(VarArray("x", None), ModAdd, Const 1)],
            Binary(Eq, Var "x", Const 10))];
      "for",
      [For("i", Const 1, Const 10, [Assign(VarArray("x", None), ModAdd, Var "i")])];
      "object block",
      [ObjectBlock("C", "t", [Assign(VarArray("x", None), ModSub, Const 1)])];
      "local block",
      [LocalBlock(IntegerType, "i", Const 0,
                  [Assign(VarArray("i", None), ModAdd, Const 1)], Const 1)];
      "local call", [LocalCall("q", [Id "a"])];
      "local uncall", [LocalUncall("q", [Id "a"])];
      "object call", [ObjectCall(VarArray("t", None), "q", [Id "a"])];
      "object uncall", [ObjectUncall(VarArray("t", None), "q", [Id "a"])];
      "construct", [ObjectConstruction("C", VarArray("t", None))];
      "destruct", [ObjectDestruction("C", VarArray("t", None))];
      "copy", [CopyReference(ObjectType "C", VarArray("a", None), VarArray("b", None))];
      "uncopy", [UncopyReference(ObjectType "C", VarArray("a", None), VarArray("b", None))];
      "array construct", [ArrayConstruction(("int", Const 2), VarArray("xs", None))];
      "array destruct", [ArrayDestruction(("int", Const 2), VarArray("xs", None))];
      "show", [Show(Var "x")];
      "print", [Print "hi"];
      "switch with fall-through",
      [Switch(VarArray("x", None),
              [((Case, [Const 0]), [Print "0"], (Esac, [Const 0], Break));
               ((Case, [Const 1]), [Print "a"], (NoEsac, [], NoBreak));
               ((Case, [Const 2]), [Print "b"], (NoEsac, [], NoBreak));
               ((Case, [Const 3]), [Print "c"], (Esac, [Const 3; Const 2; Const 1], Break))],
              [Skip], VarArray("x", None))];
      "sequence",
      [Assign(VarArray("x", None), ModAdd, Const 1);
       Swap(VarArray("x", None), VarArray("y", None));
       Assign(VarArray("y", None), ModSub, Const 2)];
    ]

let _ =
  run_test_tt_main tests1;
  run_test_tt_main tests2;
  run_test_tt_main tests3;
