open OUnit
open Value
open Print

let tests = "test suite for print.ml" >::: [
      "result = IntVal 1"  >:: (fun _ -> assert_equal (print_endline "result = 1") (print_result [("result", IntVal 1)]) );

      "result = LocsVal 1"  >:: (fun _ -> assert_equal (print_endline "x = location 1") (print_result [("x", LocsVal 1)]) );

      "x[0] = 1 x[1] = 2 x[2] = 3"  >:: (fun _ -> assert_equal (print_endline "x[0] = 1\nx[1] = 2\nx[2] = 3") (print_result [("xs[0]", IntVal 1); ("xs[1]", IntVal 2); ("xs[2]", IntVal 3)]
) );      

    ]

let _ = run_test_tt_main tests                                             
