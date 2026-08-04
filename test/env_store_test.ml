open OUnit2
open Value
open Eval

(* ===== Environment test data ===== *)
let l' = 11
let gamma = [("y1", l'); ("y2", 22)]
let l = 1

(* ===== Store test data ===== *)
let mu = [(1, IntVal 10); (2, IntVal 20); (3, IntVal 30)]

let tests = "test suite for environment and store" >::: [
      (* --- Environment --- *)

      (* γ[x := l](x) = l *)
      "Env 1"  >:: (fun _ -> let gamma' = ext_envs gamma "x" l
                              in assert_equal (lookup_envs "x" gamma') l );

      (* γ[x := l](y1) = γ(y1) if x ≠ y1 *)
      "Env 2a" >:: (fun _ -> let gamma' = ext_envs gamma "x" l
                              in assert_equal (lookup_envs "y1" gamma') (lookup_envs "y1" gamma) );

      (* γ[x := l](y2) = γ(y2) if x ≠ y2 *)
      "Env 2b" >:: (fun _ -> let gamma' = ext_envs gamma "x" l
                              in assert_equal (lookup_envs "y2" gamma') (lookup_envs "y2" gamma) );

      (* γ[x := l] overwrites existing binding for x *)
      "Env 3 overwrite" >:: (fun _ ->
        let gamma' = ext_envs gamma "y1" 99 in
        assert_equal (lookup_envs "y1" gamma') 99 );

      (* lookup_envs raises on unbound variable *)
      "Env 4 unbound" >:: (fun _ ->
        assert_raises (Failure "ERROR:unbound variable: z")
          (fun () -> lookup_envs "z" gamma) );

      (* --- Store: ext_st / lookup_st --- *)

      (* μ[l := v](l) = v *)
      "Store 1" >:: (fun _ ->
        let mu' = ext_st mu 4 (IntVal 40) in
        assert_equal (lookup_st 4 mu') (IntVal 40) );

      (* μ[l := v](l') = μ(l') if l ≠ l' *)
      "Store 2" >:: (fun _ ->
        let mu' = ext_st mu 4 (IntVal 40) in
        assert_equal (lookup_st 1 mu') (IntVal 10) );

      (* ext_st overwrites existing location *)
      "Store 3 overwrite" >:: (fun _ ->
        let mu' = ext_st mu 2 (IntVal 99) in
        assert_equal (lookup_st 2 mu') (IntVal 99) );

      (* ext_st keeps store sorted by location *)
      "Store 4 sorted" >:: (fun _ ->
        let mu' = ext_st mu 0 (IntVal 0) in
        let keys = List.map fst mu' in
        assert_equal keys [0; 1; 2; 3] );

      (* lookup_st raises on unbound location *)
      "Store 5 unbound" >:: (fun _ ->
        assert_raises (Failure "ERROR:unbound locations: 99")
          (fun () -> lookup_st 99 mu) );

      (* store with ObjVal *)
      "Store 6 ObjVal" >:: (fun _ ->
        let env_f = [("f1", 10)] in
        let mu' = ext_st mu 4 (ObjVal("MyClass", env_f)) in
        assert_equal (lookup_st 4 mu') (ObjVal("MyClass", env_f)) );

      (* store with LocsVal *)
      "Store 7 LocsVal" >:: (fun _ ->
        let mu' = ext_st mu 4 (LocsVal 5) in
        assert_equal (lookup_st 4 mu') (LocsVal 5) );

      (* store with LocsVec *)
      "Store 8 LocsVec" >:: (fun _ ->
        let mu' = ext_st mu 4 (LocsVec [5; 6; 7]) in
        assert_equal (lookup_st 4 mu') (LocsVec [5; 6; 7]) );

      (* --- ext_st_zero --- *)

      (* ext_st_zero initializes n locations to IntVal(0) *)
      "Store zero 1" >:: (fun _ ->
        let st = ext_st_zero [] 1 3 in
        assert_equal (lookup_st 1 st) (IntVal 0);
        assert_equal (lookup_st 2 st) (IntVal 0);
        assert_equal (lookup_st 3 st) (IntVal 0) );

      (* ext_st_zero with n=0 is identity *)
      "Store zero 2 empty" >:: (fun _ ->
        let st = ext_st_zero mu 5 0 in
        assert_equal st mu );

      (* --- lookup_val --- *)

      (* lookup_val = lookup_st (lookup_envs x env) st *)
      "lookup_val 1" >:: (fun _ ->
        let env = [("x", 2)] in
        assert_equal (lookup_val "x" env mu) (IntVal 20) );

      (* --- lookup_vec --- *)

      (* lookup_vec returns the correct location by index *)
      "lookup_vec 1" >:: (fun _ ->
        let vec = [10; 20; 30] in
        assert_equal (lookup_vec 0 vec) 10;
        assert_equal (lookup_vec 1 vec) 20;
        assert_equal (lookup_vec 2 vec) 30 );

      (* lookup_vec raises on out-of-bounds *)
      "lookup_vec 2 oob" >:: (fun _ ->
        assert_raises (Failure "ERROR:index out of bounds in lookup_vec")
          (fun () -> lookup_vec 3 [10; 20; 30]) );

      (* lookup_vec raises on negative index *)
      "lookup_vec 3 negative" >:: (fun _ ->
        assert_raises (Failure "ERROR:negative index in lookup_vec")
          (fun () -> lookup_vec (-1) [10; 20; 30]) );
     ]

let _ = run_test_tt_main tests
