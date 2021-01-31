open OUnit
open Syntax
open Value
open Eval
open Pretty

let l' = 11
let gamma = [("y1", l'); ("y2", 22)]
let l = 1

let mu = [(l', 2020); (22, 2021)]
let v = 123
   
let tests = "test suite for environment" >::: [
      (* γ[x := l](x) = l *)
      "Env 1"  >:: (fun _ -> let gamma' = ext_envs gamma "x" l
                              in assert_equal (lookup_envs "x" gamma') l );

      (* γ[x := l](y1) = γ(y1) if x ≠ y1 *)
      "Env 2a" >:: (fun _ -> let gamma' = ext_envs gamma "x" l
                              in assert_equal (lookup_envs "y1" gamma') (lookup_envs "y1" gamma) );

      (* γ[x := l](y1) = γ(y2) if x ≠ y2 *)
      "Env 2b" >:: (fun _ -> let gamma' = ext_envs gamma "x" l
                              in assert_equal (lookup_envs "y2" gamma') (lookup_envs "y2" gamma) );

      (* μ[l := v](l) = v *)
      "Store 1" >:: (fun _ -> let mu' = ext_map mu l v
                               in assert_equal (lookup_st l mu') v );

      (* μ[l := v](l') = μ(l') if l ≠ l' *)
      "Store 2" >:: (fun _ -> let mu' = ext_map mu l v
                               in assert_equal (lookup_st l' mu') (lookup_st l' mu') );
     ]

let _ = run_test_tt_main tests
