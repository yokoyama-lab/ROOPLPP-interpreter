open OUnit2
open Syntax

(* Rocq から抽出した検証済みインタプリタ（coq/extracted/rooplRun.ml）と
   この処理系（lib/eval.ml）の差分テスト。

   coq/roopl.v の `run` は `run_sound` で意味論に対して健全であることが
   証明されている。したがってここで一致を確認できた範囲については、
   OCaml 実装の振る舞いが機械検証された意味論に裏づけられたことになる。

   `run` が扱うのは整数の断片（skip・可逆代入・swap・並び・条件分岐・
   ループ・局所ブロック・引数つき call/uncall）。オブジェクトブロックは
   「全フィールドがゼロ」という決定不能な条件を含むため run は None を返す
   （coq/README.md 参照）。 *)

module R = RooplRun

(* ---- OCaml の int と抽出された nat / z の変換 ------------------------ *)

let rec nat_of_int (n : int) : R.nat = if n <= 0 then R.O else R.S (nat_of_int (n - 1))

let rec positive_of_int (n : int) : R.positive =
  if n = 1 then R.XH
  else if n mod 2 = 0 then R.XO (positive_of_int (n / 2))
  else R.XI (positive_of_int (n / 2))

let z_of_int (n : int) : R.z =
  if n = 0 then R.Z0
  else if n > 0 then R.Zpos (positive_of_int n)
  else R.Zneg (positive_of_int (-n))

let rec int_of_positive (p : R.positive) : int =
  match p with
  | R.XH -> 1
  | R.XO q -> 2 * int_of_positive q
  | R.XI q -> 2 * int_of_positive q + 1

let int_of_z (z : R.z) : int =
  match z with
  | R.Z0 -> 0
  | R.Zpos p -> int_of_positive p
  | R.Zneg p -> - (int_of_positive p)

(* ---- 形式側の AST から実装側の AST へ -------------------------------- *)

let name_of_id (i : R.nat) : string =
  let rec go = function R.O -> 0 | R.S n -> 1 + go n in
  "v" ^ string_of_int (go i)

let rec exp_of_formal (e : R.exp) : exp =
  match e with
  | R.Cst z -> Const (int_of_z z)
  | R.Var x -> Var (name_of_id x)
  | R.Bop (o, e1, e2) ->
     let op = match o with
       | R.Oadd -> Add | R.Osub -> Sub | R.Omul -> Mul
       | R.Oeq -> Eq | R.Olt -> Lt
     in
     Binary (op, exp_of_formal e1, exp_of_formal e2)
  | R.Fld _ | R.Idx _ -> failwith "not in the integer fragment"

let obj_of_id (x : R.nat) : obj = VarArray (name_of_id x, None)

let modop_of_formal = function
  | R.MAdd -> ModAdd | R.MSub -> ModSub | R.MXor -> ModXor

let rec stm_of_formal (s : R.stm) : stm =
  match s with
  | R.Sskip -> Skip
  | R.Sassign (x, o, e) -> Assign (obj_of_id x, modop_of_formal o, exp_of_formal e)
  | R.Sswap (x, y) -> Swap (obj_of_id x, obj_of_id y)
  | R.Sseq _ -> failwith "sequences are flattened by stms_of_formal"
  | R.Sif (e1, s1, s2, e2) ->
     Conditional (exp_of_formal e1, stms_of_formal s1, stms_of_formal s2,
                  exp_of_formal e2)
  | R.Sloop (e1, s1, s2, e2) ->
     Loop (exp_of_formal e1, stms_of_formal s1, stms_of_formal s2,
           exp_of_formal e2)
  | R.Slocal (x, e1, s', e2) ->
     LocalBlock (IntegerType, name_of_id x, exp_of_formal e1,
                 stms_of_formal s', exp_of_formal e2)
  | R.Scall (m, args) ->
     LocalCall ("m" ^ string_of_int (int_of_nat m),
                List.map (fun a -> Id (name_of_id a)) args)
  | R.Suncall (m, args) ->
     LocalUncall ("m" ^ string_of_int (int_of_nat m),
                  List.map (fun a -> Id (name_of_id a)) args)
  | _ -> failwith "not in the integer fragment"

and int_of_nat (n : R.nat) : int =
  let rec go = function R.O -> 0 | R.S m -> 1 + go m in go n

and stms_of_formal (s : R.stm) : stm list =
  match s with
  | R.Sseq (s1, s2) -> stms_of_formal s1 @ stms_of_formal s2
  | s -> [ stm_of_formal s ]

(* ---- 両エンジンの実行 ------------------------------------------------ *)

let zero_state : R.state =
  { R.vs = (fun _ -> R.Z0); R.os = (fun _ -> None); R.hn = R.O;
    R.hp = (fun _ _ -> R.Z0); R.hc = (fun _ -> R.O) }

let empty_menv : R.menv = { R.procs = (fun _ -> None); R.classes = (fun _ -> None) }

(* 検証済みインタプリタで走らせ、指定した変数の値を読む *)
let run_verified ?(env = empty_menv) (s : R.stm) (vars : int list) : int list option =
  match R.run (nat_of_int 20000) env s zero_state with
  | None -> None
  | Some st -> Some (List.map (fun v -> int_of_z (st.R.vs (nat_of_int v))) vars)

(* 同じプログラムをこの処理系で走らせる *)
let run_interpreter_stms ?(methods = []) (stms : stm list) (vars : int list)
    : int list option =
  let fields = List.map (fun v -> Decl (IntegerType, "v" ^ string_of_int v)) vars in
  let main = MDecl ("main", [], stms) in
  let prog = Prog [ CDecl ("Program", None, fields, main :: methods) ] in
  match (try Some (Eval.eval_prog prog) with
         | Util.Runtime_error _ | Failure _ -> None) with
  | None -> None
  | Some result ->
     Some (List.map
             (fun v ->
               match List.assoc_opt ("v" ^ string_of_int v) result with
               | Some (Value.IntVal n) -> n
               | _ -> assert_failure ("missing variable v" ^ string_of_int v))
             vars)

let run_interpreter ?(methods = []) (s : R.stm) (vars : int list) : int list option =
  run_interpreter_stms ~methods (stms_of_formal s) vars

(* 実装が投げたエラーメッセージ（成功したら None） *)
let interpreter_error (stms : stm list) (vars : int list) : string option =
  let fields = List.map (fun v -> Decl (IntegerType, "v" ^ string_of_int v)) vars in
  let prog =
    Prog [ CDecl ("Program", None, fields, [ MDecl ("main", [], stms) ]) ] in
  match Eval.eval_prog prog with
  | _ -> None
  | exception Util.Runtime_error m -> Some m
  | exception Failure m -> Some m

let printer = function
  | None -> "None"
  | Some l -> "[" ^ String.concat "; " (List.map string_of_int l) ^ "]"

(* 形式側と実装側が同じ結果になることを確認する *)
let agree ?(env = empty_menv) ?(methods = []) name (s : R.stm) (vars : int list) =
  name >:: (fun _ ->
    assert_equal ~printer
      (run_interpreter ~methods s vars) (run_verified ~env s vars))

(* ---- テストするプログラム -------------------------------------------- *)

let v n = nat_of_int n
let c n = R.Cst (z_of_int n)
let var n = R.Var (v n)

(* v0 += 3 ; v1 += v0 * 2 *)
let p_arith =
  R.Sseq (R.Sassign (v 0, R.MAdd, c 3),
          R.Sassign (v 1, R.MAdd, R.Bop (R.Omul, var 0, c 2)))

(* v0 += 5 ; v0 <=> v1 *)
let p_swap = R.Sseq (R.Sassign (v 0, R.MAdd, c 5), R.Sswap (v 0, v 1))

(* v0 += 7 ; v0 ^= 3 *)
let p_xor = R.Sseq (R.Sassign (v 0, R.MAdd, c 7), R.Sassign (v 0, R.MXor, c 3))

(* if v0 = 0 then v1 += 1 else v1 += 2 fi v1 = 1 *)
let p_if_true =
  R.Sif (R.Bop (R.Oeq, var 0, c 0),
         R.Sassign (v 1, R.MAdd, c 1),
         R.Sassign (v 1, R.MAdd, c 2),
         R.Bop (R.Oeq, var 1, c 1))

(* 出口表明が合わない条件分岐（両方で失敗するはず） *)
let p_if_bad =
  R.Sif (R.Bop (R.Oeq, var 0, c 0),
         R.Sassign (v 1, R.MAdd, c 1),
         R.Sassign (v 1, R.MAdd, c 2),
         R.Bop (R.Oeq, var 1, c 2))

(* from v0 = 0 do skip loop v0 += 1 until v0 = 5 *)
let p_loop =
  R.Sloop (R.Bop (R.Oeq, var 0, c 0), R.Sskip,
           R.Sassign (v 0, R.MAdd, c 1),
           R.Bop (R.Oeq, var 0, c 5))

(* 入口表明が最初から偽なループ（両方で失敗するはず） *)
let p_loop_bad =
  R.Sseq (R.Sassign (v 0, R.MAdd, c 3),
          R.Sloop (R.Bop (R.Oeq, var 0, c 0), R.Sskip,
                   R.Sassign (v 0, R.MAdd, c 1),
                   R.Bop (R.Oeq, var 0, c 5)))

(* local v2 = 3  v0 += v2  delocal v2 = 3 *)
let p_local =
  R.Slocal (v 2, c 3, R.Sassign (v 0, R.MAdd, var 2), c 3)

(* delocal の値が合わない（両方で失敗するはず） *)
let p_local_bad =
  R.Slocal (v 2, c 0,
            R.Sseq (R.Sassign (v 2, R.MAdd, c 3), R.Sassign (v 0, R.MAdd, var 2)),
            c 0)

(* 入れ子のループと局所ブロック: 三角数 *)
let p_nested =
  R.Slocal (v 2, c 0,
    R.Sloop (R.Bop (R.Oeq, var 2, c 0), R.Sskip,
             R.Sseq (R.Sassign (v 2, R.MAdd, c 1),
                     R.Sassign (v 0, R.MAdd, var 2)),
             R.Bop (R.Oeq, var 2, c 4)),
    c 4)

(* method m0(v3)  v3 += 1  を call / uncall する *)
let bump_body = R.Sassign (v 3, R.MAdd, c 1)
let menv_bump : R.menv =
  { R.procs = (fun m -> if int_of_nat m = 0 then Some (R.MDecl ([ v 3 ], bump_body))
                        else None);
    R.classes = (fun _ -> None) }
let methods_bump =
  [ MDecl ("m0", [ Decl (IntegerType, "v3") ], [ Assign (VarArray ("v3", None), ModAdd, Const 1) ]) ]

let p_call = R.Scall (nat_of_int 0, [ v 0 ])
let p_call_uncall = R.Sseq (R.Scall (nat_of_int 0, [ v 0 ]),
                            R.Suncall (nat_of_int 0, [ v 0 ]))

(* ---- for / switch：実装の追加構文と、形式化での糖衣を突き合わせる ------

   coq/roopl.v は for と switch を原始構文ではなく既存構文への糖衣として
   与えている（for_up / for_down / rev_switch）。ここでは実装側の
   For / Switch と、抽出したその糖衣とが同じ計算をすることを確かめる。
   一致しない場合（実装の側が検査を省いている場合）も、どう食い違うかを
   テストとして固定しておく。 *)

let ovar n = VarArray ("v" ^ string_of_int n, None)
let oid n = "v" ^ string_of_int n

(* 実装側と形式側を、それぞれ別に組み立てて突き合わせる *)
let agree_sugar name (stms : stm list) (s : R.stm) (vars : int list) =
  name >:: (fun _ ->
    assert_equal ~printer (run_interpreter_stms stms vars) (run_verified s vars))

(* for v2 in (1..3) do v0 += v2 end  →  v0 = 6 *)
let for_body_o = [ Assign (ovar 0, ModAdd, Var (oid 2)) ]
let for_body_r = R.Sassign (v 0, R.MAdd, var 2)

let o_for_up = [ For (oid 2, Const 1, Const 3, for_body_o) ]
let r_for_up = R.for_up (v 2) (c 1) (c 3) for_body_r

(* 降順 for v2 in (3..1) *)
let o_for_down = [ For (oid 2, Const 3, Const 1, for_body_o) ]
let r_for_down = R.for_down (v 2) (c 3) (c 1) for_body_r

(* switch v0  case 1: v1 += 10 esac 10 break | case 2: v1 += 20 esac 20 break
   （v0 = 2 なので二番目の枝を通る） *)
let o_switch =
  [ Assign (ovar 0, ModAdd, Const 2);
    Switch (ovar 0,
            [ ((Case, [ Const 1 ]), [ Assign (ovar 1, ModAdd, Const 10) ],
               (Esac, [ Const 10 ], Break));
              ((Case, [ Const 2 ]), [ Assign (ovar 1, ModAdd, Const 20) ],
               (Esac, [ Const 20 ], Break)) ],
            [], ovar 1) ]

let r_switch_cases =
  [ ((z_of_int 1, R.Sassign (v 1, R.MAdd, c 10)), z_of_int 10);
    ((z_of_int 2, R.Sassign (v 1, R.MAdd, c 20)), z_of_int 20) ]

let r_switch =
  R.Sseq (R.Sassign (v 0, R.MAdd, c 2),
          R.rev_switch (v 0) r_switch_cases R.Sskip (v 1))

(* 出口の値が枝どうしで重複している switch。実装は通してしまうが、
   糖衣は「通らなかった枝の出口表明が偽であること」を検査するので落ちる。
   出口の値が枝を識別できなければ逆向きの実行が枝を選び直せないので、
   落ちる側が正しい。 *)
let o_switch_dup =
  [ Assign (ovar 0, ModAdd, Const 2);
    Switch (ovar 0,
            [ ((Case, [ Const 1 ]), [ Assign (ovar 1, ModAdd, Const 10) ],
               (Esac, [ Const 10 ], Break));
              ((Case, [ Const 2 ]), [ Assign (ovar 1, ModAdd, Const 10) ],
               (Esac, [ Const 10 ], Break)) ],
            [], ovar 1) ]

let r_switch_dup =
  R.Sseq (R.Sassign (v 0, R.MAdd, c 2),
          R.rev_switch (v 0)
            [ ((z_of_int 1, R.Sassign (v 1, R.MAdd, c 10)), z_of_int 10);
              ((z_of_int 2, R.Sassign (v 1, R.MAdd, c 10)), z_of_int 10) ]
            R.Sskip (v 1))

(* 体がループ変数を書き換える for。実装はループ変数の不変性を
   「最初の 1 周」でしか検査しないので、2 周目以降の書き換えを見逃して
   完走する。糖衣（＝意味論）ではループが止まらない。 *)
let bad_for_body_o =
  [ Conditional (Binary (Eq, Var (oid 2), Const 2),
                 [ Assign (ovar 2, ModAdd, Const 5) ], [],
                 Binary (Eq, Var (oid 2), Const 7)) ]

let bad_for_body_r =
  R.Sif (R.Bop (R.Oeq, var 2, c 2), R.Sassign (v 2, R.MAdd, c 5), R.Sskip,
         R.Bop (R.Oeq, var 2, c 7))

let o_for_bad = [ For (oid 2, Const 1, Const 3, bad_for_body_o) ]
let r_for_bad = R.for_up (v 2) (c 1) (c 3) bad_for_body_r

let suite = "test suite for the extracted verified interpreter" >::: [
      agree "arithmetic" p_arith [ 0; 1 ];
      agree "swap" p_swap [ 0; 1 ];
      agree "xor" p_xor [ 0 ];
      agree "conditional (then branch)" p_if_true [ 0; 1 ];
      agree "conditional with a wrong exit assertion" p_if_bad [ 0; 1 ];
      agree "loop" p_loop [ 0 ];
      agree "loop with a false entry assertion" p_loop_bad [ 0 ];
      agree "local block" p_local [ 0 ];
      agree "local block with a wrong delocal value" p_local_bad [ 0 ];
      agree "nested loop and local block" p_nested [ 0 ];
      agree ~env:menv_bump ~methods:methods_bump "call" p_call [ 0 ];
      agree ~env:menv_bump ~methods:methods_bump "call then uncall" p_call_uncall [ 0 ];

      (* 検証済みインタプリタが実際に走っていること（全部 None なら無意味） *)
      "the verified interpreter actually computes" >:: (fun _ ->
        assert_equal ~printer (Some [ 3; 6 ]) (run_verified p_arith [ 0; 1 ]);
        assert_equal ~printer (Some [ 5 ]) (run_verified p_loop [ 0 ]);
        assert_equal ~printer (Some [ 10 ]) (run_verified p_nested [ 0 ]);
        (* 糖衣も実際に計算している（両方 None での「一致」を防ぐ） *)
        assert_equal ~printer (Some [ 6 ]) (run_verified r_for_up [ 0 ]);
        assert_equal ~printer (Some [ 6 ]) (run_verified r_for_down [ 0 ]);
        assert_equal ~printer (Some [ 2; 20 ]) (run_verified r_switch [ 0; 1 ]));

      (* 可逆性: 逆プログラムを走らせると元に戻る（run_invert / run_injective） *)
      "running the inverse undoes the program" >:: (fun _ ->
        match R.run (nat_of_int 20000) empty_menv p_arith zero_state with
        | None -> assert_failure "forward run failed"
        | Some st ->
           (match R.run (nat_of_int 20000) empty_menv (R.invert p_arith) st with
            | None -> assert_failure "backward run failed"
            | Some st2 ->
               assert_equal ~printer:string_of_int 0 (int_of_z (st2.R.vs (v 0)));
               assert_equal ~printer:string_of_int 0 (int_of_z (st2.R.vs (v 1)))));

      (* for / switch は糖衣として形式化されている（coq/roopl.v） *)
      agree_sugar "for (ascending) matches its desugaring" o_for_up r_for_up [ 0 ];
      agree_sugar "for (descending) matches its desugaring" o_for_down r_for_down [ 0 ];
      agree_sugar "switch matches its desugaring" o_switch r_switch [ 0; 1 ];

      (* 実装が検査を省いていた 2 か所。実装側に検査を足したので、
         いまは意味論と同じく落ちる（どちらも None）。 *)
      agree_sugar "switch with duplicated exit values is rejected"
        o_switch_dup r_switch_dup [ 0; 1 ];
      agree_sugar "for whose body changes the loop variable is rejected"
        o_for_bad r_for_bad [ 0 ];

      (* 「どちらも None」が偶然の一致でないこと：実装は理由つきで落ちる *)
      "the interpreter explains why it rejects them" >:: (fun _ ->
        (match interpreter_error o_switch_dup [ 0; 1 ] with
         | None -> assert_failure "the switch should have been rejected"
         | Some m ->
            assert_bool ("unexpected message: " ^ m)
              (Diagnostics.contains ~needle:"not taken" m));
        (match interpreter_error o_for_bad [ 0 ] with
         | None -> assert_failure "the for should have been rejected"
         | Some m ->
            assert_bool ("unexpected message: " ^ m)
              (Diagnostics.contains ~needle:"must not change" m)));

      (* 形式化の範囲外は None を返す（黙って間違えない） *)
      "statements outside the fragment are rejected" >:: (fun _ ->
        assert_equal ~printer None
          (run_verified (R.Sobj (nat_of_int 0, v 0, R.Sskip)) [ 0 ]));
    ]

let _ = run_test_tt_main suite
