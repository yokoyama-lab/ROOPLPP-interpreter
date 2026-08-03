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


(* 形式側のフィールド番号は、実装側では名前 f0, f1, … に対応させる *)
let field_name (f : R.nat) : string =
  let rec go = function R.O -> 0 | R.S n -> 1 + go n in
  "f" ^ string_of_int (go f)


let rec exp_of_formal (e : R.exp) : exp =
  match e with
  | R.Cst z -> Const (int_of_z z)
  | R.Var x -> Var (name_of_id x)
  | R.Bop (o, e1, e2) ->
     let op = match o with
       | R.Oadd -> Add | R.Osub -> Sub | R.Oxor -> Xor | R.Omul -> Mul
       | R.Odiv -> Div | R.Omod -> Mod | R.Oband -> Band | R.Obor -> Bor
       | R.Oand -> And | R.Oor -> Or
       | R.Olt -> Lt | R.Ogt -> Gt | R.Oeq -> Eq | R.One -> Ne
       | R.Ole -> Le | R.Oge -> Ge
     in
     Binary (op, exp_of_formal e1, exp_of_formal e2)
  | R.Fld (x, f) -> Dot (Var (name_of_id x), Var (field_name f))
  | R.Idx (x, e) -> ArrayElement (name_of_id x, exp_of_formal e)

let obj_of_id (x : R.nat) : obj = VarArray (name_of_id x, None)

(* 形式側のフィールド番号は、実装側では名前 f0, f1, … に対応させる *)
let field_name (f : R.nat) : string =
  let rec go = function R.O -> 0 | R.S n -> 1 + go n in
  "f" ^ string_of_int (go f)

let obj_field (x : R.nat) (f : R.nat) : obj =
  InstVar (obj_of_id x, VarArray (field_name f, None))

(* 形式側のクラス番号と、実装側の宣言の対応（ハーネスの規約）。

   形式化はオブジェクトと配列を同じ [Sobj] で確保する（配列＝添字が動的な
   オブジェクト）。実装側は表現が別（[ObjVal] と [LocsVec]）なので、
   **クラス番号で読み分ける**ことにする。
     クラス 0 → object class C0（フィールド f0, f1）
     クラス 1 → int[array_len] の配列
   クラス番号 1 のブロックは new int[n] … delete int[n] の 3 文へ写す。 *)
let array_class = 1
let array_len = 4

let class_name (c : R.nat) : string =
  let rec go = function R.O -> 0 | R.S n -> 1 + go n in
  "C" ^ string_of_int (go c)

let is_array_class (c : R.nat) : bool =
  let rec go = function R.O -> 0 | R.S n -> 1 + go n in
  go c = array_class

let modop_of_formal = function
  | R.MAdd -> ModAdd | R.MSub -> ModSub | R.MXor -> ModXor

let rec stm_of_formal (s : R.stm) : stm =
  match s with
  | R.Sskip -> Skip
  | R.Sassign (x, o, e) -> Assign (obj_of_id x, modop_of_formal o, exp_of_formal e)
  | R.Sswap (x, y) -> Swap (obj_of_id x, obj_of_id y)
  | R.Soswap (x, y) -> Swap (obj_of_id x, obj_of_id y)
  | R.Sfassign (x, f, o, e) ->
     Assign (obj_field x f, modop_of_formal o, exp_of_formal e)
  | R.Saassign (x, ei, o, e) ->
     Assign (VarArray (name_of_id x, Some (exp_of_formal ei)),
             modop_of_formal o, exp_of_formal e)
  | R.Saswap (x, e1, y, e2) ->
     Swap (VarArray (name_of_id x, Some (exp_of_formal e1)),
           VarArray (name_of_id y, Some (exp_of_formal e2)))
  | R.Scopy (x, y) ->
     CopyReference (ObjectType "C0", obj_of_id x, obj_of_id y)
  | R.Suncopy (x, y) ->
     UncopyReference (ObjectType "C0", obj_of_id x, obj_of_id y)
  | R.Socall (x, m, args) ->
     ObjectCall (obj_of_id x, "m" ^ string_of_int (int_of_nat m),
                 List.map arg_of_formal args)
  | R.Souncall (x, m, args) ->
     ObjectUncall (obj_of_id x, "m" ^ string_of_int (int_of_nat m),
                   List.map arg_of_formal args)
  | R.Sobj (cl, x, s') when is_array_class cl ->
     (* 配列はブロック構文が無いので new … delete の 3 文に開く *)
     failwith "array blocks are expanded by stms_of_formal"
  | R.Snew (cl, x) -> ObjectConstruction (class_name cl, obj_of_id x)
  | R.Sdelete (cl, x) -> ObjectDestruction (class_name cl, obj_of_id x)
  | R.Sobj (cl, x, s') ->
     ObjectBlock (class_name cl, name_of_id x, stms_of_formal s')
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
     LocalCall ("m" ^ string_of_int (int_of_nat m), List.map arg_of_formal args)
  | R.Suncall (m, args) ->
     LocalUncall ("m" ^ string_of_int (int_of_nat m), List.map arg_of_formal args)
  | _ -> failwith "not in the integer fragment"

(* 実引数：変数は参照渡し、式は値渡し *)
and arg_of_formal (a : R.arg) : arg =
  match a with
  | R.Aref x -> Id (name_of_id x)
  | R.Aval e -> Exp (exp_of_formal e)

and int_of_nat (n : R.nat) : int =
  let rec go = function R.O -> 0 | R.S m -> 1 + go m in go n

and stms_of_formal (s : R.stm) : stm list =
  match s with
  | R.Sseq (s1, s2) -> stms_of_formal s1 @ stms_of_formal s2
  | R.Sobj (cl, x, s') when is_array_class cl ->
     let ty = ("int", Const array_len) in
     [ ArrayConstruction (ty, obj_of_id x) ]
     @ stms_of_formal s'
     @ [ ArrayDestruction (ty, obj_of_id x) ]
  | s -> [ stm_of_formal s ]

(* ---- 両エンジンの実行 ------------------------------------------------ *)

(* オブジェクトブロックの相手になるクラス。形式側は「クラス名＋フィールド番号」
   しか持たないので、実装側では f0.. という名前のフィールドを持つクラスを
   用意しておく（使わないプログラムには影響しない）。 *)
let object_classes : cDecl list =
  [ CDecl ("C0", None,
           [ Decl (IntegerType, "f0"); Decl (IntegerType, "f1") ],
           [ MDecl ("noop", [], [ Skip ]) ]);
    (* 動的束縛のテスト用の階層。C3 は m0 を上書きし、C4 は継承する *)
    CDecl ("C2", None, [],
           [ MDecl ("m0", [ Decl (IntegerType, "v3") ],
                    [ Assign (VarArray ("v3", None), ModAdd, Const 5) ]) ]);
    CDecl ("C3", Some "C2", [],
           [ MDecl ("m0", [ Decl (IntegerType, "v3") ],
                    [ Assign (VarArray ("v3", None), ModAdd, Const 7) ]) ]);
    CDecl ("C4", Some "C2", [], [ MDecl ("noop", [], [ Skip ]) ]) ]

(* 配列とオブジェクトの変数は実装側では宣言が要る（形式側のストアは全域なので
   不要）。ブロック形 construct/destruct は自分で束縛するが、単体の new/delete は
   既にある変数に対して働くため。 *)
let array_fields : decl list =
  [ Decl (IntegerArrayType, "v20"); Decl (IntegerArrayType, "v21");
    Decl (ObjectType "C0", "v5"); Decl (ObjectType "C0", "v6") ]

let zero_state : R.state =
  { R.vs = (fun _ -> R.Z0); R.os = (fun _ -> None); R.hn = R.O;
    R.hp = (fun _ _ -> R.Z0); R.hc = (fun _ -> R.O) }

(* cells は「クラスあたりのセル数」。ハーネスの規約では
   クラス 0 = object class C0（フィールド f0, f1）、クラス 1 = int[array_len]。 *)
let cells_of (c : R.nat) : R.nat =
  let rec go = function R.O -> 0 | R.S n -> 1 + go n in
  nat_of_int (if go c = array_class then array_len else 2)

let empty_menv : R.menv =
  { R.procs = (fun _ -> None); R.classes = (fun _ -> None); R.cells = cells_of }

(* 検証済みインタプリタで走らせ、指定した変数の値を読む *)
(* run は状態と一緒に「書き込んだフィールド番号の上限」を返す。初期状態は
   ヒープが空なので、上限 0 から始めればよい（roopl.v の above_zero_heap）。 *)
let run_verified ?(env = empty_menv) (s : R.stm) (vars : int list) : int list option =
  match R.run (nat_of_int 20000) env s zero_state R.O with
  | None -> None
  | Some (st, _) -> Some (List.map (fun v -> int_of_z (st.R.vs (nat_of_int v))) vars)

(* 同じプログラムをこの処理系で走らせる *)
let run_interpreter_stms ?(methods = []) (stms : stm list) (vars : int list)
    : int list option =
  let fields =
    List.map (fun v -> Decl (IntegerType, "v" ^ string_of_int v)) vars
    @ array_fields in
  let main = MDecl ("main", [], stms) in
  let prog = Prog (CDecl ("Program", None, fields, main :: methods)
                   :: object_classes) in
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
  let fields =
    List.map (fun v -> Decl (IntegerType, "v" ^ string_of_int v)) vars
    @ array_fields in
  let prog =
    Prog (CDecl ("Program", None, fields, [ MDecl ("main", [], stms) ])
          :: object_classes) in
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

let bop o e1 e2 = R.Bop (o, e1, e2)

let rec seqs = function
  | [] -> R.Sskip
  | [ s ] -> s
  | s :: tl -> R.Sseq (s, seqs tl)

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

(* 局所ブロックの入口・出口の式が自分自身を参照する（E_local の x ∉ fv(e)）。
   表明が恒真になるので逆向きの実行が値を復元できない *)
let p_local_self_exit =
  R.Slocal (v 2, c 0, R.Sassign (v 2, R.MAdd, c 3), R.Var (v 2))

let p_local_self_entry =
  R.Slocal (v 2, R.Var (v 2), R.Sassign (v 0, R.MAdd, c 1), c 0)

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
    R.classes = (fun _ -> None); R.cells = cells_of }
let methods_bump =
  [ MDecl ("m0", [ Decl (IntegerType, "v3") ], [ Assign (VarArray ("v3", None), ModAdd, Const 1) ]) ]

let p_call = R.Scall (nat_of_int 0, [ R.Aref (v 0) ])
let p_call_uncall = R.Sseq (R.Scall (nat_of_int 0, [ R.Aref (v 0) ]),
                            R.Suncall (nat_of_int 0, [ R.Aref (v 0) ]))

(* 値渡し：method m1(v3, v4)  v3 += v4  を call m1(v0, 3) と呼ぶ。
   値渡しの仮引数は局所ブロックで包まれるので、本体が書き換えると落ちる。 *)
let addto_body = R.Sassign (v 3, R.MAdd, R.Var (v 4))
let menv_addto : R.menv =
  { R.procs = (fun m -> if int_of_nat m = 1
                        then Some (R.MDecl ([ v 3; v 4 ], addto_body)) else None);
    R.classes = (fun _ -> None); R.cells = cells_of }
let methods_addto =
  [ MDecl ("m1", [ Decl (IntegerType, "v3"); Decl (IntegerType, "v4") ],
           [ Assign (VarArray ("v3", None), ModAdd, Var "v4") ]) ]

let p_call_value = R.Scall (nat_of_int 1, [ R.Aref (v 0); R.Aval (c 3) ])

(* 値渡しの仮引数を書き換える本体（両方で落ちるはず） *)
let bad_body = R.Sassign (v 4, R.MAdd, c 1)
let menv_bad : R.menv =
  { R.procs = (fun m -> if int_of_nat m = 1
                        then Some (R.MDecl ([ v 3; v 4 ], bad_body)) else None);
    R.classes = (fun _ -> None); R.cells = cells_of }
let methods_bad =
  [ MDecl ("m1", [ Decl (IntegerType, "v3"); Decl (IntegerType, "v4") ],
           [ Assign (VarArray ("v4", None), ModAdd, Const 1) ]) ]

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

(* 範囲式がループ変数を指す for。糖衣は局所ブロックなので E_local の
   x ∉ fv(e1), x ∉ fv(e2) がかかり、どちらの端でも落ちる *)
let o_for_range_lo = [ For (oid 2, Var (oid 2), Const 3, for_body_o) ]
let r_for_range_lo = R.for_up (v 2) (R.Var (v 2)) (c 3) for_body_r

let o_for_range_hi = [ For (oid 2, Const 0, Var (oid 2), for_body_o) ]
let r_for_range_hi = R.for_up (v 2) (c 0) (R.Var (v 2)) for_body_r

(* ---- オブジェクトブロック（run が扱えるようになった範囲） -------------

   construct C x … destruct x は「確保 → 体 → 全フィールドのゼロクリア検査
   → 解放」。検証済みインタプリタは、書き込んだフィールド番号の上限を
   持ち回ることでゼロクリア検査を有限の検査に落としている。 *)

(* construct C0 v5  v5.f0 += 3  v0 += v5.f0  v5.f0 -= 3  destruct v5 *)
let p_object =
  R.Sobj (nat_of_int 0, v 5,
          R.Sseq (R.Sfassign (v 5, nat_of_int 0, R.MAdd, c 3),
                  R.Sseq (R.Sassign (v 0, R.MAdd, R.Fld (v 5, nat_of_int 0)),
                          R.Sfassign (v 5, nat_of_int 0, R.MSub, c 3))))

(* 2 つのフィールドを使い、片方を消し忘れる（両方で落ちるはず） *)
let p_object_dirty =
  R.Sobj (nat_of_int 0, v 5,
          R.Sseq (R.Sfassign (v 5, nat_of_int 1, R.MAdd, c 7),
                  R.Sassign (v 0, R.MAdd, R.Fld (v 5, nat_of_int 1))))

(* 入れ子のオブジェクトブロック *)
let p_object_nested =
  R.Sobj (nat_of_int 0, v 5,
          R.Sobj (nat_of_int 0, v 6,
                  R.Sseq (R.Sfassign (v 5, nat_of_int 0, R.MAdd, c 2),
                          R.Sseq (R.Sfassign (v 6, nat_of_int 0, R.MAdd, c 5),
                                  R.Sseq (R.Sassign (v 0, R.MAdd,
                                                     R.Fld (v 6, nat_of_int 0)),
                                          R.Sseq (R.Sfassign (v 6, nat_of_int 0,
                                                              R.MSub, c 5),
                                                  R.Sfassign (v 5, nat_of_int 0,
                                                              R.MAdd, c (-2))))))))

(* オブジェクト参照の swap。相手も束縛されている必要があるので、内側の
   ブロック変数と入れ替えて戻す *)
let p_object_swap =
  R.Sobj (nat_of_int 0, v 5,
          R.Sobj (nat_of_int 0, v 6,
                  R.Sseq (R.Soswap (v 5, v 6), R.Soswap (v 5, v 6))))

(* ---- 配列 -----------------------------------------------------------

   形式化は配列を「添字が動的なオブジェクト」として同じヒープに載せる。
   ハーネスの規約でクラス番号 1 を int[4] の配列に読み替える（上の
   is_array_class）。長さと範囲検査は形式化の対象外で、実装側の動的検査に
   任されている（coq/README.md 参照）。 *)

let arr = v 20

(* new int[4] v20  v20[0] += 3  v0 += v20[0]  v20[0] -= 3  delete *)
let p_array =
  R.Sobj (nat_of_int 1, arr,
          R.Sseq (R.Saassign (arr, c 0, R.MAdd, c 3),
                  R.Sseq (R.Sassign (v 0, R.MAdd, R.Idx (arr, c 0)),
                          R.Saassign (arr, c 0, R.MSub, c 3))))

(* 2 つのセルを使って入れ替え、読み出してから消す *)
let p_array_swap =
  R.Sobj (nat_of_int 1, arr,
          R.Sseq (R.Saassign (arr, c 0, R.MAdd, c 2),
                  R.Sseq (R.Saassign (arr, c 1, R.MAdd, c 9),
                          R.Sseq (R.Saswap (arr, c 0, arr, c 1),
                                  R.Sseq (R.Sassign (v 0, R.MAdd, R.Idx (arr, c 0)),
                                          R.Sseq (R.Saassign (arr, c 0, R.MSub, c 9),
                                                  R.Saassign (arr, c 1, R.MSub, c 2)))))))

(* 消し忘れたセルがある（両方で落ちるはず） *)
let p_array_dirty =
  R.Sobj (nat_of_int 1, arr,
          R.Sseq (R.Saassign (arr, c 2, R.MAdd, c 4),
                  R.Sassign (v 0, R.MAdd, R.Idx (arr, c 2))))

(* 添字が動的（v0 の値で決まる） *)
let p_array_dynamic_index =
  R.Sseq (R.Sassign (v 0, R.MAdd, c 2),
          R.Sobj (nat_of_int 1, arr,
                  R.Sseq (R.Saassign (arr, R.Var (v 0), R.MAdd, c 6),
                          R.Sseq (R.Sassign (v 1, R.MAdd, R.Idx (arr, R.Var (v 0))),
                                  R.Saassign (arr, R.Var (v 0), R.MSub, c 6)))))

(* 範囲外の書き込み。長さはクラス表の cells から引くので意味論も落ちる。 *)
(* 範囲外の読み出し。意味論 exec は許す（可逆性は壊れない）が、実行可能
   インタプリタ run は実装と同じく落ちる（coq/roopl.v の inb）。 *)
let p_array_read_oob =
  R.Sobj (nat_of_int 1, arr,
          R.Sassign (v 0, R.MAdd, R.Idx (arr, c 10)))

let p_array_read_oob_stms = stms_of_formal p_array_read_oob

let p_array_oob_stms = stms_of_formal
  (R.Sobj (nat_of_int 1, arr,
           R.Sseq (R.Saassign (arr, c 10, R.MAdd, c 1),
                   R.Saassign (arr, c 10, R.MSub, c 1))))

let p_array_out_of_bounds =
  R.Sobj (nat_of_int 1, arr,
          R.Sseq (R.Saassign (arr, c 10, R.MAdd, c 1),
                  R.Saassign (arr, c 10, R.MSub, c 1)))

(* ---- 動的束縛 --------------------------------------------------------

   受け手の実行時クラスでメソッドを選ぶ。C3 は m0 を上書き（+7）、
   C4 は C2 から継承（+5）。 *)

let self = v 50
let bump n =
  R.MDecl ([ self; v 3 ], R.Sassign (v 3, R.MAdd, c n))

let menv_dispatch : R.menv =
  { R.procs = (fun _ -> None);
    R.classes = (fun cl ->
      match int_of_nat cl with
      | 2 -> Some (R.CDecl (None,
                            fun m -> if int_of_nat m = 0 then Some (bump 5) else None))
      | 3 -> Some (R.CDecl (Some (nat_of_int 2),
                            fun m -> if int_of_nat m = 0 then Some (bump 7) else None))
      | 4 -> Some (R.CDecl (Some (nat_of_int 2), fun _ -> None))
      | _ -> None);
    R.cells = cells_of }

(* construct C3 v5  call v5::m0(v0)  destruct v5  →  v0 = 7（上書き） *)
let p_dispatch_override =
  R.Sobj (nat_of_int 3, v 5, R.Socall (v 5, nat_of_int 0, [ R.Aref (v 0) ]))

(* construct C4 v5 …  →  v0 = 5（継承） *)
let p_dispatch_inherited =
  R.Sobj (nat_of_int 4, v 5, R.Socall (v 5, nat_of_int 0, [ R.Aref (v 0) ]))

(* call してから uncall すると戻る *)
let p_dispatch_uncall =
  R.Sobj (nat_of_int 3, v 5,
          R.Sseq (R.Socall (v 5, nat_of_int 0, [ R.Aref (v 0) ]),
                  R.Souncall (v 5, nat_of_int 0, [ R.Aref (v 0) ])))

(* ---- ブロックにしない new / delete ------------------------------------

   形式化はブロック形 Sobj に加えて、単体の new / delete も持つ（ヒープは
   スタックなので delete は必ず一番上を解放する）。 *)

(* new C0 v5  v5.f0 += 3  v0 += v5.f0  v5.f0 -= 3  delete C0 v5 *)
let p_new_delete =
  R.Sseq (R.Snew (nat_of_int 0, v 5),
          R.Sseq (R.Sfassign (v 5, nat_of_int 0, R.MAdd, c 3),
                  R.Sseq (R.Sassign (v 0, R.MAdd, R.Fld (v 5, nat_of_int 0)),
                          R.Sseq (R.Sfassign (v 5, nat_of_int 0, R.MSub, c 3),
                                  R.Sdelete (nat_of_int 0, v 5)))))

(* ゼロクリアを忘れて delete する（両方で落ちるはず） *)
let p_delete_dirty =
  R.Sseq (R.Snew (nat_of_int 0, v 5),
          R.Sseq (R.Sfassign (v 5, nat_of_int 0, R.MAdd, c 3),
                  R.Sdelete (nat_of_int 0, v 5)))

(* ---- 二項演算 --------------------------------------------------------

   形式側は lib/syntax.ml の binOp 16 個をすべて持つ。除算と剰余は OCaml と
   同じく 0 方向へ切り捨てる（Z.quot / Z.rem）ので、負の被除数でも一致する
   はずである。 *)

let p_binops =
  seqs [ R.Sassign (v 0, R.MAdd, bop R.Oadd (c 7) (c 2));    (*  9 *)
         R.Sassign (v 0, R.MAdd, bop R.Osub (c 7) (c 2));    (*  5 *)
         R.Sassign (v 0, R.MAdd, bop R.Omul (c 7) (c 2));    (* 14 *)
         R.Sassign (v 0, R.MAdd, bop R.Oxor (c 6) (c 3));    (*  5 *)
         R.Sassign (v 0, R.MAdd, bop R.Oband (c 6) (c 3));   (*  2 *)
         R.Sassign (v 0, R.MAdd, bop R.Obor (c 6) (c 3));    (*  7 *)
         (* 負の被除数：切り捨ての向きが Coq の Z.div/Z.modulo とは違う *)
         R.Sassign (v 1, R.MAdd, bop R.Odiv (c 7) (c 2));    (*  3 *)
         R.Sassign (v 1, R.MAdd, bop R.Omod (c 7) (c 2));    (*  1 *)
         R.Sassign (v 1, R.MAdd, bop R.Odiv (c (-7)) (c 2)); (* -3 *)
         R.Sassign (v 1, R.MAdd, bop R.Omod (c (-7)) (c 2)); (* -1 *)
         R.Sassign (v 2, R.MAdd, bop R.Olt (c 3) (c 2));     (*  0 *)
         R.Sassign (v 2, R.MAdd, bop R.Ogt (c 3) (c 2));     (*  1 *)
         R.Sassign (v 2, R.MAdd, bop R.Oeq (c 3) (c 3));     (*  1 *)
         R.Sassign (v 2, R.MAdd, bop R.One (c 3) (c 3));     (*  0 *)
         R.Sassign (v 2, R.MAdd, bop R.Ole (c 3) (c 3));     (*  1 *)
         R.Sassign (v 2, R.MAdd, bop R.Oge (c 2) (c 3));     (*  0 *)
         R.Sassign (v 3, R.MAdd, bop R.Oand (c 0) (c 5));    (*  0 *)
         R.Sassign (v 3, R.MAdd, bop R.Oand (c 4) (c 5));    (*  1 *)
         R.Sassign (v 3, R.MAdd, bop R.Oor (c 0) (c 0));     (*  0 *)
         R.Sassign (v 3, R.MAdd, bop R.Oor (c 0) (c 5)) ]    (*  1 *)

(* ゼロ除算。意味論 exec は Z.quot a 0 = 0 を許すが、run は inb で塞いである
   （実装は「division by zero」で落ちる） *)
let p_div_zero = R.Sassign (v 0, R.MAdd, bop R.Odiv (c 1) (R.Var (v 1)))
let p_mod_zero = R.Sassign (v 0, R.MAdd, bop R.Omod (c 1) (R.Var (v 1)))

(* ---- 出口の表明の中の式 ----------------------------------------------

   入口の表明だけでなく、条件分岐・ループ・局所ブロックの**出口**の表明の中の
   式も run が検査する。実装は表明を評価するときに同じところで落ちる。 *)

(* 出口の表明がゼロ除算する条件分岐（v1 = 0 のまま） *)
let p_if_exit_div_zero =
  R.Sif (R.Bop (R.Oeq, var 0, c 0),
         R.Sassign (v 2, R.MAdd, c 1),
         R.Sassign (v 2, R.MAdd, c 2),
         R.Bop (R.Oeq, bop R.Odiv (c 1) (R.Var (v 1)), c 1))

(* 出口の表明が範囲外を読むループ *)
let p_loop_exit_oob =
  R.Sobj (nat_of_int 1, arr,
          R.Sloop (R.Bop (R.Oeq, var 0, c 0), R.Sskip,
                   R.Sassign (v 0, R.MAdd, c 1),
                   R.Bop (R.Oeq, R.Idx (arr, c 10), c 1)))

(* 出口の表明がゼロ除算する局所ブロック *)
let p_local_exit_div_zero =
  R.Slocal (v 3, c 3, R.Sskip, bop R.Odiv (c 3) (R.Var (v 1)))

(* ---- 自己代入（E_assign の x ∉ fv(e)、E_aassign の eval e b = eval e a）--

   x += x は逆向きに戻せない。形式側には導出が無く（ex_self_assign_stuck）、
   実装も 2026-08-03 から拒否する。 *)

let p_self_assign =
  R.Sseq (R.Sassign (v 0, R.MAdd, c 3), R.Sassign (v 0, R.MAdd, R.Var (v 0)))

let p_self_sub =
  R.Sseq (R.Sassign (v 0, R.MAdd, c 3), R.Sassign (v 0, R.MSub, R.Var (v 0)))

(* 配列版：書き込みが右辺の値を変えてしまう *)
let p_array_self =
  R.Sobj (nat_of_int 1, arr,
          R.Sseq (R.Saassign (arr, c 0, R.MAdd, c 5),
                  R.Saassign (arr, c 0, R.MAdd, R.Idx (arr, c 0))))

(* 添字が自分の書き込みで動く入れ替え *)
let p_swap_moves_index =
  R.Sobj (nat_of_int 1, arr,
          R.Sseq (R.Saassign (arr, c 0, R.MAdd, c 1),
                  R.Saswap (arr, R.Idx (arr, c 0), arr, c 0)))

(* ---- 負の添字 --------------------------------------------------------
   意味論は Z.to_nat で 0 に丸めるが、run は実装に合わせて拒否する（inbw）。 *)

let p_array_write_negative =
  R.Sobj (nat_of_int 1, arr, R.Saassign (arr, c (-1), R.MAdd, c 1))

let p_array_read_negative =
  R.Sobj (nat_of_int 1, arr, R.Sassign (v 0, R.MAdd, R.Idx (arr, c (-1))))

(* ---- copy / uncopy ---------------------------------------------------
   参照の複製。複製先が nil であること・取り消しの両者が同じ参照であることが
   副条件（どちらも欠けると可逆でなくなる）。 *)

(* copy した別名を通してフィールドを書き、本体から読む *)
let p_copy_uncopy =
  R.Sobj (nat_of_int 0, v 5,
          seqs [ R.Scopy (v 5, v 6);
                 R.Sfassign (v 6, nat_of_int 0, R.MAdd, c 3);
                 R.Sassign (v 0, R.MAdd, R.Fld (v 5, nat_of_int 0));
                 R.Sfassign (v 6, nat_of_int 0, R.MSub, c 3);
                 R.Suncopy (v 5, v 6) ])

(* 複製先が nil でない（両方で落ちるはず） *)
let p_copy_not_nil =
  R.Sobj (nat_of_int 0, v 5,
          R.Sobj (nat_of_int 0, v 6, R.Scopy (v 5, v 6)))

(* 同じ変数への複製（別名禁止の規則そのもの） *)
let p_copy_self = R.Sobj (nat_of_int 0, v 5, R.Scopy (v 5, v 5))

(* uncopy の両者が別の参照（両方で落ちるはず） *)
let p_uncopy_mismatch =
  R.Sobj (nat_of_int 0, v 5,
          R.Sobj (nat_of_int 0, v 6, R.Suncopy (v 5, v 6)))

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
      agree "a delocal expression mentioning its own variable is rejected"
        p_local_self_exit [ 0 ];
      agree "a local expression mentioning its own variable is rejected"
        p_local_self_entry [ 0 ];

      "both engines reject a self-referential local block" >:: (fun _ ->
        assert_equal ~printer None (run_verified p_local_self_exit [ 0 ]);
        assert_equal ~printer None (run_verified p_local_self_entry [ 0 ]));
      agree "nested loop and local block" p_nested [ 0 ];
      agree ~env:menv_bump ~methods:methods_bump "call" p_call [ 0 ];
      agree ~env:menv_bump ~methods:methods_bump "call then uncall" p_call_uncall [ 0 ];

      (* 値渡しの引数 *)
      agree ~env:menv_addto ~methods:methods_addto "call with a value argument"
        p_call_value [ 0 ];
      agree ~env:menv_bad ~methods:methods_bad
        "a body that changes a value argument is rejected" p_call_value [ 0 ];

      "value arguments are bound by a local block" >:: (fun _ ->
        assert_equal ~printer (Some [ 3 ])
          (run_verified ~env:menv_addto p_call_value [ 0 ]);
        assert_equal ~printer None
          (run_verified ~env:menv_bad p_call_value [ 0 ]));

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
        match R.run (nat_of_int 20000) empty_menv p_arith zero_state R.O with
        | None -> assert_failure "forward run failed"
        | Some (st, nf) ->
           (match R.run (nat_of_int 20000) empty_menv (R.invert p_arith) st nf with
            | None -> assert_failure "backward run failed"
            | Some (st2, _) ->
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
      agree_sugar "for whose lower range mentions the loop variable is rejected"
        o_for_range_lo r_for_range_lo [ 0; 2 ];
      agree_sugar "for whose upper range mentions the loop variable is rejected"
        o_for_range_hi r_for_range_hi [ 0; 2 ];

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

      (* オブジェクトブロック *)
      agree "object block (field written and cleared)" p_object [ 0 ];
      agree "object block leaving a field dirty" p_object_dirty [ 0 ];
      agree "nested object blocks" p_object_nested [ 0 ];
      agree "object reference swap inside a block" p_object_swap [ 0 ];

      "the verified interpreter runs object blocks" >:: (fun _ ->
        assert_equal ~printer (Some [ 3 ]) (run_verified p_object [ 0 ]);
        assert_equal ~printer (Some [ 5 ]) (run_verified p_object_nested [ 0 ]);
        assert_equal ~printer (Some [ 3 ]) (run_verified p_new_delete [ 0 ]);
        assert_equal ~printer None (run_verified p_delete_dirty [ 0 ]);
        assert_equal ~printer None (run_verified p_object_dirty [ 0 ]));

      (* ブロックにしない new / delete *)
      agree "new and delete outside a block" p_new_delete [ 0 ];
      agree "delete with a dirty field" p_delete_dirty [ 0 ];

      (* 配列（形式化ではオブジェクト＋動的添字） *)
      agree "array cell written and cleared" p_array [ 0 ];
      agree "array element swap" p_array_swap [ 0 ];
      agree "array left with a dirty cell" p_array_dirty [ 0 ];
      agree "array with a dynamic index" p_array_dynamic_index [ 0; 1 ];

      "the verified interpreter runs arrays" >:: (fun _ ->
        assert_equal ~printer (Some [ 3 ]) (run_verified p_array [ 0 ]);
        assert_equal ~printer (Some [ 9 ]) (run_verified p_array_swap [ 0 ]);
        assert_equal ~printer (Some [ 2; 6 ])
          (run_verified p_array_dynamic_index [ 0; 1 ]);
        assert_equal ~printer None (run_verified p_array_dirty [ 0 ]));

      (* 範囲外アクセスは意味論の側でも落ちる（長さはクラス表の cells から引く）*)
      agree "array write out of bounds is rejected" p_array_out_of_bounds [ 0 ];

      (* 読み出しの範囲外も、実行可能インタプリタは実装と同じく落ちる *)
      agree "array read out of bounds is rejected" p_array_read_oob [ 0 ];

      "the verified interpreter checks array reads too" >:: (fun _ ->
        assert_equal ~printer None (run_verified p_array_read_oob [ 0 ]);
        assert_equal ~printer None
          (run_interpreter_stms p_array_read_oob_stms [ 0 ]));

      "the semantics knows the array length" >:: (fun _ ->
        assert_equal ~printer None (run_verified p_array_out_of_bounds [ 0 ]);
        assert_equal ~printer None (run_interpreter_stms p_array_oob_stms [ 0 ]));

      (* 動的束縛 *)
      agree ~env:menv_dispatch "dynamic dispatch (override)"
        p_dispatch_override [ 0 ];
      agree ~env:menv_dispatch "dynamic dispatch (inherited)"
        p_dispatch_inherited [ 0 ];
      agree ~env:menv_dispatch "dynamic dispatch call then uncall"
        p_dispatch_uncall [ 0 ];

      "the verified interpreter dispatches on the run-time class" >:: (fun _ ->
        assert_equal ~printer (Some [ 7 ])
          (run_verified ~env:menv_dispatch p_dispatch_override [ 0 ]);
        assert_equal ~printer (Some [ 5 ])
          (run_verified ~env:menv_dispatch p_dispatch_inherited [ 0 ]);
        assert_equal ~printer (Some [ 0 ])
          (run_verified ~env:menv_dispatch p_dispatch_uncall [ 0 ]));

      (* 二項演算 16 個 *)
      agree "every binary operator" p_binops [ 0; 1; 2; 3 ];
      agree "division by zero is rejected" p_div_zero [ 0; 1 ];
      agree "modulo by zero is rejected" p_mod_zero [ 0; 1 ];

      (* 出口の表明の中の式も検査する（入口だけでは実装と食い違う） *)
      agree "a conditional whose exit assertion divides by zero is rejected"
        p_if_exit_div_zero [ 0; 1; 2 ];
      agree "a loop whose exit assertion reads out of bounds is rejected"
        p_loop_exit_oob [ 0 ];
      agree "a local block whose exit assertion divides by zero is rejected"
        p_local_exit_div_zero [ 0; 1 ];

      "the exit assertions are checked, not just the entry ones" >:: (fun _ ->
        assert_equal ~printer None (run_verified p_if_exit_div_zero [ 0; 1; 2 ]);
        assert_equal ~printer None (run_verified p_loop_exit_oob [ 0 ]);
        assert_equal ~printer None (run_verified p_local_exit_div_zero [ 0; 1 ]));

      "the operators compute the same numbers on both sides" >:: (fun _ ->
        assert_equal ~printer (Some [ 42; 0; 3; 2 ]) (run_verified p_binops [ 0; 1; 2; 3 ]);
        assert_equal ~printer (Some [ 42; 0; 3; 2 ])
          (run_interpreter p_binops [ 0; 1; 2; 3 ]));

      (* 自己代入（可逆性の副条件） *)
      agree "an assignment whose target occurs on the right is rejected"
        p_self_assign [ 0 ];
      agree "a subtraction whose target occurs on the right is rejected"
        p_self_sub [ 0 ];
      agree "an array assignment that reads the cell it writes is rejected"
        p_array_self [ 0 ];
      agree "a swap that moves its own index is rejected" p_swap_moves_index [ 0 ];

      "both engines reject self-assignment for the same reason" >:: (fun _ ->
        assert_equal ~printer None (run_verified p_self_assign [ 0 ]);
        (match interpreter_error (stms_of_formal p_self_assign) [ 0 ] with
         | None -> assert_failure "the self-assignment should have been rejected"
         | Some m ->
            assert_bool ("unexpected message: " ^ m)
              (Diagnostics.contains ~needle:"must not occur on both sides" m)));

      (* 負の添字 *)
      agree "a negative index write is rejected" p_array_write_negative [ 0 ];
      agree "a negative index read is rejected" p_array_read_negative [ 0 ];

      "negative indices are rejected on both sides" >:: (fun _ ->
        assert_equal ~printer None (run_verified p_array_write_negative [ 0 ]);
        assert_equal ~printer None (run_verified p_array_read_negative [ 0 ]));

      (* copy / uncopy *)
      agree "copy and uncopy of a reference" p_copy_uncopy [ 0 ];
      agree "copy onto a variable that is not nil is rejected" p_copy_not_nil [ 0 ];
      agree "copy onto itself is rejected" p_copy_self [ 0 ];
      agree "uncopy of two different references is rejected" p_uncopy_mismatch [ 0 ];

      "the verified interpreter runs copy and uncopy" >:: (fun _ ->
        assert_equal ~printer (Some [ 3 ]) (run_verified p_copy_uncopy [ 0 ]);
        assert_equal ~printer None (run_verified p_copy_not_nil [ 0 ]);
        assert_equal ~printer None (run_verified p_copy_self [ 0 ]);
        assert_equal ~printer None (run_verified p_uncopy_mismatch [ 0 ]));

      (* ゼロクリアを忘れたオブジェクトブロックは意味論どおり落ちる *)
      "an object block that leaves a field dirty is rejected" >:: (fun _ ->
        assert_equal ~printer None
          (run_verified
             (R.Sobj (nat_of_int 0, v 5,
                      R.Sfassign (v 5, nat_of_int 0, R.MAdd, c 3))) [ 0 ]));
    ]

let _ = run_test_tt_main suite
