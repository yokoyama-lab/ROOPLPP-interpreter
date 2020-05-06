(*環境：変数名とロケーションのリストを拡張する関数*)
let ext_envs env x v = (x,v) :: env

(*ストア：ロケーションと値のリストを拡張する関数*)
let ext_st st x v = (x,v) :: st

(*指定されたメソッドのフィールドに数字の3から順に環境にロケーションを追加する。*)
let rec ext_env_field env1 f1 =
  let rec ext_env_field_2 env2 f2 n =
    match f2 with
    | [] -> List.rev (env2)
    | (t, f2) :: tl -> ext_env_field_2 (ext_envs env2 f2 (n + 3) ) tl (n + 3)
  in
  ext_env_field_2 env1 f1 0

(*第一引数に変数名、第２引数に環境を指定し、環境の中に指定した変数名があれば、
その変数のロケーションを返す関数*)
let rec lookup_envs x env =
  match env with
  | (y, (v : int)) :: tl -> if x = y then v else lookup_envs x tl
  | [] -> failwith ("unbound variable: " ^ x)

(*第一引数にロケーション、第２引数にストアを指定し、ストアの中に指定したロケーションがあれば、
そのロケーションに格納されている値を返す関数*)
let rec lookup_st x st =
  match st with
  | ( (y : int), v) :: tl -> if x = y then v
                             else lookup_st x tl
  | [] -> failwith ("unbound locations: " ^ (string_of_int x))

(*環境を拡張する関数 CALL,CALLOBJでのみ使用*)
let rec ext_env_meth env1 env2 meth_var_list arg_list =
  let ext_env_meth_2 env meth_var arg = (meth_var, arg) :: env
  in
  match (meth_var_list, arg_list) with
  | ([], []) -> List.rev (env1)
  | (var :: v_tl, arg :: a_tl) ->
     begin
       match (var, arg) with
       | (Var(x1), Var(x2)) ->
          let arg2 = lookup_envs x2 env2 in
          ext_env_meth (ext_env_meth_2 env1 x1 arg2) env2 v_tl a_tl
     end
  | (_, _) -> failwith "error"


(*メソッドのリストから指定したメソッド名のメソッドを返す関数*)
let rec lookup_meth x meth =
  match meth with
  | y :: tl ->
     begin
       match y with
       | Method(MethId(id), var, state) -> if x = id
                                           then Method(MethId(id), var, state)
                                           else lookup_meth x tl
     end
  | [] -> failwith "unbound method"

(*マップのリストから指定されたクラス名のマップ(fieldとメソッドのタプル)を返す*)
let rec lookup_map id map =
  match map with
  | (x , (f, m)) :: tl -> if x = id then (f,m)
                         else lookup_map id tl
  | [] -> failwith "error"

(*タプル型のリストの第２要素のみのリストを返す関数　CALLなどで使用*)
let second_list list =
  let rec sec_list l1 l2 =
    match l1 with
    | [] -> l2
    | (x,y) :: tl -> sec_list tl (y::l2)
  in
  sec_list (List.rev (list)) []

(*eval_expを簡潔にするための関数
第一引数に演算子、第２第３引数にvalue型を受け取りIntValを返す。　*)
let bin_op f v1 v2 =
  match (v1, v2) with
    | (IntVal(n1), IntVal(n2)) -> IntVal(f n1 n2)
    | _ -> failwith "integer values expected"

(*bin_op同様。等価、不等価演算子専用。*)
let comp_op f v1 v2 =
  match (v1, v2) with
  | (IntVal(n1), IntVal(n2)) ->
     if f (n1 = 1) (n2 = 1) then IntVal(1) else IntVal(0)
  | _ -> failwith "integer values expected"

(*bin_op同様。関係演算子専用。*)
let rel_op f v1 v2 =
  match v1, v2 with
  | IntVal(n1), IntVal(n2) -> if (f n1 n2) = true then IntVal(1)
                              else IntVal(0)
  | _ -> failwith "integer values expected"

(*式expressionを評価するための関数。envは環境、stはストア。value型を返す。*)
let rec eval_exp exp env st =
  match exp with
  (*CON*)
  | Const(n) -> IntVal(n)
  (*VAR*)
  | EVar(Var(x)) -> lookup_st (lookup_envs x env) st
  (*NIL*)
  | Nill -> IntVal(0)
  (*BINOP*)
  | Plus(e1, e2)      -> bin_op (+)     (eval_exp e1 env st) (eval_exp e2 env st)
  | Minus(e1, e2)     -> bin_op (-)     (eval_exp e1 env st) (eval_exp e2 env st)
  | Xor(e1, e2)       -> bin_op (lxor)  (eval_exp e1 env st) (eval_exp e2 env st)
  | Times(e1, e2)     -> bin_op ( * )   (eval_exp e1 env st) (eval_exp e2 env st)
  | Div(e1, e2)       -> bin_op (/)     (eval_exp e1 env st) (eval_exp e2 env st)
  | Mod(e1, e2)       -> bin_op (mod)   (eval_exp e1 env st) (eval_exp e2 env st)
  | OpAnd(e1, e2)     -> bin_op (land)  (eval_exp e1 env st) (eval_exp e2 env st)
  | OpOr(e1, e2)      -> bin_op (lor)   (eval_exp e1 env st) (eval_exp e2 env st)
  | And(e1, e2)       -> comp_op (&&)   (eval_exp e1 env st) (eval_exp e2 env st)
  | Or(e1, e2)        -> comp_op (||)   (eval_exp e1 env st) (eval_exp e2 env st)
  | Greater(e1, e2)   -> rel_op (>)     (eval_exp e1 env st) (eval_exp e2 env st)
  | Less(e1, e2)      -> rel_op (<)     (eval_exp e1 env st) (eval_exp e2 env st)
  | Equal(e1, e2)     -> rel_op (=)     (eval_exp e1 env st) (eval_exp e2 env st)
  | NotEq(e1, e2)     -> rel_op (<>)    (eval_exp e1 env st) (eval_exp e2 env st)
  | LessEq(e1, e2)    -> rel_op (<=)    (eval_exp e1 env st) (eval_exp e2 env st)
  | GreaterEq(e1, e2) -> rel_op (>=)    (eval_exp e1 env st) (eval_exp e2 env st)

(*文statementを評価する関数。
第一引数に文、第二引数にオブジェクトブロックを指すロケーションと環境のタプル、
第三引数にマップ、第四引数にストアを受け取る。この関数はストアを返す。
（OBJBLOCK部分がまだ未完成*)
let rec eval_state s (locs,env) map st =
  match s with
  (*SKIP*)
  | Skip -> st
  (*SEQ*)
  | Seq(s1, s2) ->
     eval_state s2 (locs,env) map (eval_state s1 (locs, env) map st)
  (*ASSVAR*)
  | AssignPl(Var(x), e) ->
     let v1 = eval_exp e env st in
     let v2 = bin_op (+) (lookup_st (lookup_envs x env) st) v1 in
     ext_st st (lookup_envs x env) v2
  | AssignMin(Var(x), e) ->
     let v1 = eval_exp e env st in
     let v2 = bin_op (-) (lookup_st (lookup_envs x env) st) v1 in
     ext_st st (lookup_envs x env) v2
  | AssignXor(Var(x), e) ->
     let v1 = eval_exp e env st in
     let v2 = bin_op (lxor) (lookup_st (lookup_envs x env) st) v1 in
     ext_st st (lookup_envs x env) v2
  (*SWPVAR*)
  | Swap(Var(x1), Var(x2)) ->
     let v1 = lookup_st (lookup_envs x1 env) st in
     let v2 = lookup_st (lookup_envs x2 env) st in
     let st2 = ext_st st (lookup_envs x2 env) v1 in
     ext_st st2 (lookup_envs x1 env) v2
  | From(e1, state1, state2, e2) ->
     (* LOOPMAIN *)
     if (eval_exp e1 env st) <> IntVal(0) then
       let st2 = eval_state state1 (locs, env) map st in
       let state3 = From(e1, state1, state2, e2) in
       eval_state state3 (locs, env) map st2
     (*LOOPREC*)
     else if (eval_exp e2 env st) = IntVal(0) then
       let st2 = eval_state state2 (locs, env) map st in
       if(eval_exp e1 env st2) = IntVal(0) then
         let st3 = eval_state state1 (locs, env) map st2 in
         let state3 = From(e1, state1, state2, e2) in
         eval_state state3 (locs, env) map st3
       else failwith "error"
     (*LOOPBASE*)
     else if (eval_exp e2 env st) <> IntVal(0) then st
     else failwith "error"
  | If(e1, state1, state2, e2) ->
     (*IFTRUE*)
     if (eval_exp e1 env st) <> IntVal(0) then
       let st2 = eval_state state1 (locs, env) map st in
       if (eval_exp e2 env st2) <> IntVal(0) then st2
       else failwith "error"
     (*IFFALSE*)
     else if(eval_exp e1 env st) = IntVal(0) then
       let st2 = eval_state state2 (locs, env) map st in
       if (eval_exp e2 env st2) = IntVal(0) then st2
       else failwith "error"
     else failwith "error"
  (*LocalCALL*)
  | LocalCall(MethId(m),vlist) ->
     let obj = lookup_st locs st in
     begin
       match obj with
       | ObjVal((ClassId(id), envf)) ->
          let map2 = lookup_map id map in
          begin
            match map2 with
            | (f,meth) ->
               let meth2 = lookup_meth m meth in
               begin
            match meth2 with
            | Method(meth_id, meth_var, meth_body) ->
               let meth_var_list = second_list meth_var in
               let env2 = ext_env_meth envf env meth_var_list vlist in
               eval_state meth_body (locs, env2) map st
               end
          end
       | _ -> failwith "error"
     end
  (*LocalUNCALL*)
  | LocalUncall(m,vlist) ->
     eval_state (LocalCall(m,vlist)) (locs, env) map st
  (*CALLOBJ*)
  | Call(Var(x), MethId(m), vlist) ->
     let locs2 = lookup_st (lookup_envs x env) st in
     begin
       match locs2 with
       | IntVal(n) -> let obj = lookup_st n st
                      in
                      begin
                        match obj with
                        | ObjVal((ClassId(id), envf)) ->
                           let map2 = lookup_map id map in
                           begin
                             match map2 with
                             | (f,meth) ->
                                let meth2 = lookup_meth m meth in
                                begin
                                  match meth2 with
                                  | Method(meth_id, meth_var, meth_body) ->
                                     let meth_var_list = second_list meth_var in
                                     let env2 = ext_env_meth envf env meth_var_list vlist in
                                     eval_state meth_body (n, env2) map st
                                end
                           end
                        | _ -> failwith "error"
                      end
       | _ -> failwith "error"
     end
  (*UNCALLOBJ*)
  | UnCall(var, m, vlist) -> eval_state (Call(var, m, vlist)) (locs, env) map st
  (*(*OBJBLOCK*)
  | Construt(ClassId(id), Var(x), statement, Var(x)) ->
     let map2 = lookup_map id map in
     begin
       match map2 with
       | (f, m) -> let env2 = ext_env (ext_env( (ext_env_field env f) "r" 2 )) "l'" 1  in *)
  | _ -> failwith "unknown statement"
