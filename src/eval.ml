(*環境：変数名とロケーションのリストを拡張する関数*)
let ext_envs env x v = (x,v) :: env

(*ストア：ロケーションと値のリストを拡張する関数*)
let ext_st st x v = (x,v) :: st


(*環境に指定されたメソッドのフィールドを数字の-1から順にロケーションを追加する。eval_stateのOBJBLOKで使用*)
let ext_env_field f1 =
  let rec ext_env_field_2 f2 n =                    
   match f2 with
  | [] -> []
  | Decl(dtype, id) :: tl -> ext_envs (ext_env_field_2 tl (n - 1)) id n
in
ext_env_field_2 f1 (-1)

(*mainメソッドがあるクラスのフィールドから環境を生成する関数　eval_progでのみ使用*)
let gen_env fid1 =
let rec gen_env_2 env fid2 n =
  match fid2 with
  | [] -> env
  | id :: tl -> ext_envs (gen_env_2 env tl (n + 1)) id n
in
gen_env_2 [] fid1 1

(*eval_statement用のストア拡張関数　*)
let ext_st_a st len id f =
  let rec ext_st_a_2 st n len2 id f =
    if n > (-len2) then
      let y = (-len2) + 1 in
      let z = (-len2) + 2 in
      begin
        match n with
        | x when y = n-> ext_st (ext_st_a_2 st (n - 1) len2 id f) y (ObjVal(id, f))
        | x when z = n-> ext_st (ext_st_a_2 st (n - 1) len2 id f) z (IntVal(y))
        | x -> ext_st (ext_st_a_2 st (n - 1) len2 id f) x (IntVal(0))
      end
    else st
  in
  ext_st_a_2 st (-1) (len + 3) id f

(*OBJBLOCK用　すべてのフィールドの値を0にする.*)
let rec ext_st_zero st len  =
  if len <> 0 then
    ext_st_zero (ext_st st len (IntVal(0))) (len + 1)
  else st

(*マップ拡張用 programの意味論に使用*)
let rec ext_map map c fm = (c, fm) :: map 
  
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
let rec ext_env_meth env envf pidl aidl =
  match pidl, aidl with
  | [], [] -> envf
  | pid :: ptl, aid :: atl ->
     let aid2 = lookup_envs aid env in
     ext_envs (ext_env_meth env envf ptl atl) pid aid2
  | _, _ -> failwith "error"


(*メソッドのリストから指定したメソッド名のメソッドを返す関数*)
let rec lookup_meth x meth =
  match meth with
  | y :: tl ->
     begin
       match y with
       | MDecl(id, decl, stml) -> if x = id
                                  then y
                                  else lookup_meth x tl
     end
  | [] -> failwith "unbound method"
 
(*マップのリストから指定されたクラス名のfieldとメソッドのタプルを返す*)
let rec lookup_map id map =
  match map with
  | (x , (f, m)) :: tl -> if x = id then (f,m)
                         else lookup_map id tl
  | [] -> failwith "error"

(*マップのリストから指定されたメソッド名を含んでいるクラス名とそのメソッドのstatementのタプルを返す*)
let rec lookup_class id1 map =
  let rec lookup_class_2 id2 ml =
    match ml with
    | MDecl(mid, paral, stml) :: tl2 -> if mid = id2 then Some(stml)
                                        else lookup_class_2 id2 tl2 
    | [] -> None
  in
  match map with
  | (cid, (fl, ml)) :: tl1 -> let result =  (lookup_class_2 id1 ml) in
                              if result = None
                              then lookup_class id1 tl1
                              else
                                begin
                                  match result with
                                  | Some(stm) -> (cid, stm)
                                end
  | [] -> failwith ("error, not found")
        
(*タプル型のリストの第1要素のみのリストを返す関数　CALLなどで使用*)
let first_list list =
  let rec fir_list l1 l2 =
    match l1 with
    | [] -> l2
    | (x,y) :: tl -> x :: (fir_list tl l2) 
  in
  fir_list list []
        
(*タプル型のリストの第２要素のみのリストを返す関数　CALLなどで使用*)
let rec second_list list =
  match list with
  | [] -> []
  | (x, y) :: tl -> y :: (second_list tl)

(*decl listからidのみのリストへ変換する関数*)
let rec id_list para =
  match para with
  | [] -> []
  | hd :: tl ->
     begin
       match hd with
       | Decl(dtype, id) -> id :: (id_list tl)
     end

(*マップを生成する関数：クラスリストを受け取り、マップを返す eval_progでのみ使用*)
let rec gen_map clist =
  match clist with
  | [] -> []
  | CDecl(id, None, fl, ml) :: tl -> ext_map (gen_map tl) id (fl, ml)

(*ストアを生成する関数：eval_progでのみ使用*)
let gen_st env1 objval =
let rec gen_st2 st2 env2 n =
  match env2 with
  | [] -> st2
  | f :: tl -> ext_st (gen_st2 st2 env2 (n + 1)) n (IntVal(0))
in 
let st3 = ext_st [] ((List.length env1) + 1) objval in
gen_st2 st3 env1 1

(*結果を生成する関数：eval_progでのみ使用*)
let rec gen_result env st =
  match env with
  | [] -> []
  | (f, locs) :: tl -> let v = lookup_st locs st in
                       (f, v) :: (gen_result tl st)  
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
  | Var(x) -> lookup_st (lookup_envs x env) st
  (*NIL*)
  | Nil -> IntVal(0)
  (*BINOP*)
  | Binary(b, e1, e2) ->
     begin
       match b with
       | Add  -> bin_op (+)     (eval_exp e1 env st) (eval_exp e2 env st)
       | Sub  -> bin_op (-)     (eval_exp e1 env st) (eval_exp e2 env st)
       | Xor  -> bin_op (lxor)  (eval_exp e1 env st) (eval_exp e2 env st)
       | Mul  -> bin_op ( * )   (eval_exp e1 env st) (eval_exp e2 env st)
       | Div  -> bin_op (/)     (eval_exp e1 env st) (eval_exp e2 env st)
       | Mod  -> bin_op (mod)   (eval_exp e1 env st) (eval_exp e2 env st)
       | Band -> bin_op (land)  (eval_exp e1 env st) (eval_exp e2 env st)
       | Bor  -> bin_op (lor)   (eval_exp e1 env st) (eval_exp e2 env st)
       | And  -> comp_op (&&)   (eval_exp e1 env st) (eval_exp e2 env st)
       | Or   -> comp_op (||)   (eval_exp e1 env st) (eval_exp e2 env st)
       | Lt   -> rel_op (>)     (eval_exp e1 env st) (eval_exp e2 env st)
       | Gt   -> rel_op (<)     (eval_exp e1 env st) (eval_exp e2 env st)
       | Eq   -> rel_op (=)     (eval_exp e1 env st) (eval_exp e2 env st)
       | Ne   -> rel_op (<>)    (eval_exp e1 env st) (eval_exp e2 env st)
       | Le   -> rel_op (<=)    (eval_exp e1 env st) (eval_exp e2 env st)
       | Ge   -> rel_op (>=)    (eval_exp e1 env st) (eval_exp e2 env st)
     end
(*文statementを評価する関数。
第一引数に文、第二引数にオブジェクトブロックを指すロケーションと環境のタプル、
第三引数にマップ、第四引数にストアを受け取る。この関数はストアを返す。
（OBJBLOCK部分がまだ未完成*)
let rec eval_state stml (locs,env) map st =
  match stml with
  | [] -> st
  | stm :: tl ->
     begin
       match stm with
       (*SKIP*)
       | Skip -> eval_state tl (locs, env) map st
       (*ASSVAR*)
       | Assign((x, None), op, e) ->
          begin
            match op with
            | ModAdd ->
               let v1 = eval_exp e env st in
               let v2 = bin_op (+) (lookup_st (lookup_envs x env) st) v1 in
               let st2 = ext_st st (lookup_envs x env) v2 in
               eval_state tl (locs, env) map st2
            | ModSub ->
               let v1 = eval_exp e env st in
               let v2 = bin_op (-) (lookup_st (lookup_envs x env) st) v1 in                     let st2 = ext_st st (lookup_envs x env) v2 in                                    eval_state tl (locs, env) map st2
            | ModXor ->
               let v1 = eval_exp e env st in
               let v2 = bin_op (lxor) (lookup_st (lookup_envs x env) st) v1 in
               let st2 = ext_st st (lookup_envs x env) v2 in
               eval_state tl (locs, env) map st2
          end
       (*SWPVAR*)
       | Swap((x1, None),(x2, None)) ->
          let v1 = lookup_st (lookup_envs x1 env) st in
          let v2 = lookup_st (lookup_envs x2 env) st in
          let st2 = ext_st st (lookup_envs x2 env) v1 in
          let st3 = ext_st st2 (lookup_envs x1 env) v2 in
          eval_state tl (locs, env) map st3
       | Loop(e1, stml1, stml2, e2) ->
          (* LOOPMAIN *)
          if (eval_exp e1 env st) <> IntVal(0) then
            let st2 = eval_state stml1 (locs, env) map st in
            eval_state stml (locs, env) map st2
          (*LOOPREC*)
          else if (eval_exp e2 env st) = IntVal(0) then
            let st2 = eval_state stml2 (locs, env) map st in
            if(eval_exp e1 env st2) = IntVal(0) then
              let st3 = eval_state stml1 (locs, env) map st2 in
              eval_state stml (locs, env) map st3
            else failwith "error"
                          (*LOOPBASE*)
          else if (eval_exp e2 env st) <> IntVal(0) then
            eval_state tl (locs, env) map st
          else failwith "error"
       | Conditional(e1, stml1, stml2, e2) ->
          (*IFTRUE*)
          if (eval_exp e1 env st) <> IntVal(0) then
            let st2 = eval_state stml1 (locs, env) map st in
            if (eval_exp e2 env st2) <> IntVal(0) then
              eval_state tl (locs, env) map st2
            else failwith "error"
          (*IFFALSE*)
          else if(eval_exp e1 env st) = IntVal(0) then
            let st2 = eval_state stml2 (locs, env) map st in
            if (eval_exp e2 env st2) = IntVal(0) then
              eval_state tl (locs, env) map st2
            else failwith "error"
          else failwith "error"
       (*LocalCALL*)
       | LocalCall(mid,objl) ->
          let objval = lookup_st locs st in
          let aidl = first_list objl in(*aidl = 実引数のidリスト*)
          begin
            match objval with
            | ObjVal((id, envf)) ->
               let map2 = lookup_map id map in
               begin
                 match map2 with
                 | (f,meth) ->
                    let meth2 = lookup_meth mid meth in
                    begin
                      match meth2 with
                      | MDecl(mid, para, mstml) ->
                         let pidl = id_list para in(*pidl = 仮引数のidリスト*)
                         let env2 = ext_env_meth env envf pidl aidl in(*env2 = γ'*)
                         let st2 = eval_state mstml (locs, env2) map st in
                         eval_state tl (locs, env) map st2 
                    end
               end
            | _ -> failwith "errpor"
          end
       (*LocalUNCALL*)
       | LocalUncall(mid, objl) ->
          let st2 = eval_state [LocalCall(mid,objl)] (locs, env) map st in
          eval_state tl (locs, env) map st2
       (*CALLOBJ*)
       | ObjectCall(obj, mid, objl) ->
          begin
            match obj with
            | (id, n) ->
               let locs2 = lookup_st (lookup_envs id env) st in
               begin
                 match locs2 with
                 | IntVal(n) ->
                    let objval = lookup_st n st in
                    let aidl = first_list objl in(*aidl = 実引数のidリスト*)
                    begin
                      match objval with
                      | ObjVal((id, envf)) ->
                         let map2 = lookup_map id map in
                         begin
                           match map2 with
                           | (f,meth) ->
                              let meth2 = lookup_meth mid meth in
                              begin
                                match meth2 with
                                | MDecl(mid, para, mstml) ->
                                   let pidl = id_list para in(*pidl = 仮引数のidリスト*)
                                   let env2 = ext_env_meth env envf pidl aidl in(*env2 = γ'*)
                                   let st2 = eval_state mstml (locs, env2) map st in
                                   eval_state tl (n, env) map st2 
                              end
                         end
                      | _ -> failwith "error"
                    end
                 | _ -> failwith "error"
               end       
          end
      (*UNCALLOBJ*)
       | ObjectUncall(obj, mid, objl) ->
          let st2 = eval_state [ObjectCall(obj, mid, objl)] (locs, env) map st in
          eval_state tl (locs, env) map st2
       (*OBJBLOCK*)
       | ObjectBlock(tid, id, stml) ->
          let map2 = lookup_map tid map in
          begin
            match map2 with
            | (f, m) -> let envf = ext_env_field f  in
                        let st2 = ext_st_a st (List.length env) tid envf in
                        let env2 = ext_envs env id ((-List.length st2) - 3)  in
                        let st3 = eval_state stml (locs, env2) map st2 in 
                        let st4 = ext_st_zero st3 (-List.length f) in
                        eval_state tl (locs, env) map st4
          end         
       | _ -> failwith "unknown statement"
     end

let rec eval_prog = function
  | Prog(cl) -> let map = gen_map cl in
                let mainid = lookup_class "main" map in
                begin
                  match mainid with
                  | mid, mainstml ->
                     let fm = lookup_map mid map in
                     begin
                       match fm with
                       | (field, methl) -> 
                          let fid = id_list field in
                          let env = gen_env fid in
                          let st = gen_st env (ObjVal(mid, env)) in
                          let st2 = eval_state mainstml ((List.length env) + 1, env) map st in
                          gen_result env st2
                     end
                     
                end

                           
