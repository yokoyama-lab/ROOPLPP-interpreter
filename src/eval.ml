open Syntax
open Value
open Invert
   
(*環境：変数名とロケーションのリストを拡張する関数(同じ識別子がある場合古いものを削除し、新しいものを追加する*)
let ext_envs env x v = (x,v) :: (List.remove_assoc x env)

(*ストア：ロケーションと値のリストを拡張する関数*)
let ext_st st x v = List.sort (fun x y -> if x < y then -1 else 1) ((x,v) :: (List.remove_assoc x st))
                     
(*環境に指定されたメソッドのフィールドを使われていないロケーションに追加する。eval_stateのOBJBLOKで使用*)
let rec ext_env_field f n =
   match f with
  | [] -> []
  | Decl(dtype, id) :: tl -> ext_envs (ext_env_field tl (n + 1)) id n

(*eval_stateで使用：locsからn-1までのロケーションに対応する値をすべてIntVal(0)にする関数*)
let rec ext_st_zero st locs n =
  if locs < n then
    ext_st (ext_st_zero st (locs + 1) n) locs (IntVal(0))
  else
    st
      
(*マップ拡張用 eval_progで使用*)
let rec ext_map map c fm = (c, fm) :: map
  
(*第一引数に変数名、第２引数に環境を指定し、環境の中に指定した変数名があれば、
その変数のロケーションを返す関数*)
let lookup_envs x env =
 try snd (List.find (fun (y,_) -> x = y) env)
 with Not_found -> failwith ("unbound variable: " ^ x)
        
(*第一引数にロケーション、第２引数にストアを指定し、ストアの中に指定したロケーションがあれば、
そのロケーションに格納されている値を返す関数*)
let lookup_st x st =
  try snd (List.find (fun (y,_) -> x = y) st)
  with Not_found -> failwith ("unbound locations: " ^ (string_of_int x))

(*環境を拡張する関数 CALL,CALLOBJでのみ使用*)
let rec ext_env_meth env envf pidl aidl =
  match pidl, aidl with
  | [], [] -> envf
  | pid :: ptl, aid :: atl ->
     let aid2 = lookup_envs aid env in
     ext_envs (ext_env_meth env envf ptl atl) pid aid2
  | _, _ -> failwith "error in ext_env_meth"


(*メソッドのリストから指定したメソッド名のメソッドを返す関数*)
let rec lookup_meth x meth =
  try List.find (fun (MDecl(id, _, _)) -> x = id) meth
  with Not_found -> failwith "unbound method"
 
(*マップのリストから指定されたクラス名のfieldとメソッドのタプルを返す*)
let rec lookup_map id map =
  try snd (List.find (fun (x , _) -> x = id) map)
  with Not_found -> failwith "error in lookup_map"

(*マップのリストから指定されたメソッド名を含んでいるクラス名とそのメソッドのstatementのタプルを返す*)
let rec lookup_class id1 map =
  let rec lookup_class_2 id2 ml =
    match ml with
    | MDecl(mid, paral, stml) :: tl2 ->
       if mid = id2 then Some(stml)
       else lookup_class_2 id2 tl2 
    | [] -> None
  in
  match map with
  | (cid, (fl, ml)) :: tl1 ->
     let result =  (lookup_class_2 id1 ml) in
     if result = None
     then lookup_class id1 tl1
     else
       begin
         match result with
         | Some(stm) -> (cid, stm)
         | _ -> failwith "error in lookup_class"
       end
  | [] -> failwith ("error, not found")

(*ロケーションのベクトルから指定されたインデックスのロケーションを返す（添字は0から）*)
let rec lookup_vec index vec =
    begin
      match vec with
      | [] -> failwith "error in lookup_vec"
      | l :: tl -> if index <> 0 then lookup_vec (index - 1) tl
                   else l
    end
  
(*decl listからidのみのリストへ変換する関数*)
let id_list = List.map (fun (Decl(_, id)) -> id)

(*OBJDElETEのための関数*)
let rec delete_st st locs n =
  if n <> 0 then delete_st (List.remove_assoc locs st) (locs + 1) (n - 1)
  else st

(*ARRDELETEのための関数*)
let rec delete_arr st vec =
  match vec with
  | [] -> st
  | l :: tl -> delete_arr (List.remove_assoc l st) tl
  
(*ロケーションのベクトルを生成する関数：第一引数に要素数、第二引数に使われてないロケーションの場所を受け取る。*)
let rec gen_locsvec n locs =
  if n <> 0 then locs :: gen_locsvec (n - 1) (locs + 1)
  else []

(*クラスからidを取り出す関数 gen_mapで使用*)
let lookup_cid (CDecl(id, _, _, _)) = id

(*指定したクラスidのクラスを返す関数　α^-1に相当 map_fieldとmap_methodで使用*)
let rec lookup_class_map clist cid =
  match clist with
  | [] -> failwith "unbound class"
  | CDecl(id, tid, fl, m) :: tl ->
     if cid = id then CDecl(id, tid, fl, m)
     else lookup_class_map tl cid

(*gen_mapで使用する関数 ROOPL++26ページの関数fieldに相当*)
let rec map_field clist1 cl =
  match cl with
  | CDecl(id, None, fl, m) -> fl
  | CDecl(id, Some(cid), fl, m) ->
     let parent_class = lookup_class_map clist1 cid in (*a^-1(c')*)
     let parent_method = map_field clist1 parent_class in
     parent_method @ fl

(*下にある関数の説明
map_method: gen_mapで使用する関数 ROOPL++26ページの関数methodに相当
lookup_methid: メソッドのリストに指定した名前のメソッド名があるか調べる関数
method_union: サブクラスに親クラスと同じ名前のメソッドがある場合
親クラスからそのメソッドを削除し、サブクラスの同じ名前のメソッドを追加する関数(オーバーライド)
remove__method: 親クラスがサブクラスと同じ名前のメソッドをもつ場合そのメソッドを削除する関数
*)
let rec map_method clist1 cl =
  let rec lookup_methid id = function
    | [] -> false
    | MDecl(mid, _, _) :: tl -> if id = mid then true
                                else lookup_methid id tl
  in
  let method_union subm parem =
    let rec remove_method subm parem =
      match parem with
      | [] -> []
      | MDecl(mid, dl, stml) :: tl ->
         if (lookup_methid mid subm)
         then remove_method subm tl
         else MDecl(mid, dl, stml) :: remove_method subm tl
    in
    (remove_method subm parem) @ subm
  in
  match cl with
  | CDecl(id, None, fl, m) -> m
  | CDecl(id, Some(cid), fl, m) ->
     let parent_class = lookup_class_map clist1 cid in (*a^-1(c')*)
     let parent_method = map_method clist1 parent_class in
     method_union m parent_method

(*マップを生成する関数*)     
let gen_map clist =
let rec gen_map2 clist1 clist2 =
  match clist2 with
  | [] -> []
  | cl :: tl ->
     (lookup_cid cl, (map_field clist1 cl, map_method clist1 cl)) :: (gen_map2 clist1 tl)
in
gen_map2 clist clist

(*mainメソッドがあるクラスのフィールドから環境を生成する関数　eval_progでのみ使用*)
let gen_env fid1 : env =
  let rec gen_env_2 fid2 n =
    match fid2 with
    | [] -> [("this", n + 1)]
    | id :: tl -> ext_envs (gen_env_2 tl (n + 1)) id n
  in
  gen_env_2 fid1 1
  
(*ストアを生成する関数：eval_progでのみ使用*)
let gen_st env1 objval =
 let rec gen_st2 env2 objval n =
   match env2 with
   | [] -> failwith "error"
   | [f] -> [(n, objval); (n + 1, LocsVal n)]
   | f :: tl -> ext_st (gen_st2 tl objval (n + 1)) n (IntVal(0))
 in
 gen_st2 env1 objval 1

(*結果を生成する関数：eval_progでのみ使用*)
let rec gen_result env st =
  let rec gen_result_vec vec st f n =
    match vec with
    | [] -> []
    | l :: tl -> (f ^ "[" ^ (string_of_int n) ^ "]", lookup_st l st) :: (gen_result_vec tl st f (n + 1))
  in
  match env with
  | [] -> []
  | (f, locs) :: tl ->
     let v = lookup_st locs st in
     begin
       match v with
       | LocsVec(vec) -> (gen_result_vec vec st f 0) @ (gen_result tl st)
       | _ -> (f, v) :: (gen_result tl st)
     end
                       
(*eval_expを簡潔にするための関数
第一引数に演算子、第２第３引数にvalue型を受け取りIntValを返す。　*)
let bin_op f v1 v2 =
  match v1, v2 with
    | IntVal(n1), IntVal(n2) -> IntVal(f n1 n2)
    | _ -> failwith "integer values expected"

(*bin_op同様。等価、不等価演算子専用。*)
let comp_op f v1 v2 =
  match v1, v2 with
  | IntVal(n1), IntVal(n2) ->
     if f (n1 = 1) (n2 = 1) then IntVal(1) else IntVal(0)
  | _ -> failwith "integer values expected"

(*bin_op同様。関係演算子専用。*)
let rel_op f v1 v2 =
  match v1, v2 with
  | IntVal(n1), IntVal(n2) -> if f n1 n2 = true then IntVal(1)
                              else IntVal(0)
  | _ -> failwith "integer values expected"

(*式expressionを評価するための関数。envは環境、stはストア。value型を返す。*)
let rec eval_exp exp env st =
  match exp with
  (*CON*)
  | Const(n) -> IntVal(n)
  (*VAR*)
  | Var(x) -> lookup_st (lookup_envs x env) st
  (*ARRELEM*)
  | ArrayElement(id, e) ->
     let IntVal(index) = eval_exp e env st in
     let locs = lookup_envs id env in
     let LocsVec(lv) = lookup_st locs st in
     let locs2 = lookup_vec index lv in
     lookup_st locs2 st
  (*NIL*)
  | Nil -> IntVal(0)
  (*BINOP*)
  | Binary(b, e1, e2) ->
     let f = function
       | Add  -> bin_op (+)
       | Sub  -> bin_op (-)
       | Xor  -> bin_op (lxor)
       | Mul  -> bin_op ( * )
       | Div  -> bin_op (/)
       | Mod  -> bin_op (mod)
       | Band -> bin_op (land)
       | Bor  -> bin_op (lor)
       | And  -> comp_op (&&)
       | Or   -> comp_op (||)
       | Lt   -> rel_op (<)
       | Gt   -> rel_op (>)
       | Eq   -> rel_op (=)
       | Ne   -> rel_op (<>)
       | Le   -> rel_op (<=)
       | Ge   -> rel_op (>=)
     in f b (eval_exp e1 env st) (eval_exp e2 env st)

(*文statementを評価する関数。
第一引数に文、第二引数にオブジェクトブロックを指すロケーションと環境のタプル、
第三引数にマップ、第四引数にストアを受け取る。この関数はストアを返す。
（OBJBLOCK部分がまだ未完成*)
let rec eval_state stml env map st =
  match stml with
  | [] -> st
  | stm :: tl ->
     begin
       match stm with
       (*SKIP*)
       | Skip -> eval_state tl env map st
       (*ASSVAR*)
       | Assign((x, None), op, e) ->
        let f = function
          | ModAdd -> (+)
          | ModSub -> (-)
          | ModXor -> (lxor) in
        let v1 = eval_exp e env st in
        let lvx = lookup_envs x env in (* the left value of x *)
        let vx = lookup_st lvx st in  (* the right value of x *)
        let v2 = bin_op (f op) vx v1 in
        let st2 = ext_st st lvx v2 in
        eval_state tl env map st2
       (*ASSARRELEMVAR*)
       | Assign((x,Some(e1)), op, e2)(*x[e1] op= e2*) ->
          let f = function
          | ModAdd -> (+)
          | ModSub -> (-)
          | ModXor -> (lxor) in
          let IntVal(v1) = eval_exp e1 env st in
          let v2 = eval_exp e2 env st in
          let locs = lookup_envs x env in
          let LocsVec(lv) = lookup_st locs st in
          let locs2 = lookup_vec v1 lv in
          let w = lookup_st locs2 st in
          let w2 = bin_op (f op) w v2 in
          let st2 = ext_st st locs2 w2 in
          eval_state tl env map st2
       (*SWPVAR*)
       | Swap((x1, None),(x2, None)) ->
          let v1 = lookup_st (lookup_envs x1 env) st in
          let v2 = lookup_st (lookup_envs x2 env) st in
          let st2 = ext_st st (lookup_envs x2 env) v1 in
          let st3 = ext_st st2 (lookup_envs x1 env) v2 in
          eval_state tl env map st3
       (*SWAPARRAYVAR*)
       | Swap((x, Some(e1)), (y, Some(e2))) ->
          let IntVal(x_index) = eval_exp e1 env st in
          let locsx1 = lookup_envs x env in
          let LocsVec(locsvecx) = lookup_st locsx1 st in
          let locsx2 = lookup_vec x_index locsvecx in
          let v1 = lookup_st locsx2 st in
          let IntVal(y_index) = eval_exp e2 env st in
          let locsy1 = lookup_envs y env in
          let LocsVec(locsvecy) = lookup_st locsy1 st in
          let locsy2 = lookup_vec y_index locsvecy in
          let v2 = lookup_st locsy2 st in
          let st2 = ext_st st locsx2 v2 in
          let st3 = ext_st st2 locsy2 v1 in
          eval_state tl env map st3
       | Loop(e1, stml1, stml2, e2) ->
          let rec eval_loop (e1, stml1, stml2, e2) env map st =
            (*LOOPREC*)
            if (eval_exp e2 env st) = IntVal(0) then
              let st2 = eval_state stml2 env map st in
              if(eval_exp e1 env st2) = IntVal(0) then
                let st3 = eval_state stml1 env map st2 in
                eval_loop (e1, stml1, stml2, e2) env map st3
              else failwith "error in LOOPREC"
             (*LOOPBASE*)
            else if (eval_exp e2 env st) <> IntVal(0) then
              st
            else failwith "error in LOOPBASE"
          in
          (* LOOPMAIN *)
          if (eval_exp e1 env st) <> IntVal(0) then
            let st2 = eval_state stml1 env map st in
            let st3 = eval_loop (e1, stml1, stml2, e2) env map st2 in
            eval_state tl env map st3
          else failwith "error in LOOPMAIN"
       | Conditional(e1, stml1, stml2, e2) ->
          (*IFTRUE*)
          if (eval_exp e1 env st) <> IntVal(0) then
            let st2 = eval_state stml1 env map st in
            if (eval_exp e2 env st2) <> IntVal(0) then
              eval_state tl env map st2
            else failwith "error in IFTRUE"
          (*IFFALSE*)
          else if(eval_exp e1 env st) = IntVal(0) then
            let st2 = eval_state stml2 env map st in
            if (eval_exp e2 env st2) = IntVal(0) then
              eval_state tl env map st2
            else failwith "error in IFFALSE A"
          else failwith "error in IFFALSE B"
       (*LocalCALL*)
       | LocalCall(mid,objl) ->
          let locs = lookup_envs "this" env in
          let LocsVal(locs2) = lookup_st locs st in
          let ObjVal(id, envf)  = lookup_st locs2 st in
          let aidl = List.map fst objl in (*aidl = 実引数のidリスト*)
          let (f, meth) = lookup_map id map in
          let MDecl(mid, para, mstml) = lookup_meth mid meth in
          let pidl = id_list para in
          let env2 = ext_env_meth env envf pidl aidl in
          let env3 = ext_envs env2 "this" locs in
          let st2 = eval_state mstml env3 map st in
          eval_state tl env map st2
       (*LocalUNCALL*)
       | LocalUncall(mid, objl) ->
          let locs = lookup_envs "this" env in
          let LocsVal(locs2) = lookup_st locs st in
          let ObjVal(id, envf)  = lookup_st locs2 st in
          let aidl = List.map fst objl in (*aidl = 実引数のidリスト*)
          let (f, meth) = lookup_map id map in
          let MDecl(mid, para, mstml) = lookup_meth mid meth in
          let pidl = id_list para in
          let env2 = ext_env_meth env envf pidl aidl in
          let env3 = ext_envs env2 "this" locs in
          let st2 = eval_state (invert mstml) env3 map st in
          eval_state tl env map st2
       (*CALLOBJ*)
       | ObjectCall((id, None), mid, objl) ->
          let locs = lookup_envs id env in
          let LocsVal(locs2) = lookup_st locs st in
          let ObjVal(id2, envf) = lookup_st locs2 st in
          let aidl = List.map fst objl in
          let (f, meth) = lookup_map id2 map in
          let MDecl(mid, para, mstml) = lookup_meth mid meth in
          let pidl = id_list para in
          let env2 = ext_env_meth env envf pidl aidl in
          let env3 = ext_envs env2 "this" locs in
          let st2 = eval_state mstml env3 map st in
          eval_state tl env map st2
       (*CALLOBJARRAY*)
       | ObjectCall((id, Some(e)), mid, objl) ->
          let veclocs = lookup_envs id env in
          let LocsVec(vec) = lookup_st veclocs st in
          let IntVal(index) = eval_exp e env st in
          let locs = lookup_vec index vec in
          let LocsVal(locs2) = lookup_st locs st in
          let LocsVal(locs3) = lookup_st locs2 st in
          let ObjVal(id2, envf) = lookup_st locs3 st in
          let aidl = List.map fst objl in
          let (f, meth) = lookup_map id2 map in
          let MDecl(mid, para, mstml) = lookup_meth mid meth in
          let pidl = id_list para in
          let env2 = ext_env_meth env envf pidl aidl in
          let env3 = ext_envs env2 "this" locs2 in
          let st2 = eval_state mstml env3 map st in
          eval_state tl env map st2
      (*UNCALLOBJ*)
       | ObjectUncall((id, None), mid, objl) ->
          let locs = lookup_envs id env in
          let LocsVal(locs2) = lookup_st locs st in
          let ObjVal(id2, envf) = lookup_st locs2 st in
          let aidl = List.map fst objl in
          let (f, meth) = lookup_map id2 map in
          let MDecl(mid, para, mstml) = lookup_meth mid meth in
          let pidl = id_list para in
          let env2 = ext_env_meth env envf pidl aidl in
          let env3 = ext_envs env2 "this" locs in
          let st2 = eval_state (invert mstml) env3 map st in
          eval_state tl env map st2
       (*UNCALLOBJARRAY*)
       | ObjectUncall((id, Some(e)), mid, objl) ->
          let veclocs = lookup_envs id env in
          let LocsVec(vec) = lookup_st veclocs st in
          let IntVal(index) = eval_exp e env st in
          let locs = lookup_vec index vec in
          let LocsVal(locs2) = lookup_st locs st in
          let LocsVal(locs3) = lookup_st locs2 st in
          let ObjVal(id2, envf) = lookup_st locs3 st in
          let aidl = List.map fst objl in
          let (f, meth) = lookup_map id2 map in
          let MDecl(mid, para, mstml) = lookup_meth mid meth in
          let pidl = id_list para in
          let env2 = ext_env_meth env envf pidl aidl in
          let env3 = ext_envs env2 "this" locs2 in
          let st2 = eval_state (invert mstml) env3 map st in
          eval_state tl env map st2
       (*OBJBLOCK*)
       | ObjectBlock(tid, id, stml) ->
          let (fl, ml) = lookup_map tid map in
          let env2 = ext_envs env id (List.length st + 2)(*r*) in
          let envf = ext_env_field fl (List.length st + 3)(*a1*) in
          let st2 = ext_st_zero st (List.length st + 3) ((List.length st + 3) + (List.length fl)) in
          let st3 = ext_st st2 (List.length st + 1) (ObjVal(tid, envf)) (*l'->(c,γ')*) in
          let st4 = ext_st st3 (lookup_envs id env2) (LocsVal(List.length st + 1)) (*r->l'*) in
          let st5 = eval_state stml env2 map st4 in
          let st6 = ext_st_zero st5 (List.length st + 3) ((List.length st + 3) + (List.length fl)) in
          eval_state tl env map st6
       (*OBJNEW*)
       | ObjectConstruction(tid, (id, None)) ->
          let (fl, ml) = lookup_map tid map in
          let env2 = ext_envs env id (List.length st + 2)(*r*) in
          let locs = lookup_envs id env2 in
          let envf = ext_env_field fl (List.length st + 3)(*l1*) in
          let st2 = ext_st_zero st (List.length st + 3) ((List.length st + 3) + (List.length fl)) in
          let st3 = ext_st st2 (List.length st + 1) (ObjVal(tid, envf)) in
          let st4 = ext_st st3 locs (LocsVal(List.length st + 1)) in
          eval_state tl env2 map st4
       (*OBJNEWARRAY*)
       | ObjectConstruction(tid, (id, Some(e))) ->
          let (fl, ml) = lookup_map tid map in
          let veclocs = lookup_envs id env in
          let LocsVec(vec) = lookup_st veclocs st in
          let IntVal(index) = eval_exp e env st in
          let locs = lookup_vec index vec in
          let st = ext_st st locs (LocsVal((List.length st + 2))) in
          let LocsVal(locs2) = lookup_st locs st in
          let envf = ext_env_field fl (List.length st + 3)(*l1*) in
          let st2 = ext_st_zero st (List.length st + 3) ((List.length st + 3) + (List.length fl)) in
          let st3 = ext_st st2 (List.length st + 1) (ObjVal(tid, envf)) in
          let st4 = ext_st st3 locs2 (LocsVal(List.length st + 1)) in
          eval_state tl env map st4
       (*OBJDELETE*)
       | ObjectDestruction(tid, (id, None))->
          let (fl, ml) = lookup_map tid map in
          let locs = (lookup_envs id env) - 1 (*l'*) in          
          let st2 = delete_st st locs (2 + List.length fl) in
          eval_state tl env map st2
       (*OBJARRAYDLETE*)
       | ObjectDestruction(tid, (id, Some(e)))->
          let (fl, ml) = lookup_map tid map in
          let veclocs = lookup_envs id env in
          let LocsVec(vec) = lookup_st veclocs st in
          let IntVal(index) = eval_exp e env st in
          let locs = lookup_vec index vec in
          let LocsVal(locs2) = (lookup_st locs st) in
          let locs3 = locs2 - 1 (*l'*) in
          let st2 = delete_st st locs3 (2 + List.length fl) in
          let st3 = ext_st st2 locs (IntVal 0) in
          eval_state tl env map st3
       (*ARRNEW*)
       | ArrayConstruction((tid, e), id) ->
          let IntVal(n) = eval_exp e env st in
          let locs = lookup_envs id env in
          let st2 = ext_st st locs (LocsVec(gen_locsvec n (List.length st + 1))) in
          let st3 = ext_st_zero st2 (List.length st2 + 1) ((List.length st2 + 1) + n) in
          eval_state tl env map st3
       (*ARRDELETE*)
       | ArrayDestruction((tid, e), id) ->
          let veclocs = lookup_envs id env in
          let LocsVec(vec) = lookup_st veclocs st in
          let st2 = delete_arr st vec in
          let st3 = ext_st st2 veclocs (IntVal 0) in
          eval_state tl env map st3
       (*COPY*)
       | CopyReference(dt, (id1, None), (id2, None)) ->
          let locs1 = lookup_envs id1 env in
          let locs2 = lookup_envs id2 env in
          let v = lookup_st locs1 st in
          let st2 = ext_st st locs2 v in
          eval_state tl env map st2
       (*UNCOPY*)
       | UncopyReference(dt, (id1, None), (id2, None)) ->
          let locs2 = lookup_envs id2 env in
          let st2 = ext_st st locs2 (LocsVal (locs2 - 1)) in
          eval_state tl env map st2
       (*LOCALBLOCK*)
       | LocalBlock(dt, id, e1, stml, e2) ->
          let (v1, v2) = (eval_exp e1 env st, eval_exp e2 env st) in
          let env2 = ext_envs env id (List.length st + 1) in
          let st2 = ext_st st (List.length st + 1) v1 in
          let st3 = eval_state stml env2 map st2 in
          let st4 = ext_st st3 (List.length st + 1) v2 in
          eval_state tl env map st4 
     end
let eval_prog (Prog(cl)) =
  (*マップ生成*)
  let map = gen_map cl in
  (*mainメソッドを含んでいるクラスidとメソッドの文を取得*)
  let (mid, mainstml) = lookup_class "main" map in
  (*mainメソッドを含んでいるクラスのフィールドを取得*)
  let (field, _) = lookup_map mid map in
  (*フィールドを識別子のみのリストに変換*)
  let fid = id_list field in
  (*フィールドから環境を生成*)
  let env = gen_env fid in
  (*環境からストアを生成*)
  let st = gen_st env (ObjVal(mid, env)) in
  (*mainメソッドの処理を実行*)
  let st2 = eval_state mainstml env map st in
  (*結果を生成(mainメソッドを含んでいるクラスのフィールドとそれに対応する値の組を返す)*)
  List.remove_assoc "this" (gen_result env st2)
