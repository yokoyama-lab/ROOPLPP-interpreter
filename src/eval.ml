open Syntax
open Value
open Invert

let myassert (cond, msg) =  assert (if not cond then print_endline msg; cond)

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
  if n <> 0 then
    ext_st (ext_st_zero st (locs + 1) (n - 1)) locs (IntVal(0))
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

let lookup_val x env st = lookup_st (lookup_envs x env) st

(*eval_stateで使用：オブジェクトフィールドの値がすべてゼロクリアされているか確認する関数*)
let rec is_field_zero st locs n =
  if n <> 0 then
  let flag = if lookup_st locs st = IntVal(0) then true
             else false in
  flag && (is_field_zero st (locs + 1) (n - 1))
  else true
  
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
  match vec with
  | [] -> failwith "error in lookup_vec"
  | l :: tl -> if index <> 0 then lookup_vec (index - 1) tl
               else l

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
    | MDecl(mid, _, _) :: tl -> id = mid || lookup_methid id tl
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
    | [] -> [("this", n)]
    | id :: tl -> ext_envs (gen_env_2 tl (n + 1)) id n
  in
  gen_env_2 fid1 1

(*ストアを生成する関数：eval_progでのみ使用*)
let gen_st env1 objval =
  let rec gen_st2 env2 objval n =
    match env2 with
    | [] -> failwith "error"
    | [f] -> [(n, LocsVal (n + 1)); (n + 1, objval)]
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
     match v with
     | LocsVec(vec) -> (gen_result_vec vec st f 0) @ (gen_result tl st)
     | _ -> (f, v) :: (gen_result tl st)

(*eval_expを簡潔にするための関数
第一引数に演算子、第２第３引数にvalue型を受け取りIntValを返す。　*)
let bin_op f v1 v2 =
  match v1, v2 with
    | IntVal(n1), IntVal(n2) -> IntVal(f n1 n2)
    | _ -> failwith "integer values expected"

(*bin_op同様。関係演算子専用。*)
let rel_op f v1 v2 =
  match v1, v2 with
  | IntVal(n1), IntVal(n2) ->
     if f (n1 = 1) (n2 = 1) then IntVal(1) else IntVal(0)
  | _ -> failwith "integer values expected"

(*bin_op同様。比較算子専用。*)
let comp_op f v1 v2 =
  if f v1 v2 = true then IntVal(1)
  else IntVal(0)

(*式expressionを評価するための関数。envは環境、stはストア。value型を返す。*)
let rec eval_exp exp env st =
  let rec lval_val y env =
    match y with
    | Var(x) -> let lv = lookup_envs x env in lv, lookup_st lv st
    | ArrayElement(x, e) ->
       let IntVal(x_index) = eval_exp e env st in
       let LocsVec(locsvecx) = lookup_val x env st in
       let locsx' = lookup_vec x_index locsvecx in
       let v = lookup_st locsx' st in (*the value of x[e1]*)
       locsx', v
    | Dot(x, xi) ->         
       let _, LocsVal(l) = lval_val x env in
       let ObjVal(c, env') = lookup_st l st in
       let li, v = lval_val xi env' in
       (*         let li = lookup_envs xi' env' in
                  let v = lookup_st li st in*)
       li, v
  in
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
  (* DOT *)
  | Dot(x, xi) -> let _, v = lval_val (Dot(x, xi)) env in v
(*  | Dot (Var(x), Var(xi')) ->
     let l = lookup_envs x env in
     let LocsVal(l') = lookup_st l st in
     let ObjVal(c, env') = lookup_st l' st in
     let li = lookup_envs xi' env' in
     lookup_st li st*)
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
       | And  -> rel_op (&&)
       | Or   -> rel_op (||)
       | Lt   -> comp_op (<)
       | Gt   -> comp_op (>)
       | Eq   -> comp_op (=)
       | Ne   -> comp_op (<>)
       | Le   -> comp_op (<=)
       | Ge   -> comp_op (>=)
     in f b (eval_exp e1 env st) (eval_exp e2 env st)

(*文statementを評価する関数。
第一引数に文、第二引数にオブジェクトブロックを指すロケーションと環境のタプル、
第三引数にマップ、第四引数にストアを受け取る。この関数はストアを返す。*)
let rec eval_state stml env map st0 =
  (* ストアのロケーションの最大値を求める *)
  let max_locs st = List.fold_left (fun x y -> if x < y then y else x) 0 (List.map fst st) in
  let isTrue = function
    | IntVal(0) -> false
    | IntVal(_) -> true
    | _ -> failwith "error in isTrue" in
  let f = function
    | ModAdd -> (+)
    | ModSub -> (-)
    | ModXor -> (lxor) in
  let update st stm =
    (* y (= x or x[n]) を受けとり左辺値と値を返す *)
    let rec lval_val y env =
    match y with
    | VarArray(x, None) -> let lv = lookup_envs x env in lv, lookup_st lv st
    | VarArray(x, Some e) ->
       let IntVal(x_index) = eval_exp e env st in
       let LocsVec(locsvecx) = lookup_val x env st in
       let locsx' = lookup_vec x_index locsvecx in
       let v = lookup_st locsx' st in (*the value of x[e1]*)
       locsx', v
    | InstVar(x, xi) ->         
       let _, LocsVal(l) = lval_val x env in
       let ObjVal(c, env') = lookup_st l st in
       let li, v = lval_val xi env' in
       (*         let li = lookup_envs xi' env' in
                  let v = lookup_st li st in*)
       li, v
    (*let lval_val = function
      | VarArray(x, None) -> let lv = lookup_envs x env in lv, lookup_st lv st
      | VarArray(x, Some(e)) ->
         let IntVal(x_index) = eval_exp e env st in
         let LocsVec(locsvecx) = lookup_val x env st in
         let locsx' = lookup_vec x_index locsvecx in
         let v = lookup_st locsx' st in (*the value of x[e1]*)
         locsx', v*)
         in
  match stm with
    (*PRINT*)
    | Print str -> (print_string str; st)
    (*SHOW*)
    | Show e ->
       let v = eval_exp e env st in
       (Print.print_value_rec v; st)
    (*SKIP*)
    | Skip -> st
    (*ASSVAR*) (*ASSARRELEMVAR*)
    | Assign(y, op, e) (*y op= e2*) ->
       let lvx, vx = lval_val y env in
       let v = eval_exp e env st in
       let v' = bin_op (f op) vx v in
       ext_st st lvx v' (* the right value of x *)
    (*SWPVAR*) (*SWAPARRVAR*)
    | Swap(y1, y2) (*y1 <=> y2*)->
       let lv1, v1 = lval_val y1 env in
       let lv2, v2 = lval_val y2 env in
       let st2 = ext_st st lv1 v2 in (*update y2 -> y1*)
       ext_st st2 lv2 v1 (*update y1 -> y2*)
    | Loop(e1, stml1, stml2, e2) ->
       let rec eval_loop (e1, stml1, stml2, e2) env map st =
         (*LOOPREC*)
         if (eval_exp e2 env st) = IntVal(0) then
           let st2 = eval_state stml2 env map st in
           if isTrue (eval_exp e1 env st2) then
             failwith "error in LOOPREC"
           else
             let st3 = eval_state stml1 env map st2 in
             eval_loop (e1, stml1, stml2, e2) env map st3
         (*LOOPBASE*)
         else
           (myassert(isTrue (eval_exp e2 env st), "error in LOOPBASE"); st)
       in
       (* LOOPMAIN *)
       if isTrue (eval_exp e1 env st) then
         let st2 = eval_state stml1 env map st in
         eval_loop (e1, stml1, stml2, e2) env map st2
       else failwith "error in LOOPMAIN"
    (*FOR CONST: for x in (e1..e2) do stml end *)
    | FOR(x, NFOR(n1, n2), stml) ->
       let rec for_con (x, (e1, e2), stml) env map st =                         (*意味関数F*)
         let IntVal n1,IntVal n2 = (eval_exp e1 env st, eval_exp e2 env st) in  (*e1,e2を評価*)
         if n1 = n2 then List.remove_assoc (lookup_envs x env) st               (* ストアからロケーションxを取り除く *)
         else
           let v = if n1 < n2 then (n1 + 1)
                   else (n1 - 1)
           in
           let st2 = ext_st st (lookup_envs x env) (IntVal v) in  (* μ[γ(x) -> n1 + 1 or n1 - 1 *)
           let st3 = eval_state stml env map st2 in               (* stml1回実行 *)
           for_con (x, (Const v, Const n2), stml) env map st3     (* 再帰 *)
       in
       let locs = (max_locs st) + 1 in                            (* 未使用のロケーションを取得 *)
       let env2 = ext_envs env x locs in                          (* γ[x->l] *)
       let st2 = ext_st st locs (eval_exp n1 env st) in           (* μ[l->n1 *)
       let st3 = eval_state stml env2 map st2 in                  (*stml1回実行*)
       for_con (x, (n1, n2), stml) env2 map st3                   (*意味関数Fへ*)
    (* FOR ARRAY: for x1 in (x2 or (rev)x2) do s end *)
    | FOR(x1, AFOR(rev, x2), stml) ->                    (* rev:bool trueの場合(rev)x2 falseの場合x2 *)
       let LocsVec(vec) = lookup_val x2 env st in        (* ベクトルを求める μ(γ(x2)) *)
       let rec for_array (x1, x2, stml, n) env map st =  (*意味関数A or B*)
         let flag, index = if rev = true 
                           then 0, (n - 1)
                           else (List.length vec) - 1, (n + 1)
         in
         if n = flag (* if n = x2.length - 1 or n = 0 *)
         then List.remove_assoc (lookup_envs x1 env) st      (*ストアstからx1のロケーションを解除*)
         else
           let locs = lookup_vec index vec in                (* ベクトルとindexからロケーションを求める *)
           let w = lookup_st locs st in                      (* 求めたロケーションｎ値を取得 *)
           let st2 = ext_st st (lookup_envs x1 env) w in     (* γ(x1) -> w *)
           let st3 = eval_state stml env map st2 in          (* stml1回実行 *)
           for_array (x1, x2, stml, index) env map st3       (* 再帰 *)
       in
       let index = if rev = true then (List.length vec) - 1  (* index求める *)
               else 0
       in
       let locs1 = lookup_vec index vec in
       let w = lookup_st locs1 st in
       let locs2 = (max_locs st) + 1 in
       let env2 = ext_envs env x1 locs2 in
       let st2 = ext_st st locs2 w in
       let st3 = eval_state stml env2 map st2 in
       for_array (x1, x2, stml, index) env2 map st3
    (*追加部分SWITCH*)
    | Switch(rev, obj1, case, obj2) ->
       let rec is_break n case_list =
         match case_list with
         | [] -> failwith "error in switch"
         | (m1, stml, m2, break) :: tl ->
            if n = m1 && break then true, stml, tl, case_list, m2
            else if n = m1 && not break then false, stml, tl,case_list, m2
            else is_break n tl
       in
       let rec fall_through case_list env map st =
         match case_list with
         | [] -> failwith "error in switch of fall_through"
         | (m1, stml, m2, break) :: tl ->
            let st2 = eval_state stml env map st in
            if break then st2
            else fall_through tl env map st2
       in
       let rec fall_through_rev case_list env map st =
         let rec search_case case_list =
           match case_list with
           | [] -> failwith "error in (rev)switch of fall_through"
           | (_, stml, _, break) :: tl ->
              if break then stml
              else stml @ (search_case tl)
         in
         let stml2 = List.rev(search_case case_list) in
         eval_state stml2 env map st
       in
       let _, IntVal(v1) = lval_val obj1 env in
       let locs_obj2, _ = lval_val obj2 env  in
       let flag, stml2, case_list2, case_list3, assertion = is_break v1 case in
       let st3 =
         if flag then eval_state stml2 env map st
         else
           if rev then fall_through_rev case_list3 env map st
           else
             let st2 = eval_state stml2 env map st in
             fall_through case_list2 env map st2
       in
       if (lookup_st locs_obj2 st3) = IntVal(assertion) then st3
       else failwith "assersion in switch is not correct"
    | Conditional(e1, stml1, stml2, e2) ->
       (*IFTRUE*)
       if (eval_exp e1 env st) <> IntVal(0) then
         let st2 = eval_state stml1 env map st in
         if (eval_exp e2 env st2) <> IntVal(0) then
           st2
         else failwith "assertion is incorrect in if-true"
       (*IFFALSE*)
       else if (eval_exp e1 env st) = IntVal(0) then
         let st2 = eval_state stml2 env map st in
         if (eval_exp e2 env st2) = IntVal(0) then
           st2
         else failwith "assertion is incorrect in if-false"
       else failwith "assertion is incorrect in if-false"
    (*LocalCALL*)
    | LocalCall(mid,idl) (* call q(y1,...,yn) *)->
       let locs = lookup_envs "this" env in                   (* γ(this) = l *)
       let LocsVal(locs2) = lookup_st locs st in              (* μ(l) = l' *)
       let ObjVal(id, envf)  = lookup_st locs2 st in          (* μ(l') = (c, γ') *)
       let (f, meth) = lookup_map id map in                   (* Γ(c) = (field, method) *)
       let MDecl(mid, para, mstml) = lookup_meth mid meth in  (* メソッド名がmidのメソッドを求める(q) *)
       let pidl = id_list para in                             (* pidl=仮引数のidのみのリスト(z1,...,zk)b *)
       let env2 = ext_env_meth env envf pidl idl in           (* 環境拡張 γ''=γ'[z1->γ(y1),...,zk->γ(yn)] *)
       let env3 = ext_envs env2 "this" locs in                (* 環境拡張 γ'''=γ''[this->l]*)
       eval_state mstml env3 map st                           (* メソッドの本体を実行 *)
    (*LocalUNCALL*)
    | LocalUncall(mid, idl) (* uncall q(y1,...,yn) *)->
       let locs = lookup_envs "this" env in
       let LocsVal(locs2) = lookup_st locs st in
       let ObjVal(id, envf) = lookup_st locs2 st in
       let (f, meth) = lookup_map id map in
       let MDecl(mid, para, mstml) = lookup_meth mid meth in
       let pidl = id_list para in
       let env2 = ext_env_meth env envf pidl idl in
       let env3 = ext_envs env2 "this" locs in
       eval_state (invert mstml) env3 map st (* メソッド本体を逆実行 *)
    (*CALLOBJ*)
    | ObjectCall(obj, mid, idl) (* call x0::q(y1,...,yn) *)->
       let locs, _ = lval_val obj env in              (* γ(x0)=l *) 
       let LocsVal(locs2) = lookup_st locs st in  (* 以下からlocalcallと同一 *)
       let ObjVal(id2, envf) = lookup_st locs2 st in
       let (f, meth) = lookup_map id2 map in
       let MDecl(mid, para, mstml) = lookup_meth mid meth in
       let pidl = id_list para in
       let env2 = ext_env_meth env envf pidl idl in
       let env3 = ext_envs env2 "this" locs in
       eval_state mstml env3 map st
    (*UNCALLOBJ*)
    | ObjectUncall(obj, mid, idl) (* uncall x0::q(y1,...,yn) *)->
       let locs, _ = lval_val obj env in
       let LocsVal(locs2) = lookup_st locs st in
       let ObjVal(id2, envf) = lookup_st locs2 st in
       let (f, meth) = lookup_map id2 map in
       let MDecl(mid, para, mstml) = lookup_meth mid meth in
       let pidl = id_list para in
       let env2 = ext_env_meth env envf pidl idl in
       let env3 = ext_envs env2 "this" locs in
       eval_state (invert mstml) env3 map st (* メソッド本体を逆実行 *)
    (*OBJBLOCK*)
    | ObjectBlock(tid, id, stml) (* construct c x  s destruct x *)->
       let (fl, ml) = lookup_map tid map in                 (* Γ(c)=(f1,...,fn, medhods) *)
       let max_locs = max_locs st in                        (* ロケーションの最大値を求める *)
       let locs = max_locs + 1 in                           (* locs = l *)
       let locs0 = max_locs + 2 in                          (* locs0 = l0 *)
       let locs1 = max_locs + 3 in                          (* locs1 = l1 *)
       let env2 = ext_envs env id locs in                   (* 環境拡張 γ[x->l] *)
       let envf = ext_env_field fl locs1 in                 (* 環境生成 γ'=[f1->l1,...,fn->ln] *)
       let st2 = ext_st_zero st locs1 (List.length fl) in   (* ストア拡張 μ'=μ[l1->0,...,ln->0 *)
       let st3 = ext_st st2 (locs0) (ObjVal(tid, envf)) in  (* ストア拡張 μ''=μ'[l0->(c,γ')]*)
       let st4 = ext_st st3 locs (LocsVal(locs0)) in        (* ストア拡張 μ'''=μ'[l->l0] *)
       let st5 = eval_state stml env2 map st4 in            (* sを実行 *)
       if is_field_zero st5 locs1 (List.length fl)          (*l1からlnがゼロクリアされているか確認*)
        then st5 else failwith "instance field is not zero-cleared"
    (*OBJNEW*)
    | ObjectConstruction(tid, obj) (* new c y *)->
       let (fl, ml) = lookup_map tid map in                (* Γ(c)=(f1,...,fn, methods) *)
       let locs, _ = lval_val obj env in                   (* l=γ(y) *)
       let max_locs = max_locs st in                       (* ロケーションの最大値を求める *)
       let locs0 = max_locs + 1 in                         (* locs0 = l0 *)  
       let locs1 = max_locs + 2 in                         (* locs1 = l1 *) 
       let envf = ext_env_field fl locs1 in                (* 環境生成 γ'=[f1->l1,...,fn->ln]*)
       let st2 = ext_st_zero st locs1 (List.length fl) in  (* ストア拡張 μ'=μ[l1->0,...,ln->0] *)
       let st3 = ext_st st2 locs0 (ObjVal(tid, envf)) in   (* ストア拡張 μ''=μ[l0->(c,γ')] *)
       ext_st st3 locs (LocsVal locs0)                     (* ストア拡張 μ'''=μ''[l->l0] *)
    (*OBJDELETE*)
    | ObjectDestruction(tid, obj) (* delete c y *)->
       let (fl, _) = lookup_map tid map in
       let locs, _ = lval_val obj env in                    (* l=γ(y) *)
       let LocsVal(locs0) = lookup_st locs st in            (* l0 *)
       let locs1 = locs0 + 1 in                             (* l1 *)
       if is_field_zero st locs1 (List.length fl) then      (* インスタンスフィールドがゼロクリアされているか確認 *)
       let st2 = delete_st st locs0 (1 + List.length fl) in (* ストアからロケーションl0',...,ln'を削除 *)
       ext_st st2 locs (IntVal 0)                           (* lの値をゼロクリア *)
       else failwith "instance field is not zero-cleared"
    (*ARRNEW*)
    | ArrayConstruction((tid, e), id) ->                                      (* new a[e] x *)
       let IntVal(n) = eval_exp e env st in                                   (* 要素数を求める *)
       let locs = lookup_envs id env in                                       (* xのロケーションを求める *)
       let st2 = ext_st st locs (LocsVec(gen_locsvec n (max_locs st + 1))) in (* ベクトルを生成({l'1,...,l'n}しストアに格納 *)
       ext_st_zero st2 (max_locs st2 + 1)  n                                  (* ストア拡張 μ[l'1->0,...,l'n->0 *)
    (*ARRDELETE*)
    | ArrayDestruction((tid, e), id) ->            (* delete a[e] x *)
       let IntVal(n) = eval_exp e env st in        (* 要素数を求める *)
       let veclocs = lookup_envs id env in         (* l=γ(x) *)
       let LocsVec(vec) = lookup_st veclocs st in  (* μ(l) = {l'1,...,l'n} *)
       let locs = lookup_vec 0 vec in              (* locs = l'1 *)
       if is_field_zero st locs n                  (* 配列要素すべてがゼロクリアされているか確認 *)
       then 
       let st2 = delete_arr st vec in              (* l'1からl'nのロケーションを削除 *)
       ext_st st2 veclocs (IntVal 0)               (* xのロケーションをゼロに初期化 *)
       else failwith "array elements is not zero-cleared"
    (*COPY*)
    | CopyReference(dt, obj1, obj2) ->      (* copy c x x' *)
       let locsx',_ = lval_val obj2 env in  (* v=μ(γ(x)) *)
       let _, vx = lval_val obj1 env in     (* l'=γ(x') *)
       ext_st st locsx' vx                  (* ストア更新μ[l'->v] *)
    (*UNCOPY*)
    | UncopyReference(_, _, obj) -> (* uncopy c _ x *)
       let locs,_ = lval_val obj env in
       ext_st st locs (IntVal 0)
    (*LOCALBLOCK*)
    | LocalBlock(dt, id, e1, stml, e2) -> (* local c x = e1  s  delocal x = e2 *)
       let v1 = eval_exp e1 env st in
       let locs = max_locs st + 1 in
       let env2 = ext_envs env id locs in
       let st2 = ext_st st locs v1 in
       let st3 = eval_state stml env2 map st2 in
       let v2 = eval_exp e2 env2 st3 in
       if lookup_st locs st3 = v2 then
         List.remove_assoc locs st3 
       else failwith "error: delocal value is not valid"
  in List.fold_left update st0 stml

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
