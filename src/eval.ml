(**評価器：式、文、プログラムを評価する*)
open Syntax
open Value
open Pretty
open Invert

let myassert (cond, msg) =  assert (if not cond then print_endline msg; cond)

(**環境を拡張する関数(同じ識別子がある場合古いものを削除し、新しいものを追加する)*)
let ext_envs env x v = (x,v) :: List.remove_assoc x env

(**ストアを拡張する関数(古いものを削除、昇順にソートされる)*)
let ext_st st x v = List.sort (fun x y -> if x < y then -1 else 1) ((x,v) :: (List.remove_assoc x st))

(**eval_stateで使用：locsからn-1までのロケーションに対応する値をすべてIntVal(0)にする関数*)
let rec ext_st_zero st locs n =
  if n <> 0 then
    ext_st (ext_st_zero st (locs + 1) (n - 1)) locs (IntVal(0))
  else
    st

(**マップ拡張用：eval_progで使用*)
let rec ext_map map c fm = (c, fm) :: map

(**第一引数に変数名、第２引数に環境を指定し、環境の中に指定した変数名があれば、
その変数のロケーションを返す関数*)
let lookup_envs x env =
  try snd (List.find (fun (y,_) -> x = y) env)
  with Not_found -> failwith ("ERROR:unbound variable: " ^ x)

(**第一引数にロケーション、第２引数にストアを指定し、ストアの中に指定したロケーションがあれば、
そのロケーションに格納されている値を返す関数*)
let lookup_st x st =
  try snd (List.find (fun (y,_) -> x = y) st)
  with Not_found -> failwith ("ERROR:unbound locations: " ^ (string_of_int x))

(**変数、環境、ストアを受け取り、その変数の値を返す*)
let lookup_val x env st = lookup_st (lookup_envs x env) st

(**ロケーションのベクトルから指定されたインデックスのロケーションを返す（添字は0から）*)
let rec lookup_vec index vec =
  match vec with
  | [] -> failwith "ERROR in lookup_vec"
  | l :: tl -> if index <> 0 then lookup_vec (index - 1) tl
               else l

(**演算子、式を受け取り、演算をする関数*)
let bin_op f v1 v2 =
  match v1, v2 with
    | IntVal(n1), IntVal(n2) -> IntVal(f n1 n2)
    | _ -> failwith "ERROR:integer values expected"

(**bin_op同様．関係演算子専用*)
let rel_op op v1 v2 =
  match v1, v2 with
  | IntVal(n1), IntVal(n2) ->
     if op (n1 <> 0) (n2 <> 0) then IntVal(1) else IntVal(0)
  | _ -> failwith "ERROR:integer values expected"

(**bin_op同様．比較算子専用*)
let comp_op f v1 v2 = IntVal(if f v1 v2 then 1 else 0)

(**式expressionを評価するための関数：環境、ストアを受け取り、値を返す．*)
let rec eval_exp exp env st =
  let rec lval_val y env =
    match y with
    | Var(x) -> let lv = lookup_envs x env in lv, lookup_st lv st
    | ArrayElement(x, e) ->
       let IntVal(x_index) = eval_exp e env st in
       let LocsVec(locsvecx) = lookup_val x env st in
       let locsx' = lookup_vec x_index locsvecx in
       let v = lookup_st locsx' st in
       locsx', v
    | Dot(x, xi) ->
       let _, locs = lval_val x env in
       match locs with
       | LocsVal(l)->
          begin
            match lookup_st l st with
            | ObjVal(c, env') ->
               let li, v = lval_val xi env' in
               li, v
            | _ -> failwith "not implemented"
          end
       | _ -> - 1, IntVal 0
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
     let locs2 = if index < List.length lv then index + List.hd lv
                 else failwith (pretty_exp exp ^ "\nERROR:Array index " ^ id ^ "[" ^
                                  string_of_int index ^ "] is out of bounds in this stament")
     in
     lookup_st locs2 st
  (*NIL*)
  | Nil -> IntVal(0)
  (* DOT *)
  | Dot(x, xi) ->
     let _, v = lval_val (Dot(x, xi)) env in v
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
     in
     try f b (eval_exp e1 env st) (eval_exp e2 env st) with
     | Failure e -> failwith (pretty_exp exp ^ "\n" ^ (e ^ " in this expression"))

(**ロケーションのベクトルを生成する関数：第一引数に要素数、第二引数に使われてないロケーションの場所を受け取る*)
let rec gen_locsvec n locs =
  if n <> 0 then locs :: gen_locsvec (n - 1) (locs + 1)
  else []

(** callの意味論の関数search_aに相当 *)
let rec search_a args env st locs =
  let search_a1 arg env st locs =
    match arg with
    | Id(id) -> lookup_envs id env
    | Exp(e) -> locs + 1
  in
  match args with
  | [] -> []
  | hd :: tl -> search_a1 hd env st locs :: search_a tl env st (locs + 1)

(** callの意味論の関数argに相当 *)
let argv arg env st =
  match arg with
  | Id(id) -> lookup_val id env st
  | Exp(e) -> eval_exp e env st

(** callの意味論の関数remove_aに相当 *)
let rec remove_a argl locsl vl st =
  let remove_a1 arg locs v st =
    match arg with
    | Id(id) -> st
    | Exp(_) -> if lookup_st locs st = v then
                  List.remove_assoc locs st
                else failwith "ERROR: formal argument and actual argument are not same value in this statement"
  in
  match argl, locsl, vl with
  | [], [], [] -> st
  | arg :: arg_tl, locs :: locs_tl, v :: v_tl ->
     remove_a arg_tl locs_tl v_tl (remove_a1 arg locs v st)
  | _ -> failwith "not implemented"

(**オブジェクトフィールドの値がすべてゼロクリアされているか確認する関数*)
let rec is_field_zero st locs n =
  n = 0 ||
    let flag = lookup_st locs st = IntVal(0) in
    flag && is_field_zero st (locs + 1) (n - 1)

(**メソッドのリストから指定したメソッド名のメソッドを返す関数*)
let rec lookup_meth x vl meth =
  try List.find (fun (MDecl(id, para, _)) ->
          x = id && List.length vl = List.length para) meth
  with Not_found -> failwith ("ERROR: Method " ^ x ^ " does not exist or wrong number of arguments for the function")

(**マップのリストから指定されたクラス名のfieldとメソッドのタプルを返す*)
let rec lookup_map id map =
  try snd (List.find (fun (x , _) -> x = id) map)
  with Not_found -> failwith ("ERROR:class " ^ id ^ " is not valid")

(**環境に指定されたメソッドのフィールドを使われていないロケーションに追加する．eval_stateのOBJBLOKで使用*)
let rec ext_env_field f n =
  match f with
  | [] -> []
  | Decl(dtype, id) :: tl -> ext_envs (ext_env_field tl (n + 1)) id n

(**文statementを実行する関数：第一引数に文、第二引数にオブジェクトブロックを指すロケーションと環境のタプル、
第三引数にマップ、第四引数にストアを受け取り、更新されたストアを返す．*)
let rec eval_state stml env map st0 =
  (** ストアのロケーションの最大値を求める *)
  let max_locs st = List.fold_left (fun x y -> if x < y then y else x) 0 (List.map fst st) in
  let isTrue = function
    | IntVal(0) -> false
    | IntVal(_) -> true
    | _ -> failwith "ERROR in isTrue" in
  let isFalse x = not (isTrue x) in
  let f = function
    | ModAdd -> (+)
    | ModSub -> (-)
    | ModXor -> (lxor) in
  let update st stm =
    (** y (= x or x[n] or y.y) を受けとりそのロケーションと値を返す *)
    let rec lval_val y env =
    match y with
    | VarArray(x, None) -> let lv = lookup_envs x env in lv, lookup_st lv st
    | VarArray(x, Some e) ->
       let IntVal(x_index) = eval_exp e env st in
       let LocsVec(locsvecx) = lookup_val x env st in
       let locsx' =
         if x_index < List.length locsvecx then x_index + List.hd locsvecx
         else failwith ((pretty_stms [stm] 0) ^ "\nERROR:Array index " ^ x ^ "[" ^
                          (string_of_int x_index) ^ "] is out of bounds in this stament")                
       in
       let v = lookup_st locsx' st in (**the value of x[e1]*)
       locsx', v
    | InstVar(x, xi) ->
       let _, locs = lval_val x env in
       match locs with
         LocsVal(l) ->
          begin
            match (lookup_st l st) with
            | ObjVal(c, env') ->
               let li, v = lval_val xi env' in
               li, v
          end
       | _ -> - 1, IntVal 0
    in
    (** call処理の共通部分を実行する関数．invertFlagが1なら逆実行 *)
    let mycall locs locs2 invertFlag =
      match stm with
      | LocalCall (mid0, args) | LocalUncall(mid0, args) | ObjectCall(_, mid0, args) | ObjectUncall(_, mid0, args) ->
         let vl = List.map (fun x -> argv x env st) args in     (* v_i = arg(a_i, γ, μ) (実引数の値を求める) *)         
         let ObjVal(id, envf)  = lookup_st locs2 st in          (* μ(l') = (c, γ') *)
         let (f, meth) = try lookup_map id map with                   (* Γ(c) = (field, method) *)
           | Failure str -> failwith ((pretty_stms [stm] 0) ^ "\n" ^ str ^ " in this statement") in
         let MDecl(mid, para, mstml) = lookup_meth mid0 vl meth in  (* メソッド名がmidのメソッドを求める(q) *)
         let pidl = List.map (fun (Decl(_, id)) -> id) para in  (* pidl=仮引数のidのみのリスト(z1,...,zk) *)
         let arg_locsl = search_a args env st (max_locs st) in  (* [l'_1...l'n] = search_a(a_i, γ, μ) (実引数のロケーションを求める) *)
         let env2 = List.fold_left2 ext_envs envf pidl arg_locsl in  (* 環境拡張 γ''=γ'[z1->l'1,...,zk->l'n] *)
         let env3 = ext_envs env2 "this" locs in                (* 環境拡張 γ'''=γ''[this->l]*)      
         let st2 = List.fold_left2 ext_st st arg_locsl vl in    (* ストア拡張 μ'=μ[l'_1 -> v_1,...,l'_n -> v_n] *)
         let mstml2 = if invertFlag = 1 then invert mstml else mstml in
         let st3 = eval_state mstml2 env3 map st2 in             (* メソッドの本体を実行 *)
         (* 実引数が式の場合、メソッド実行後の仮引数の値と実引数の値が等しいか確認し、等しければストアから取り除く．等しくなければ、エラー表示．実引数が変数の場合、そのまま． *)
         begin
           try (remove_a args arg_locsl vl st3) with
           | Failure str -> failwith (pretty_stms [stm] 0 ^ "\n" ^ str)
         end
      | _ -> failwith "not implemented"
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
       let v' = try (bin_op (f op) vx v) with
                | Failure e -> failwith  ((pretty_stms [stm] 0) ^ "\nERROR:Integer value expected in this statement")
       in
       ext_st st lvx v' (* the right value of x *)
    (*SWPVAR*) (*SWAPARRVAR*)
    | Swap(y1, y2) (*y1 <=> y2*)->
       let lv1, v1 = lval_val y1 env in
       let lv2, v2 = lval_val y2 env in
       let st2 = ext_st st lv1 v2 in (*update y2 -> y1*)
       ext_st st2 lv2 v1 (*update y1 -> y2*)
    | Loop(e1, stml1, stml2, e2) ->                          (* from e1 do s1 loop s2 until e2 *)
       let rec eval_loop (e1, stml1, stml2, e2) env map st = (* 意味関数L *)
         (*LOOPREC*)
         if isFalse (eval_exp e2 env st) then           (* ?e2 = 0(false) *)
           let st2 = eval_state stml2 env map st in          (* s2実行 *)
           if isFalse (eval_exp e1 env st2) then              (* アサーション ?e1 = 0(false) *)
             let st3 = eval_state stml1 env map st2 in       (* 満たす場合、s1実行 *)
             eval_loop (e1, stml1, stml2, e2) env map st3    (* 意味関数L繰り返し *)
           else
             failwith ((pretty_stms [stm] 0) ^ "\nERROR:Assertion should be false in this statement")             
         (*LOOPBASE*)
         else
           (myassert(isTrue (eval_exp e2 env st), (pretty_stms [stm] 0) ^ "ERROR:assertion is incorrect in this statement"); st)
       in
       (* LOOPMAIN *)
       if isTrue (eval_exp e1 env st) then                   (* アサーション ?e1 != 0(true) *) 
         let st2 = eval_state stml1 env map st in            (* s1実行 *)
         eval_loop (e1, stml1, stml2, e2) env map st2        (* 意味関数Lへ *)
       else
         failwith ((pretty_stms [stm] 0) ^ "\nERROR:Assertion should be true in this statement")
    (*FOR CONST: for x in (e1..e2) do stml end *)
    | For(x, e1, e2, stml) ->
       let rec for_con (x, (e1, e2), stml) env map st =                         (*意味関数F*)
         let IntVal n1, IntVal n2 = eval_exp e1 env st, eval_exp e2 env st in   (*e1,e2を評価*)
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
       let st2 = ext_st st locs (eval_exp e1 env st) in           (* μ[l->n1 *)
       let st3 = eval_state stml env2 map st2 in                  (*stml1回実行*)
       if (lookup_val x env2 st2) = (lookup_val x env2 st3) then
         for_con (x, (e1, e2), stml) env2 map st3                 (*意味関数Fへ*)
       else failwith ((pretty_stms [stm] 0) ^ "\nERROR:Variable "^ x ^ " must not change in this for statement")
    (*追加部分SWITCH*)
    | Switch(obj1, cases, stml, obj2) ->
       let rec search_case n cases cnt env st =
         match cases with
         | [] -> failwith ((pretty_stms [stm] 0) ^ "\nERROR:not match in this case statement")
         | (c, e, s, e', b) :: tl ->
            if n = (eval_exp e env st) then (c, e, s, e', b), cnt
            else search_case n tl (cnt + 1) env st
       in
       let rec search_ecase cases n cnt =
         match cases with
         | [] -> failwith "ERROR:not match in ecase"
         | (c, _, _, _, _) :: tl ->
              if (c = Ecase) && (cnt > n) then cnt
              else search_ecase tl n (cnt + 1)
       in
       let rec search_state cases k j =
         let (_, _, stm, _, _) = List.nth cases k in
         if k = j then stm
         else stm @ (search_state cases (k + 1) j)
       in
       let rec eval_case j k cases env map st =
         if k = - 1 then
           let m = search_ecase cases j 0 in
           eval_state (search_state cases j m) env map st
         else
         let (ck, _, _, _, bk) = List.nth cases k in
         match ck, bk with
         | Ecase, _ -> eval_state (search_state cases k j) env map st
         | _ , Break ->
            let m = search_ecase cases j 0 in
            eval_state (search_state cases j m) env map st
         | _ ->  eval_case j (k - 1) cases env map st
       in
       let _, v1 = lval_val obj1 env in
       let v2_locs, _ = lval_val obj2 env in
       let (c, e, s, e', b), index =
         try search_case v1 cases 0 env st with
           Failure str -> let (_, e, _, e', b) = List.hd(List.rev cases) in
                          (Case, e, stml, e', b), -1
       in
       let st2 =
         begin
         match c with
         | Fcase -> eval_case index (index - 1) cases env map st
         | _ -> eval_state s env map st
         end
       in
       if index = -1 then
         if lookup_st v2_locs st2 <> eval_exp e' env st2 then st2
         else failwith ((pretty_stms [stm] 0) ^ "\nERROR:default assertion is incorrect in this switch statement")
       else if lookup_st v2_locs st2 = eval_exp e' env st2 then st2
       else
         failwith ((pretty_stms [stm] 0) ^ ("\nERROR:assertion is incorrect when " ^ (pretty_obj obj1) ^ " = " ^ (pretty_exp e) ^ " in this switch statement"))
    | Conditional(e1, stml1, stml2, e2) ->           (* if e1 then s1 else s2 fi e2 *)
       (*IFTRUE*)
       if isTrue (eval_exp e1 env st) then     (* ?e1 != 0(true)  *)
         let st2 = eval_state stml1 env map st in    (* s1実行 *)
         if isTrue (eval_exp e2 env st2) then  (* アサーション ?e2 != 0(true) *)
           st2
         else                                        (* アサーションを満たさない場合のエラー *)
           failwith ((pretty_stms [stm] 0) ^ "\nERROR:Assertion should be true in this statement")           
       (*IFFALSE*)
       else if isFalse (eval_exp e1 env st) then (* ?e1 = 0(false) *)
         let st2 = eval_state stml2 env map st in    (* s2実行 *)
         if isFalse (eval_exp e2 env st2) then   (* アサーション ?e2 = 0(false) *)
           st2
         else                                        (* アサーションを満たさない場合のエラー *)
           failwith ((pretty_stms [stm] 0) ^ "\nERROR:Assertion should be false in this statement")
       else
         failwith ((pretty_stms [stm] 0) ^ "\nERROR:Assertion should be false in this statement")
    (*LocalCALL*)
    | LocalCall(mid, args) (* call q(y1,...,yn) *)->
       let locs = lookup_envs "this" env in                   (* γ(this) = l *)
       let LocsVal(locs2) = lookup_st locs st in              (* μ(l) = l' *)
       mycall locs locs2 0
    (*LocalUNCALL*)
    | LocalUncall(mid, args) (* uncall q(y1,...,yn) *)->
       let locs = lookup_envs "this" env in                   (* γ(this) = l *)
       let LocsVal(locs2) = lookup_st locs st in              (* μ(l) = l' *)
       mycall locs locs2 1
    (*CALLOBJ*)
    | ObjectCall(obj, mid, args) (* call x0::q(a1,...,an) *)->
       let locs, LocsVal(locs2) = lval_val obj env in         (* l = search_l(x0), l' = μ(l) *)
       mycall locs locs2 0
    (*UNCALLOBJ*)
    | ObjectUncall(obj, mid, args) (* uncall x0::q(a1,...,an) *)->
       let locs, LocsVal(locs2) = lval_val obj env in         (* l = search_l(x0), l' = μ(l) *)
       mycall locs locs2 1
    (*OBJBLOCK*)
    | ObjectBlock(tid, id, stml) (* construct c x  s destruct x *)->
       let (fl, ml) = try lookup_map tid map with           (* Γ(c)=(f1,...,fn, medhods) *)
         | Failure str -> failwith ((pretty_stms [stm] 0) ^ "\n" ^ str ^ " in this statement") in
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
       then st5 else
         failwith ((pretty_stms [stm] 0) ^ "\nERROR:" ^ (id ^ "'s instance field is not zero-cleared in this statement"))
    (*OBJNEW*)
    | ObjectConstruction(tid, obj) (* new c y *)->
       let (fl, ml) = try lookup_map tid map with          (* Γ(c)=(f1,...,fn, methods) *)
         | Failure str -> failwith ((pretty_stms [stm] 0) ^ "\n" ^ str ^ " in this statement") in
       let locs, v = lval_val obj env in                   (* l=γ(y) *)
       if v <> IntVal(0) then                              (* yがnilか確認 *)
         failwith ((pretty_stms [stm] 0) ^ "\nERROR: variable is not nil in this statement")
       else
       let max_locs = max_locs st in                       (* ロケーションの最大値を求める *)
       let locs0 = max_locs + 1 in                         (* locs0 = l0 *)
       let locs1 = max_locs + 2 in                         (* locs1 = l1 *)
       let envf = ext_env_field fl locs1 in                (* 環境生成 γ'=[f1->l1,...,fn->ln]*)
       let st2 = ext_st_zero st locs1 (List.length fl) in  (* ストア拡張 μ'=μ[l1->0,...,ln->0] *)
       let st3 = ext_st st2 locs0 (ObjVal(tid, envf)) in   (* ストア拡張 μ''=μ[l0->(c,γ')] *)
       ext_st st3 locs (LocsVal locs0)                     (* ストア拡張 μ'''=μ''[l->l0] *)
    (*OBJDELETE*)
    | ObjectDestruction(tid, obj) (* delete c y *)->
       (**ベクトルの要素を削除するための関数：locsからnまでのロケーションを削除*)
       let rec delete_st st locs n =
         if n <> 0 then delete_st (List.remove_assoc locs st) (locs + 1) (n - 1)
         else st in
       let (fl, _) = try lookup_map tid map with
         | Failure str -> failwith ((pretty_stms [stm] 0) ^ "\n" ^ str ^ " in this statement") in
       let locs, _ = lval_val obj env in                    (* l=γ(y) *)
       let LocsVal(locs0) = lookup_st locs st in            (* l=μ(l0) *)
       let ObjVal(_, envf) = lookup_st locs0 st in          (* (c,γ')=μ(l0) *)
       let locs1 = if List.length envf = 0 then 0
                   else List.hd (List.map snd envf) in      (* locs1=l1 *)
       if is_field_zero st locs1 (List.length fl) then      (* インスタンスフィールドがゼロクリアされているか確認 *)
         let st2 = delete_st st locs1 (List.length fl) in   (* ストアからロケーションl1,...,lnを削除 *)
         let st3 = List.remove_assoc locs0 st2 in           (* ストアからロケーションl0を削除 *) 
         ext_st st3 locs (IntVal 0)                         (* lの値をゼロクリア *)
       else
         failwith ((pretty_stms [stm] 0) ^ "\nERROR:All instance field is not zero-cleared in this statement")
    (*ARRNEW*)
    | ArrayConstruction((tid, e), obj) ->                                      (* new a[e] x *)
       let locs, v = lval_val obj env in                                       (* xのロケーションを求める *)
       if v <> IntVal(0) then                                                  (* xがnilか確認 *)
         failwith ((pretty_stms [stm] 0) ^ "\nERROR:Variable is not nil in this statement")
       else
       let IntVal(n) = eval_exp e env st in                                   (* 要素数を求める *)
       let st2 = ext_st st locs (LocsVec(gen_locsvec n (max_locs st + 1))) in (* ベクトルを生成({l'1,...,l'n}しストアに格納 *)
       ext_st_zero st2 (max_locs st2 + 1)  n                                  (* ストア拡張 μ[l'1->0,...,l'n->0 *)
    (*ARRDELETE*)
    | ArrayDestruction((tid, e), obj) ->           (* delete a[e] x *)
       let IntVal(n) = eval_exp e env st in        (* 要素数を求める *)
       let veclocs,_ = lval_val obj env in         (* l=γ(x) *)
       let LocsVec(vec) = lookup_st veclocs st in  (* μ(l) = {l'1,...,l'n} *)
       let locs = lookup_vec 0 vec in              (* locs = l'1 *)
       if is_field_zero st locs n                  (* 配列要素すべてがゼロクリアされているか確認 *)
       then
       let st2 = List.fold_left (fun x y -> List.remove_assoc y x) st vec in              (* l'1からl'nのロケーションを削除 *)
       ext_st st2 veclocs (IntVal 0)               (* xのロケーションをゼロに初期化 *)
       else
         failwith (pretty_stms [stm] 0 ^ "\nERROR:All array elements is not zero-cleared in this statement")
    (*COPY*)
    | CopyReference(dt, obj1, obj2) ->      (* copy c x x' *)
       let locsx',v = lval_val obj2 env in  (* v=μ(γ(x)) *)
       if v <> IntVal(0) then               (* x'がnilか確認 *)
         failwith (pretty_stms [stm] 0 ^ "\nERROR:variable of right is not nil in this statement")
       else
       let _, vx = lval_val obj1 env in     (* l'=γ(x') *)
       ext_st st locsx' vx                  (* ストア更新μ[l'->v] *)
    (*UNCOPY*)
    | UncopyReference(dt, obj1, obj2) -> (* uncopy c x x' *)
       let _, v1 = lval_val obj1 env in (* 変数xの値を求める *)
       let locs, v2 = lval_val obj2 env in (* 変数x'の値を求める *)
       if v1 = v2 then                   (* 同じ領域を指しているか確認 *)
         ext_st st locs (IntVal 0)       (* 変数x'をゼロクリア *)
       else
         failwith (pretty_stms [stm] 0 ^ "\nERROR:both variable's reference is not same in this statement")
    (*LOCALBLOCK*)
    | LocalBlock(dt, id, e1, stml, e2) -> (* local c x = e1  s  delocal x = e2 *)
       let v1 = eval_exp e1 env st in     (* e1の値を求める *)
       let locs = max_locs st + 1 in      (* 未使用のロケーションを求める *)
       let env2 = ext_envs env id locs in (* 環境に変数xを追加 *)
       let st2 = ext_st st locs v1 in     (* ストアに変数xを追加 *)
       let st3 = eval_state stml env2 map st2 in (* sを実行 *)
       let v2 = eval_exp e2 env2 st3 in          (* e2の値を求める *)
       if lookup_st locs st3 = v2 then    (* x = e2 を満たすか確認 *)
         List.remove_assoc locs st3       (* xをストアから取り除く *)
       else
         failwith (pretty_stms [stm] 0 ^
                     "\nERROR: Variable " ^ id ^ " = " ^ Print.show_val (lookup_st locs st3) ^ ", But it should be " ^ Print.show_val v2 ^ " in this statement")
  in List.fold_left update st0 stml

(**mainメソッドがあるクラスのフィールドから環境を生成する関数　eval_progでのみ使用*)
let gen_env fid1 : env =
  let rec gen_env_2 fid2 n =
    match fid2 with
    | [] -> [("this", n)]
    | id :: tl -> ext_envs (gen_env_2 tl (n + 1)) id n
  in
  gen_env_2 fid1 1

(**ストアを生成する関数：eval_progでのみ使用*)
let gen_st env1 objval =
  let rec gen_st2 env2 objval n =
    match env2 with
    | [] -> failwith "ERROR"
    | [f] -> [(n, LocsVal (n + 1)); (n + 1, objval)]
    | f :: tl -> ext_st (gen_st2 tl objval (n + 1)) n (IntVal(0))
  in
  gen_st2 env1 objval 1

(**結果を生成する関数：eval_progでのみ使用*)
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

(**指定したクラスidのクラスを返す関数　α^-1に相当 map_fieldとmap_methodで使用*)
let rec lookup_class_map clist cid =
  match clist with
  | [] -> failwith ("ERROR:class "^ cid ^ " is not exist")
  | CDecl(id, tid, fl, m) :: tl ->
     if cid = id then CDecl(id, tid, fl, m)
     else lookup_class_map tl cid

(**gen_mapで使用する関数 ROOPL++26ページの関数fieldに相当*)
let rec map_field clist1 cl =
  match cl with
  | CDecl(id, None, fl, m) -> fl
  | CDecl(id, Some(cid), fl, m) ->
     let parent_class = lookup_class_map clist1 cid in (*a^-1(c')*)
     let parent_method = map_field clist1 parent_class in
     parent_method @ fl

(**gen_mapで使用する関数 ROOPL++26ページの関数methodに相当*)
let rec map_method clist1 cl =
  (**メソッドのリストに指定した名前のメソッド名があるか調べる関数*)
  let rec lookup_methid id = function
    | [] -> false
    | MDecl(mid, _, _) :: tl -> id = mid || lookup_methid id tl
  in
  (**サブクラスに親クラスと同じ名前のメソッドがある場合親クラスからそのメソッドを削除し、サブクラスの同じ名前のメソッドを追加する関数(オーバーライド)*)
  let method_union subm parem =
    (**親クラスがサブクラスと同じ名前のメソッドをもつ場合そのメソッドを削除する関数*)
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

(**マップを生成する関数*)
let gen_map clist =
  let rec gen_map2 clist1 clist2 =
    (**クラスからidを取り出す関数 gen_mapで使用*)
    let lookup_cid (CDecl(id, _, _, _)) = id in
    match clist2 with
    | [] -> []
    | cl :: tl ->
       (lookup_cid cl, (map_field clist1 cl, map_method clist1 cl)) :: (gen_map2 clist1 tl)
  in
  gen_map2 clist clist

  (**マップのリストから指定されたメソッド名を含んでいるクラス名とそのメソッドの文のタプルを返す*)
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
         | _ -> failwith ("ERROR:method " ^ id1 ^ "is not exist")
       end
  | [] -> failwith ("ERROR, not found")

(**プログラムを実行する関数（クラスリストを受け取り、環境とストアの組を返す）*)
let eval_prog ?(library0 = Prog []) (Prog(cl)) =
  let Prog(cl2) = library0 in
  (*マップ生成*)
  let map0 = gen_map cl2 in
  (*標準ライブラリ読み込み*)
  let map = map0 @ (gen_map cl) in
  (*mainメソッドを含んでいるクラスidとメソッドの文を取得*)
  let (mid, mainstml) = lookup_class "main" map in
  (*mainメソッドを含んでいるクラスのフィールドを取得*)
  let (field, _) = lookup_map mid map in
  (*フィールドを識別子のみのリストに変換*)
  let fid = List.map (fun (Decl(_, id)) -> id) field in
  (*フィールドから環境を生成*)
  let env = gen_env fid in
  (*環境からストアを生成*)
  let st = gen_st env (ObjVal(mid, env)) in
  (*mainメソッドの処理を実行*)
  let st2 = eval_state mainstml env map st in
  (*結果を生成(mainメソッドを含んでいるクラスのフィールドとそれに対応する値の組を返す)*)
  List.remove_assoc "this" (gen_result env st2)
