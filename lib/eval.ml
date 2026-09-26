(**評価器：式、文、プログラムを評価する*)
open Syntax
open Value
open Pretty
open Invert

(**すでに「どの文で起きたか」を含む実行時エラーを投げる。
   Failure ではなく Util.Runtime_error を使うことで、文を包むラッパが
   同じ文を二重に付けないようにする。 *)
let fail_stm msg = raise (Util.Runtime_error msg)

(**環境を拡張する関数(同じ識別子がある場合古いものを削除し、新しいものを追加する)*)
let ext_envs env x v = (x,v) :: List.remove_assoc x env

(**ストアを拡張する関数(古いものを削除、昇順にソートされる)*)
let ext_st st x v = List.sort (fun x y -> compare (fst x) (fst y)) ((x,v) :: (List.remove_assoc x st))

(**eval_stateで使用：locsからn-1までのロケーションに対応する値をすべてIntVal(0)にする関数*)
let rec ext_st_zero st locs n =
  (* n > 0 で止める（gen_locsvec と同じ理由） *)
  if n > 0 then
    ext_st (ext_st_zero st (locs + 1) (n - 1)) locs (IntVal(0))
  else
    st

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
  | [] -> failwith "ERROR:index out of bounds in lookup_vec"
  | l :: tl -> if index > 0 then lookup_vec (index - 1) tl
               else if index = 0 then l
               else failwith "ERROR:negative index in lookup_vec"

(**ゼロ除算チェック付き除算*)
let safe_div n1 n2 =
  if n2 = 0 then failwith "ERROR:division by zero"
  else n1 / n2

(**ゼロ除算チェック付き剰余*)
let safe_mod n1 n2 =
  if n2 = 0 then failwith "ERROR:modulo by zero"
  else n1 mod n2

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
  (* [ienv] は**添字の式**を評価する環境。名前の解決 [env] とは別に持ち回る:
     ドットの右側（o.xs[i]）では、フィールド名 xs はオブジェクト側の環境で
     引くが、添字 i は呼び出し側のスコープの変数である。両方を env' にすると
     o.xs[k] の k がオブジェクトのフィールド k を指してしまう *)
  let rec lval_val_in ienv y env =
    match strip_epos y with
    | Var(x) -> let lv = lookup_envs x env in lv, lookup_st lv st
    | ArrayElement(x, e) ->
       let x_index = match eval_exp e ienv st with
         | IntVal(n) -> n
         | _ -> failwith "ERROR:array index must be an integer" in
       let locsvecx = match lookup_val x env st with
         | LocsVec(v) -> v
         | _ -> failwith "ERROR:expected array value" in
       let locsx' =
         if x_index >= 0 && x_index < List.length locsvecx
         then x_index + List.hd locsvecx
         else failwith ("ERROR:Array index " ^ x ^ "[" ^ string_of_int x_index
                        ^ "] is out of bounds in this statement") in
       let v = lookup_st locsx' st in
       locsx', v
    | Dot(x, xi) ->
       let _, locs = lval_val_in ienv x env in
       (match locs with
       | LocsVal(l)->
          (match lookup_st l st with
            | ObjVal(_c, env') ->
               (* 名前は env' で引くが、添字は ienv のまま *)
               let li, v = lval_val_in ienv xi env' in
               li, v
            | _ -> failwith "ERROR:Field access needs an object on the left of the dot, but it holds no object here")
       | _ -> failwith "ERROR:Field access needs an object on the left of the dot, but the value is nil or an integer")
    | _ -> failwith "ERROR:not an l-value expression"
  in
  let lval_val y env = lval_val_in env y env in
  match exp with
  (* 位置つきの式：中で落ちたらいちばん内側の位置を例外に載せる *)
  | EPos(p, e) ->
     (try eval_exp e env st with
      | Failure m ->
         raise (Util.Expr_error ((p.line, p.col, p.end_line, p.end_col), m)))
  (*CON*)
  | Const(n) -> IntVal(n)
  (*VAR*)
  | Var(x) -> lookup_st (lookup_envs x env) st
  (*ARRELEM*)
  | ArrayElement(id, e) ->
     let index = match eval_exp e env st with
       | IntVal(n) -> n
       | _ -> failwith "ERROR:array index must be an integer" in
     let locs = lookup_envs id env in
     let lv = match lookup_st locs st with
       | LocsVec(v) -> v
       | _ -> failwith "ERROR:expected array value" in
     let locs2 = if index >= 0 && index < List.length lv then index + List.hd lv
                 else failwith (pretty_exp exp ^ "\nERROR:Array index " ^ id ^ "[" ^
                                  string_of_int index ^ "] is out of bounds in this statement")
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
       | Div  -> bin_op safe_div
       | Mod  -> bin_op safe_mod
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
     (try f b (eval_exp e1 env st) (eval_exp e2 env st) with
      | Failure e -> failwith (pretty_exp exp ^ "\n" ^ e ^ " in this expression")
      (* 位置は内側のものを保ち、メッセージだけ積み増す *)
      | Util.Expr_error (sp, e) ->
         raise (Util.Expr_error (sp, pretty_exp exp ^ "\n" ^ e ^ " in this expression")))

(**ロケーションのベクトルを生成する関数：第一引数に要素数、第二引数に使われてないロケーションの場所を受け取る*)
let rec gen_locsvec n locs =
  (* n > 0 で止める。n <> 0 だと負の要素数で無限再帰して Stack_overflow になる *)
  if n > 0 then locs :: gen_locsvec (n - 1) (locs + 1)
  else []

(** callの意味論の関数search_aに相当 *)
let rec search_a args env st locs =
  let search_a1 arg env _st locs =
    match arg with
    | Id(id) -> lookup_envs id env
    | Exp(_) -> locs + 1
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
    | Id(_) -> st
    | Exp(_) -> if lookup_st locs st = v then
                  List.remove_assoc locs st
                else failwith "ERROR: formal argument and actual argument are not same value in this statement"
  in
  match argl, locsl, vl with
  | [], [], [] -> st
  | arg :: arg_tl, locs :: locs_tl, v :: v_tl ->
     remove_a arg_tl locs_tl v_tl (remove_a1 arg locs v st)
  | _ -> failwith "ERROR:mismatched argument list lengths in method call"

(**オブジェクトフィールドの値がすべてゼロクリアされているか確認する関数*)
let rec is_field_zero st locs n =
  n = 0 ||
    let flag = lookup_st locs st = IntVal(0) in
    flag && is_field_zero st (locs + 1) (n - 1)

(**メソッドのリストから指定したメソッド名のメソッドを返す関数*)
let lookup_meth x vl meth =
  try List.find (fun (MDecl(id, para, _)) ->
          x = id && List.length vl = List.length para) meth
  with Not_found -> failwith ("ERROR: Method " ^ x ^ " does not exist or wrong number of arguments for the function")

(**マップのリストから指定されたクラス名のfieldとメソッドのタプルを返す*)
let lookup_map id map =
  try snd (List.find (fun (x , _) -> x = id) map)
  with Not_found -> failwith ("ERROR:Class " ^ id ^ " is not declared in this program")

(**環境に指定されたメソッドのフィールドを使われていないロケーションに追加する．eval_stateのOBJBLOKで使用*)
let rec ext_env_field f n =
  match f with
  | [] -> []
  | Decl(_dtype, id) :: tl -> ext_envs (ext_env_field tl (n + 1)) id n

(**式に自由に現れる変数名（いまの環境で解決されるものだけ）。
   ドットの右側はオブジェクト側の環境で解決されるので数えない*)
let rec free_vars e =
  match e with
  | Const _ | Nil -> []
  | Var(x) -> [x]
  | ArrayElement(x, e1) -> x :: free_vars e1
  | Binary(_, e1, e2) -> free_vars e1 @ free_vars e2
  | Dot(x, _) -> free_vars x
  | EPos(_, e1) -> free_vars e1

(**l 値が添字を含むか。含むなら書き込みでロケーションが動きうる*)
let rec obj_has_index = function
  | VarArray(_, None) -> false
  | VarArray(_, Some _) -> true
  | InstVar(x, xi) -> obj_has_index x || obj_has_index xi

(**文statementを実行する関数：第一引数に文、第二引数にオブジェクトブロックを指すロケーションと環境のタプル、
第三引数にマップ、第四引数にストアを受け取り、更新されたストアを返す．*)
let rec eval_state stml env map st0 =
  (* ストアのロケーションの最大値を求める *)
  let max_locs st = List.fold_left max 0 (List.map fst st) in
  let isTrue = function
    | IntVal(0) -> false
    | IntVal(_) -> true
    | _ -> failwith "ERROR:Integer value expected in the condition of this statement" in
  let isFalse x = not (isTrue x) in
  let f = function
    | ModAdd -> (+)
    | ModSub -> (-)
    | ModXor -> (lxor) in
  let rec update st stm =
    (* y (= x or x[n] or y.y) を受けとりそのロケーションと値を返す。
        ストアを引数に取るのは、書き込んだ後の状態でもう一度解決して
        「l 値が自分の書き込みで動いていないか」を確かめるため *)
    let rec lval_val_in ?(ienv = env) st y env =
    match y with
    | VarArray(x, None) -> let lv = lookup_envs x env in lv, lookup_st lv st
    | VarArray(x, Some e) ->
       let x_index = match eval_exp e ienv st with
         | IntVal(n) -> n
         | _ -> failwith "ERROR:array index must be an integer" in
       let locsvecx = match lookup_val x env st with
         | LocsVec(v) -> v
         | _ -> failwith "ERROR:expected array value" in
       let locsx' =
         if x_index >= 0 && x_index < List.length locsvecx then x_index + List.hd locsvecx
         else fail_stm (pretty_stms [stm] 0 ^ "\nERROR:Array index " ^ x ^ "[" ^
                          string_of_int x_index ^ "] is out of bounds in this statement")
       in
       let v = lookup_st locsx' st in (*the value of x[e1]*)
       locsx', v
    | InstVar(x, xi) ->
       let _, locs = lval_val_in ~ienv st x env in
       (match locs with
         LocsVal(l) ->
          (match lookup_st l st with
            | ObjVal(_c, env') ->
               (* 名前は env' で引くが、添字は呼び出し側の ienv のまま *)
               let li, v = lval_val_in ~ienv st xi env' in
               li, v
            | _ -> failwith "ERROR:Field access needs an object on the left of the dot, but it holds no object here")
       | _ -> failwith "ERROR:Field access needs an object on the left of the dot, but the value is nil or an integer")
    in
    let lval_val y env = lval_val_in st y env in
    (* 書き込みで l 値のロケーションが動いていないことを確認する。
        coq/roopl.v の E_aassign / E_aswap の前提「eval ei b = eval ei a」
        （添字が自分の書き込みで変わらない）にあたる。添字を含まない l 値は
        動きようがないので調べない *)
    (* copy / uncopy の両辺が同じ変数でないこと。ロケーションで比べるので、
       別の変数が同じオブジェクトを指す（uncopy の正当な使い方）は通る *)
    let check_not_same_var o1 o2 =
      let l1, _ = lval_val_in st o1 env in
      let l2, _ = lval_val_in st o2 env in
      if l1 = l2 then
        fail_stm (pretty_stms [stm] 0 ^
                    "\nERROR:copy and uncopy need two different variables; \
                     uncopy of a variable with itself would erase its value")
    in
    let check_locs_stable st' targets =
      List.iter
        (fun (y, lv) ->
          if obj_has_index y then
            let lv', _ = lval_val_in st' y env in
            if lv' <> lv then
              fail_stm (pretty_stms [stm] 0 ^
                          "\nERROR:The array index of this statement must not be \
                           changed by the statement itself"))
        targets
    in
    (* call処理の共通部分を実行する関数．invertFlagが1なら逆実行 *)
    let mycall locs locs2 invertFlag =
      match stm with
      | LocalCall (mid0, args) | LocalUncall(mid0, args) | ObjectCall(_, mid0, args) | ObjectUncall(_, mid0, args) ->
         let vl = List.map (fun x -> argv x env st) args in     (* v_i = arg(a_i, γ, μ) (実引数の値を求める) *)         
         let id, envf = match lookup_st locs2 st with
           | ObjVal(id, envf) -> id, envf
           | _ -> fail_stm (pretty_stms [stm] 0 ^ "\nERROR:expected object value for method call") in
         let _f, meth = try lookup_map id map with                   (* Γ(c) = (field, method) *)
           | Failure str -> failwith ((pretty_stms [stm] 0) ^ "\n" ^ str ^ " in this statement") in
         let MDecl(_mid, para, mstml) = lookup_meth mid0 vl meth in  (* メソッド名がmidのメソッドを求める(q) *)
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
           | Failure str -> fail_stm (pretty_stms [stm] 0 ^ "\n" ^ str)
         end
      | _ -> failwith "not implemented"
    in
    match stm with
    (* 位置情報は診断専用。通常は update_with_stm が剥がしてから渡すので
       ここには来ないが、念のため素通しする *)
    | Positioned(_, s0) -> update st s0
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
       let v' = try bin_op (f op) vx v with
                | Failure _e -> fail_stm (pretty_stms [stm] 0 ^ "\nERROR:Integer value expected in this statement")
       in
       let st2 = ext_st st lvx v' (* the right value of x *) in
       (* 可逆性の副条件。整数変数への代入は構文的に（coq/roopl.v の E_assign の
          x ∉ fv(e)）検査する。 *)
       (match y with
        | VarArray(x, None) ->
           if List.mem x (free_vars e) then
             fail_stm (pretty_stms [stm] 0 ^ "\nERROR:Variable " ^ x ^
                         " must not occur on both sides of this assignment")
        | _ -> ());
       (* 右辺が自分の書き込みで変わらないこと（E_fassign / E_aassign の
          eval e b = eval e a）。フィールドと配列要素はこれが本来の副条件で、
          別名を構文で近似せずに済む。

          **整数変数にもこれを掛ける。** 名前が違っても同じ場所を指しうるため:
          call q(x, x) のように同じ変数を 2 つの仮引数へ参照渡しすると、本体の
          a += b は名前の上では別物なのに実質 x += x になる。形式側は
          bind_args が仮引数を実引数の名前へ置き換えるので構文的な条件で
          弾けるが、実装は名前ではなくロケーションで束ねるので、値で見るしかない *)
       if eval_exp e env st2 <> v then
         fail_stm (pretty_stms [stm] 0 ^
                     "\nERROR:The right-hand side of this assignment must not be \
                      changed by the assignment itself");
       check_locs_stable st2 [ (y, lvx) ];
       st2
    (*SWPVAR*) (*SWAPARRVAR*)
    | Swap(y1, y2) (*y1 <=> y2*)->
       let lv1, v1 = lval_val y1 env in
       let lv2, v2 = lval_val y2 env in
       let st2 = ext_st st lv1 v2 in (*update y2 -> y1*)
       let st3 = ext_st st2 lv2 v1 (*update y1 -> y2*) in
       check_locs_stable st3 [ (y1, lv1); (y2, lv2) ];
       st3
    | Loop(e1, stml1, stml2, e2) ->                          (* from e1 do s1 loop s2 until e2 *)
       let rec eval_loop (e1, stml1, stml2, e2) env map st = (* 意味関数L *)
         (*LOOPREC*)
         if isFalse (eval_exp e2 env st) then           (* ?e2 = 0(false) *)
           let st2 = eval_state stml2 env map st in          (* s2実行 *)
           if isFalse (eval_exp e1 env st2) then              (* アサーション ?e1 = 0(false) *)
             let st3 = eval_state stml1 env map st2 in       (* 満たす場合、s1実行 *)
             eval_loop (e1, stml1, stml2, e2) env map st3    (* 意味関数L繰り返し *)
           else
             fail_stm (pretty_stms [stm] 0 ^ "\nERROR:Assertion should be false in this statement")             
         (*LOOPBASE*)
         else
           (if isFalse (eval_exp e2 env st) then fail_stm (pretty_stms [stm] 0 ^ "\nERROR:assertion is incorrect in this statement") else st)
       in
       (* LOOPMAIN *)
       if isTrue (eval_exp e1 env st) then                   (* アサーション ?e1 != 0(true) *) 
         let st2 = eval_state stml1 env map st in            (* s1実行 *)
         eval_loop (e1, stml1, stml2, e2) env map st2        (* 意味関数Lへ *)
       else
         fail_stm (pretty_stms [stm] 0 ^ "\nERROR:Assertion should be true in this statement")
    (*FOR CONST: for x in (e1..e2) do stml end *)
    (* 範囲は入口で一度だけ評価し、体がループ変数 x と範囲式 e1/e2 を変えない
       ことを毎周検査する。この 2 つが可逆性の前提で（逆は for x in (e2..e1)
       do 逆体 end）、どちらかが崩れると逆向きの実行が同じ道をたどらない。
       形式化は coq/roopl.v の for_up / for_down（局所ブロック＋二重ガードの
       ループへの糖衣）。 *)
    | For(x, e1, e2, stml) ->
       (* for は局所ブロック＋二重ガードのループの糖衣（coq/roopl.v の for_up）
          なので、E_local の x ∉ fv(e1), x ∉ fv(e2) がそのまま範囲式にかかる。
          範囲がループ変数を指すと出口の表明が恒真になる *)
       if List.mem x (free_vars e1) || List.mem x (free_vars e2) then
         fail_stm (pretty_stms [stm] 0 ^
                     "\nERROR:The range of this for statement must not mention \
                      the loop variable " ^ x);
       let int_of e st' = match eval_exp e env st' with
         | IntVal(n) -> n
         | _ -> failwith "ERROR:for range must be integer"
       in
       let n1 = int_of e1 st in                                   (* 範囲の始点 *)
       let n2 = int_of e2 st in                                   (* 範囲の終点 *)
       let d = if n1 < n2 then 1 else -1 in                        (* 進む向き *)
       let locs = max_locs st + 1 in                            (* 未使用のロケーションを取得 *)
       let env2 = ext_envs env x locs in                          (* γ[x->l] *)
       let rec for_con i st =
         let st2 = ext_st st locs (IntVal i) in                    (* μ[l->i] *)
         let st3 = eval_state stml env2 map st2 in                 (* stml1回実行 *)
         (* 体はループ変数を変えてはならない（毎周検査する） *)
         if (lookup_val x env2 st2) <> (lookup_val x env2 st3) then
           fail_stm (pretty_stms [stm] 0 ^ "\nERROR:Variable "^ x ^ " must not change in this for statement")
         (* 体は範囲の値も変えてはならない *)
         else if int_of e1 st3 <> n1 || int_of e2 st3 <> n2 then
           fail_stm (pretty_stms [stm] 0 ^ "\nERROR:The range of this for statement must not change in its body")
         else if i = n2 then List.remove_assoc locs st3            (* ストアからロケーションxを取り除く *)
         else for_con (i + d) st3                                  (* 再帰 *)
       in
       for_con n1 st
    (*追加部分SWITCH*)
    | Switch(obj1, cases, stml, obj2) ->
       let rec eval_cases obj1 cs s obj2 env map st =
         match cs with
         | [] -> eval_state s env map st
         | (((_c1, q1), s1, (p1, q'1, _b1))::tl) ->
         let  isMatch obj q env st =
           if List.length q = 0 then false
           else
             let locs, _ = lval_val obj env in
             let v = lookup_st locs st in
               List.exists (fun x -> let n = eval_exp x env st in n = v) q
         in
         let rec search_break = function
           | ((_, _), s, (_, q', b))::tl ->
              (s,q') :: if b = Break then [] else search_break tl
           | [] -> failwith "ERROR:no matching case found in switch statement"
         in
         let rec eval_case1 sq obj2 length env map st =
           match sq with
           | [] -> failwith "ERROR:empty case list in switch statement"
           | [(s,q)] -> let st2 = eval_state s env map st in
                        let locs, _ = lval_val obj2 env in
                        let v = lookup_st locs st2 in
                        if length > 0 && length - 1 < List.length q then
                          (if v = eval_exp (List.nth q (length - 1)) env st then
                            st2
                          else fail_stm (pretty_stms [stm] 0 ^ "\nERROR:assertion is incorrect:should be " ^ pretty_exp (List.nth q (length - 1)) ^ " in this switch statement"))
                        else fail_stm (pretty_stms [stm] 0 ^ "\nERROR:assertion index out of bounds in switch statement")
           | (s,_) :: tl -> let st2 = eval_state s env map st in
                            eval_case1 tl obj2 length env map st2
         in
         let rec eval_case2 obj1 q1 sq obj2 n env map st =
           let rec countMatch obj1 q n env st =
             let _, v = lval_val obj1 env in
             match q with
             | [] -> failwith "ERROR:no matching value found in switch case expression list"
             | e::tl -> if v = eval_exp e env st then n
                        else countMatch obj1 tl (n + 1) env st
           in
           let count = countMatch obj1 q1 1 env st in
           match sq with
           | [] -> failwith "ERROR:no matching case found in switch case evaluation"
           | (_s,[])::_ -> failwith "ERROR:empty expression list in switch case"
           | (s,(e::_tl0))::tl ->
              if count = n then
                let st2 = eval_state s env map st in
                let locs, _ = lval_val obj2 env in
                let v = lookup_st locs st2 in
                if v = eval_exp e env st then st2
                else fail_stm (pretty_stms [stm] 0 ^ "\nERROR:assertion is incorrect:should be " ^ pretty_exp e ^ " in this switch statement")
              else
                let st2 = eval_state s env map st in
                eval_case2 obj1 q1 tl obj2 (n + 1) env map st2
         in
         if List.length q1 = 1 && List.length q'1 = 1 && isMatch obj1 q1 env st then
           let st2 = eval_state s1 env map st in
           if isMatch obj2 q'1 env st2 then st2
           else fail_stm (pretty_stms [stm] 0 ^ "\nERROR:assertion is incorrect:should be " ^ pretty_exp (List.hd q'1)  ^ " in this switch statement")
         else if ((List.length q1 = 1 && p1 = NoEsac) || (List.length q'1 >= 2)) && isMatch obj1 q1 env st then
           let sq = search_break cs in
           eval_case1 sq obj2 (List.length sq) env map st
         else if List.length q1 >= 2 && isMatch obj1 q1 env st then
           let sq = search_break cs in
           eval_case2 obj1 q1 sq obj2 1 env map st
         else if List.length cs = 1 && not (isMatch obj1 q1 env st) then
           eval_state s env map st
         else
           let st2 = eval_cases obj1 tl s obj2 env map st in
           (* 通らなかった枝の出口表明は偽でなければならない。さもないと出口の
              値が枝を識別できず、逆向きの実行が枝を選び直せない（形式化では
              coq/roopl.v の rev_switch が入れ子の条件分岐の else 側として
              E_if_f でこれを検査している）。 *)
           if List.length q'1 = 1 && not (isMatch obj1 q1 env st)
              && isMatch obj2 q'1 env st2 then
             fail_stm (pretty_stms [stm] 0 ^ "\nERROR:assertion is incorrect:the exit value " ^ pretty_exp (List.hd q'1) ^ " of a case that was not taken must not match in this switch statement")
           else st2
       in
       eval_cases obj1 cases stml obj2 env map st
    | Conditional(e1, stml1, stml2, e2) ->           (* if e1 then s1 else s2 fi e2 *)
       (*IFTRUE*)
       if isTrue (eval_exp e1 env st) then     (* ?e1 != 0(true)  *)
         let st2 = eval_state stml1 env map st in    (* s1実行 *)
         if isTrue (eval_exp e2 env st2) then  (* アサーション ?e2 != 0(true) *)
           st2
         else                                        (* アサーションを満たさない場合のエラー *)
           fail_stm (pretty_stms [stm] 0 ^ "\nERROR:Assertion should be true in this statement")           
       (*IFFALSE*)
       else if isFalse (eval_exp e1 env st) then (* ?e1 = 0(false) *)
         let st2 = eval_state stml2 env map st in    (* s2実行 *)
         if isFalse (eval_exp e2 env st2) then   (* アサーション ?e2 = 0(false) *)
           st2
         else                                        (* アサーションを満たさない場合のエラー *)
           fail_stm (pretty_stms [stm] 0 ^ "\nERROR:Assertion should be false in this statement")
       else
         fail_stm (pretty_stms [stm] 0 ^ "\nERROR:Assertion should be false in this statement")
    (*LocalCALL*)
    | LocalCall(_mid, _args) (* call q(y1,...,yn) *)->
       let locs = lookup_envs "this" env in                   (* γ(this) = l *)
       let locs2 = match lookup_st locs st with LocsVal(l) -> l | _ -> failwith "ERROR:The receiver of this call is nil or not an object" in
       mycall locs locs2 0
    (*LocalUNCALL*)
    | LocalUncall(_mid, _args) (* uncall q(y1,...,yn) *)->
       let locs = lookup_envs "this" env in                   (* γ(this) = l *)
       let locs2 = match lookup_st locs st with LocsVal(l) -> l | _ -> failwith "ERROR:The receiver of this call is nil or not an object" in
       mycall locs locs2 1
    (*CALLOBJ*)
    | ObjectCall(obj, _mid, _args) (* call x0::q(a1,...,an) *)->
       let locs, v = lval_val obj env in
       let locs2 = match v with LocsVal(l) -> l | _ -> failwith "ERROR:The receiver of this call is nil or not an object" in
       mycall locs locs2 0
    (*UNCALLOBJ*)
    | ObjectUncall(obj, _mid, _args) (* uncall x0::q(a1,...,an) *)->
       let locs, v = lval_val obj env in
       let locs2 = match v with LocsVal(l) -> l | _ -> failwith "ERROR:The receiver of this call is nil or not an object" in
       mycall locs locs2 1
    (*OBJBLOCK*)
    | ObjectBlock(tid, id, stml) (* construct c x  s destruct x *)->
       let (fl, _ml) = try lookup_map tid map with           (* Γ(c)=(f1,...,fn, medhods) *)
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
         fail_stm (pretty_stms [stm] 0 ^ "\nERROR:" ^ id ^ "'s instance field is not zero-cleared in this statement")
    (*OBJNEW*)
    | ObjectConstruction(tid, obj) (* new c y *)->
       let (fl, _ml) = try lookup_map tid map with          (* Γ(c)=(f1,...,fn, methods) *)
         (* クラスが見つからない場合エラー *)
         | Failure str -> fail_stm (pretty_stms [stm] 0 ^ "\n" ^ str ^ " in this statement") in
       let locs, v = lval_val obj env in                   (* l=γ(y) *)
       if v <> IntVal(0) then                              (* yがnilか確認 *)
         fail_stm (pretty_stms [stm] 0 ^ "\nERROR: variable is not nil in this statement")
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
       (*ベクトルの要素を削除するための関数：locsからnまでのロケーションを削除*)
       let rec delete_st st locs n =
         if n <> 0 then delete_st (List.remove_assoc locs st) (locs + 1) (n - 1)
         else st in
       let (fl, _) = try lookup_map tid map with
         | Failure str -> fail_stm (pretty_stms [stm] 0 ^ "\n" ^ str ^ " in this statement") in
       let locs, _ = lval_val obj env in                    (* l=γ(y) *)
       let locs0 = match lookup_st locs st with LocsVal(l) -> l | _ -> failwith "ERROR:delete needs an allocated object, but the variable is nil or not an object" in
       let acls, envf = match lookup_st locs0 st with
         | ObjVal(c, e) -> c, e
         | _ -> failwith "ERROR:delete needs an allocated object, but the variable does not refer to one" in
       (* 解放するクラスが実際のクラスと一致すること（coq/roopl.v の E_delete /
          E_obj の hc a l = cl）。これが無いと、フィールド数の違うクラス名で
          delete したときに消すロケーション数がずれてストアが壊れる *)
       if acls <> tid then
         fail_stm (pretty_stms [stm] 0 ^ "\nERROR:This deletes a " ^ acls ^
                     " as if it were a " ^ tid ^ "; the class must match in this statement");
       let locs1 = if List.length envf = 0 then 0
                   else List.hd (List.map snd envf) in      (* locs1=l1 *)
       if is_field_zero st locs1 (List.length fl) then      (* インスタンスフィールドがゼロクリアされているか確認 *)
         let st2 = delete_st st locs1 (List.length fl) in   (* ストアからロケーションl1,...,lnを削除 *)
         let st3 = List.remove_assoc locs0 st2 in           (* ストアからロケーションl0を削除 *) 
         ext_st st3 locs (IntVal 0)                         (* lの値をゼロクリア *)
       else
         fail_stm (pretty_stms [stm] 0 ^ "\nERROR:All instance field is not zero-cleared in this statement")
    (*ARRNEW*)
    | ArrayConstruction((_tid, e), obj) ->                                      (* new a[e] x *)
       let locs, v = lval_val obj env in                                       (* xのロケーションを求める *)
       if v <> IntVal(0) then                                                  (* xがnilか確認 *)
         fail_stm (pretty_stms [stm] 0 ^ "\nERROR:Variable is not nil in this statement")
       else
       let n = match eval_exp e env st with IntVal(n) -> n | _ -> failwith "ERROR:array size must be integer" in
       if n < 1 then
         fail_stm (pretty_stms [stm] 0 ^ "\nERROR:Array size must be at least 1, but it is "
                   ^ string_of_int n ^ " in this statement");
       let st2 = ext_st st locs (LocsVec(gen_locsvec n (max_locs st + 1))) in (* ベクトルを生成({l'1,...,l'n}しストアに格納 *)
       ext_st_zero st2 (max_locs st2 + 1)  n                                  (* ストア拡張 μ[l'1->0,...,l'n->0 *)
    (*ARRDELETE*)
    | ArrayDestruction((_tid, e), obj) ->           (* delete a[e] x *)
       let n = match eval_exp e env st with IntVal(n) -> n | _ -> failwith "ERROR:array size must be integer" in
       if n < 1 then
         fail_stm (pretty_stms [stm] 0 ^ "\nERROR:Array size must be at least 1, but it is "
                   ^ string_of_int n ^ " in this statement");
       let veclocs,_ = lval_val obj env in         (* l=γ(x) *)
       let vec = match lookup_st veclocs st with LocsVec(v) -> v | _ -> failwith "ERROR:expected array value for deletion" in
       let locs = lookup_vec 0 vec in              (* locs = l'1 *)
       if is_field_zero st locs n                  (* 配列要素すべてがゼロクリアされているか確認 *)
       then
       let st2 = List.fold_left (fun x y -> List.remove_assoc y x) st vec in              (* l'1からl'nのロケーションを削除 *)
       ext_st st2 veclocs (IntVal 0)               (* xのロケーションをゼロに初期化 *)
       else
         fail_stm (pretty_stms [stm] 0 ^ "\nERROR:All array elements is not zero-cleared in this statement")
    (*COPY*)
    | CopyReference(_dt, obj1, obj2) ->      (* copy c x x' *)
       (* 同じ変数どうしは禁止（coq/roopl.v の E_copy / E_uncopy の x ≠ y）。
          uncopy x x は値を消してしまい逆向きに戻せない。別の変数が同じ
          オブジェクトを指すのは uncopy の正当な使い方なので、値ではなく
          **ロケーション**で比べる *)
       check_not_same_var obj1 obj2;
       let locsx',v = lval_val obj2 env in  (* v=μ(γ(x)) *)
       if v <> IntVal(0) then               (* x'がnilか確認 *)
         fail_stm (pretty_stms [stm] 0 ^ "\nERROR:variable of right is not nil in this statement")
       else
       let _, vx = lval_val obj1 env in     (* l'=γ(x') *)
       ext_st st locsx' vx                  (* ストア更新μ[l'->v] *)
    (*UNCOPY*)
    | UncopyReference(_dt, obj1, obj2) -> (* uncopy c x x' *)
       check_not_same_var obj1 obj2;
       let _, v1 = lval_val obj1 env in (* 変数xの値を求める *)
       let locs, v2 = lval_val obj2 env in (* 変数x'の値を求める *)
       if v1 = v2 then                   (* 同じ領域を指しているか確認 *)
         ext_st st locs (IntVal 0)       (* 変数x'をゼロクリア *)
       else
         fail_stm (pretty_stms [stm] 0 ^ "\nERROR:both variable's reference is not same in this statement")
    (*LOCALBLOCK*)
    | LocalBlock(_dt, id, e1, stml, e2) -> (* local c x = e1  s  delocal x = e2 *)
       (* 入口・出口の式が自分自身を参照すると表明が恒真になり、逆向きの実行が
          値を復元できない（coq/roopl.v の E_local の x ∉ fv(e1), x ∉ fv(e2)） *)
       if List.mem id (free_vars e1) || List.mem id (free_vars e2) then
         fail_stm (pretty_stms [stm] 0 ^ "\nERROR:Local variable " ^ id ^
                     " must not occur in its own local/delocal expression");
       let v1 = eval_exp e1 env st in     (* e1の値を求める *)
       let locs = max_locs st + 1 in      (* 未使用のロケーションを求める *)
       let env2 = ext_envs env id locs in (* 環境に変数xを追加 *)
       let st2 = ext_st st locs v1 in     (* ストアに変数xを追加 *)
       let st3 = eval_state stml env2 map st2 in (* sを実行 *)
       let v2 = eval_exp e2 env2 st3 in          (* e2の値を求める *)
       if lookup_st locs st3 = v2 then    (* x = e2 を満たすか確認 *)
         List.remove_assoc locs st3       (* xをストアから取り除く *)
       else
         fail_stm (pretty_stms [stm] 0 ^
                     "\nERROR: Variable " ^ id ^ " = " ^ Print.show_val (lookup_st locs st3) ^ ", But it should be " ^ Print.show_val v2 ^ " in this statement")
  in
  (* 文脈のない失敗（式の評価など）には、その文の pretty 表示を足してから
     外へ投げ直す。すでに文が付いているもの（Util.Runtime_error）は
     そのまま通すので、入れ子の文が何重にも積まれることはない。 *)
  let update_with_stm st stm0 =
    (* 位置情報は診断専用なので、意味論に渡す前に剥がす *)
    let stm = strip_pos stm0 in
    let at = match pos_of stm0 with
      | Some p -> Diagnostics.at_line p.line
      | None -> ""
    in
    try update st stm with
    | Failure e ->
       raise (Util.Runtime_error
                (pretty_stms [stm] 0 ^ "\n" ^ e
                 ^ Diagnostics.where_line stm env st ^ at))
    (* 式の中で落ちた場合は、文ではなく式の範囲を位置として使う *)
    | Util.Expr_error (sp, e) ->
       raise (Util.Runtime_error
                (pretty_stms [stm] 0 ^ "\n" ^ e
                 ^ Diagnostics.where_line stm env st ^ Diagnostics.at_span sp))
    (* fail_stm で投げられたものは文は付いているが変数の値はまだない。
       いちばん内側のラッパ（＝その文自身）だけが値と位置を付ける。 *)
    | Util.Runtime_error e when not (Diagnostics.has_where e) ->
       raise (Util.Runtime_error (e ^ Diagnostics.where_line stm env st ^ at))
  in
  List.fold_left update_with_stm st0 stml

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
    | [] -> failwith "ERROR:empty environment in gen_st (no main class fields found)"
    | [_f] -> [(n, LocsVal (n + 1)); (n + 1, objval)]
    | _f :: tl -> ext_st (gen_st2 tl objval (n + 1)) n (IntVal(0))
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
let rec map_field clist1 (CDecl(_id, opt, fl, _m)) =
  match opt with
  | None -> fl
  | Some(cid) ->
     let parent_class = lookup_class_map clist1 cid in (*a^-1(c')*)
     let parent_method = map_field clist1 parent_class in
     parent_method @ fl

(**gen_mapで使用する関数 ROOPL++26ページの関数methodに相当*)
let rec map_method clist1 cl =
  (*メソッドのリストに指定した名前のメソッド名があるか調べる関数*)
  let rec lookup_methid id = function
    | [] -> false
    | MDecl(mid, _, _) :: tl -> id = mid || lookup_methid id tl
  in
  (*サブクラスに親クラスと同じ名前のメソッドがある場合親クラスからそのメソッドを削除し、サブクラスの同じ名前のメソッドを追加する関数(オーバーライド)*)
  let method_union subm parem =
    (*親クラスがサブクラスと同じ名前のメソッドをもつ場合そのメソッドを削除する関数*)
    let rec remove_method subm parem =
      match parem with
      | [] -> []
      | MDecl(mid, dl, stml) :: tl ->
         if (lookup_methid mid subm)
         then remove_method subm tl
         else MDecl(mid, dl, stml) :: remove_method subm tl
    in
    remove_method subm parem @ subm
  in
  match cl with
  | CDecl(_, None, _, m) -> m
  | CDecl(_id, Some(cid), _fl, m) ->
     let parent_class = lookup_class_map clist1 cid in (*a^-1(c')*)
     let parent_method = map_method clist1 parent_class in
     method_union m parent_method

(**マップを生成する関数*)
let gen_map clist =
  let rec gen_map2 clist1 clist2 =
    (*クラスからidを取り出す関数 gen_mapで使用*)
    let lookup_cid (CDecl(id, _, _, _)) = id in
    match clist2 with
    | [] -> []
    | cl :: tl ->
       (lookup_cid cl, (map_field clist1 cl, map_method clist1 cl)) :: gen_map2 clist1 tl
  in
  gen_map2 clist clist

  (**マップのリストから指定されたメソッド名を含んでいるクラス名とそのメソッドの文のタプルを返す*)
let rec lookup_class id1 map =
  let rec lookup_class_2 id2 = function
    | MDecl(mid, _paral, stml) :: tl2 ->
       if mid = id2 then Some(stml)
       else lookup_class_2 id2 tl2
    | [] -> None
  in
  match map with
  | [] -> failwith ("ERROR:class " ^ id1 ^ " was not found")
  | (cid, (_fl, ml)) :: tl1 ->
     match lookup_class_2 id1 ml with
     | None -> lookup_class id1 tl1
     | Some(stm) -> (cid, stm)

(**プログラムを実行し、(結果, 最終ストア) を返す関数。
   最終ストアは終了時のゼロクリア検査（diagnostics.ml）で参照する。
   結果だけでよい場合は eval_prog を使う。 *)
let eval_prog_state ?(library0 = Prog []) (Prog(cl)) =
  let Prog(cl2) = library0 in
  (*マップ生成*)
  let map0 = gen_map cl2 in
  (*標準ライブラリ読み込み*)
  let map = map0 @ (gen_map cl) in
  (*mainメソッドを含んでいるクラスidとメソッドの文を取得*)
  let mid, mainstml = lookup_class "main" map in
  (*mainメソッドを含んでいるクラスのフィールドを取得*)
  let field, _ = lookup_map mid map in
  (*フィールドを識別子のみのリストに変換*)
  let fid = List.map (fun (Decl(_, id)) -> id) field in
  (*フィールドから環境を生成*)
  let env = gen_env fid in
  (*環境からストアを生成*)
  let st = gen_st env (ObjVal(mid, env)) in
  (*mainメソッドの処理を実行*)
  let st2 = eval_state mainstml env map st in
  (*結果を生成(mainメソッドを含んでいるクラスのフィールドとそれに対応する値の組を返す)*)
  (List.remove_assoc "this" (gen_result env st2), st2)

(**プログラムを実行する関数（クラスリストを受け取り、結果のリストを返す）*)
let eval_prog ?(library0 = Prog []) prog = fst (eval_prog_state ~library0 prog)
