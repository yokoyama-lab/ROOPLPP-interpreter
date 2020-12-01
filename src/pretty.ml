(**プリティプリンタ：構文木を文字列に変換する*)
open Syntax

(**n回分半角4文字字下げする関数*)
let rec indent n = if n = 0 then ""
                   else indent (n - 1) ^ "    "

(**型をプリントする関数*)
let pretty_dataType = function
  | IntegerType -> "int"
  | ObjectType typeId -> typeId
  | CopyType typeId -> typeId
  | ObjectArrayType typeId -> typeId ^ "[]"
  | IntegerArrayType -> "int[]"
  | ArrayType -> ""
  | ArrayElementType -> ""
  | NilType -> ""

(**演算子をプリントする関数*)
let pretty_binOp = function
  | Add -> "+"
  | Sub -> "-"
  | Xor -> "^"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Band -> "&"
  | Bor -> "|"
  | And -> "&&"
  | Or -> "||"
  | Lt -> "<"
  | Gt -> ">"
  | Eq -> "="
  | Ne -> "!="
  | Le -> "<="
  | Ge -> ">="

(**代入演算子をプリントする関数*)
let pretty_modOp = function
  | ModAdd -> "+="
  | ModSub -> "-="
  | ModXor -> "^="

(**式をプリントする関数*)
let rec pretty_exp = function
  | Const n -> string_of_int n
  | Var id -> id
  | ArrayElement(id, exp) -> id ^ "[" ^ pretty_exp exp ^ "]"
  | Nil -> "nil"
  | Binary(binOp, exp1, exp2) -> pretty_exp exp1 ^ " " ^ pretty_binOp binOp ^ " " ^ pretty_exp exp2
  | Dot(exp1, exp2) -> pretty_exp exp1 ^ "." ^ pretty_exp exp2

(**変数、配列、ドット演算子をプリントする関数*)
let rec pretty_obj = function
  | VarArray(id, None) -> id
  | VarArray(id, Some exp) -> id ^ "[" ^ pretty_exp exp ^ "]"
  | InstVar(obj1, obj2) ->  pretty_obj obj1 ^ "." ^ pretty_obj obj2

(**メソッド呼び出しの引数をプリントする関数*)
let rec pretty_actArgs =
  let pretty_actArg = function
    | Id(id) -> id
    | Exp(e) -> pretty_exp e
  in
  function
  | [] -> ""
  | [arg] -> pretty_actArg arg
  | hd :: tl -> pretty_actArg hd ^ ", " ^ pretty_actArgs tl

(**文をプリントする関数：nを受け取り、n回分字下げする*)
let rec pretty_stms stms n =
  match stms with
  | [] -> ""
  | hd :: tl -> indent n ^ pretty_stm hd n ^ "\n" ^ pretty_stms tl n
  and
pretty_stm stm n =
  let s =
    match stm with
    | Assign(obj, modOp, exp) -> pretty_obj obj ^ " " ^ pretty_modOp modOp ^ " " ^ pretty_exp exp
    | Swap(obj1, obj2) -> pretty_obj obj1 ^ " <=> " ^ pretty_obj obj2
    | Conditional(exp1, stm1, stm2, exp2) -> "if " ^ pretty_exp exp1 ^ " then\n" ^ pretty_stms stm1 (n + 1) ^ indent n ^ "else\n" ^ pretty_stms stm2 (n + 1) ^ indent n ^ "fi " ^ pretty_exp exp2
    | Loop(exp1, stm1, stm2, exp2) -> "from " ^ pretty_exp exp1 ^ " do\n" ^ pretty_stms stm1 (n + 1) ^ indent n ^  "loop\n" ^ pretty_stms stm2 (n + 1) ^ indent n ^ "until " ^ pretty_exp exp2
    (* for追加部分 *)
    | For(id, e1, e2, stms) -> "for " ^ id ^ " in " ^  "(" ^ pretty_exp e1 ^ ".." ^ pretty_exp e2 ^ ")" ^ " do\n" ^ pretty_stms stms (n + 1) ^ indent n ^  "end"
      (* switch追加部分 *)
    | Switch(obj1, cases, stml, obj2) ->
       let rec pretty_cases =
         let pretty_case = function | Case -> "case" | Fcase -> "fcase" | Ecase -> "ecase" in
         let pretty_break = function | Break -> " break " | NoBreak -> "" in
         function
         | [] -> ""
         | (c, e1, s, e2, b) :: tl -> pretty_case c ^ " " ^ pretty_exp e1 ^ ":\n" ^ pretty_stms s (n + 1) ^ indent (n + 1) ^ "esac " ^ pretty_exp e2 ^ "\n" ^ indent (n + 1) ^ pretty_break b ^ "\n" ^ indent n ^ pretty_cases tl
       in
       "switch " ^ pretty_obj obj1 ^ "\n" ^ indent n ^ pretty_cases cases ^ "default:\n" ^ pretty_stms stml (n + 1) ^ indent (n + 1) ^ "break\n" ^ indent n ^ "hctiws " ^ pretty_obj obj2
    | ObjectBlock(typeId, id, stm) -> "construct " ^ typeId ^ " " ^ id ^ "\n" ^ pretty_stms stm (n + 1) ^ "\n" ^ indent n ^ "destruct " ^ id
    | LocalBlock(dataType, id, exp1, stm, exp2) -> "local " ^ pretty_dataType dataType ^ " " ^ id ^ " = " ^ pretty_exp exp1 ^ "\n" ^ pretty_stms stm n  ^ indent n ^ "delocal " ^ pretty_dataType dataType ^ " " ^ id ^ " = " ^ pretty_exp exp2
    | LocalCall(methodId, ids) -> "call " ^ methodId ^ "(" ^ pretty_actArgs ids ^ ")"
    | LocalUncall(methodId, ids) -> "uncall " ^ methodId ^ "(" ^ pretty_actArgs ids ^ ")"
    | ObjectCall(obj, methodId, ids) -> "call " ^ pretty_obj obj ^ "::" ^ methodId ^ "(" ^ pretty_actArgs ids ^ ")"
    | ObjectUncall(obj, methodId, ids) -> "uncall " ^ pretty_obj obj ^ "::" ^ methodId ^ "(" ^ pretty_actArgs ids ^ ")"
    | ObjectConstruction(typeId, obj) -> "new " ^ typeId ^ " " ^ pretty_obj obj
    | ObjectDestruction(typeId, obj) -> "delete " ^ typeId ^ " " ^ pretty_obj obj
    | CopyReference(dataType, obj1, obj2) -> "copy " ^ pretty_dataType dataType ^ " " ^ pretty_obj obj1 ^ " " ^ pretty_obj obj2
    | UncopyReference(dataType, obj1, obj2) -> "uncopy " ^ pretty_dataType dataType ^ " " ^ pretty_obj obj1 ^ " " ^ pretty_obj obj2
    | ArrayConstruction((typeId, exp), obj) -> "new " ^ typeId ^ "[" ^ pretty_exp exp ^ "] " ^ pretty_obj obj
    | ArrayDestruction((typeId, exp), obj) -> "delete " ^ typeId ^ "[" ^ pretty_exp exp ^ "] " ^ pretty_obj obj
    | Skip -> "skip"
    | Show(exp) -> "show" ^ "(" ^ pretty_exp exp ^ ")"
    | Print(str) -> "print" ^ "(\""  ^ String.escaped str ^ "\")"
    in
    s

(**フィールドまたは引数をプリントする関数*)
let pretty_decl (Decl(dataType, id)) = pretty_dataType dataType ^ " " ^ id

(**フィールドをプリントする関数(リストを受け取る)*)
let rec pretty_fields = function
  | [] -> ""
  | hd :: tl -> pretty_decl hd ^ "\n    " ^ pretty_fields tl

(**引数をプリントする関数*)
let rec pretty_args = function
  | [] -> ""
  | [arg] -> pretty_decl arg
  | hd :: tl -> pretty_decl hd ^ ", " ^ pretty_args tl

(**メソッドをプリントする関数*)
let pretty_method (MDecl(id, args, stms)) =
  "method " ^ id ^ "(" ^ pretty_args args ^ ")\n" ^ pretty_stms stms 2
(**メソッドをプリントする関数(リストを受け取る)*)
let rec pretty_methods = function
  | [m] -> pretty_method m
  | hd :: tl -> pretty_method hd ^ "\n    " ^ pretty_methods tl
  | _ -> failwith "not implemented"

(**クラスをプリントする関数*)
let pretty_c (CDecl(c, inherits, fields, methods)) =
  let inher =
  match inherits with
  | None -> ""
  | Some(id) -> " inherits " ^ id
  in
  "class " ^ c ^ inher  ^ "\n    " ^ pretty_fields fields ^ "\n    " ^ pretty_methods methods

(**クラスをプリントする関数(リストを受け取る)*)
let rec pretty_cl = function
  | [cl] -> pretty_c cl
  | hd :: tl -> pretty_c hd ^ "\n" ^ pretty_cl tl
  | _ -> failwith "not implemented"

(**プログラムをプリントする関数*)
let pretty_prog (Prog(cl)) = print_string(pretty_cl cl ^ "\n")
