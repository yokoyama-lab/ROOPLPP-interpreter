(**データ型の定義*)
type id = string
type typeId = id
type methodId = id

(**型*)
type dataType =
  | IntegerType
  | ObjectType of typeId
  | CopyType of typeId
  | ObjectArrayType of typeId
  | IntegerArrayType
  | ArrayType
  | ArrayElementType
  | NilType

(** 演算子 *)
type binOp =
  | Add (**+*)
  | Sub (**-*)
  | Xor (**^*)
  | Mul (**/**)
  | Div (**/ *)
  | Mod (**%*)
  | Band (**&*)
  | Bor (**|*)
  | And (**&&*)
  | Or (**||*)
  | Lt (**<*)
  | Gt (**>*)
  | Eq (**=*)
  | Ne (**!=*)
  | Le (**<=*)
  | Ge (**>=*)

(**代入演算子*)
type modOp =
  | ModAdd (**+=*)
  | ModSub (**-=*)
  | ModXor (**^=*)

(**位置（診断用。意味論には影響しない）。文にも式にも付く。
   列は 0 起点で、[end_col] は最後の文字の次を指す（キャレットの幅に使う）*)
type pos = { line : int; col : int; end_line : int; end_col : int }

(**式*)
type exp =
  | Const of int (**整数*)
  | Var of id (**変数*)
  | ArrayElement of id * exp (**配列*)
  | Nil (**Nil*)
  | Binary of binOp * exp * exp (**二項演算*)
  | Dot of exp * exp (**ドット演算子*)
  | EPos of pos * exp (**位置情報つきの式（構文解析器が付ける）*)

(**変数*)
type obj =
  | VarArray of id * exp option
  | InstVar  of obj * obj

(**メソッド呼び出しの実引数*)
type arg =
  | Id of id (**変数*)
  | Exp of exp (**式*)

(**case文で使われる*)
type case =
  | Case
  | NoCase

(**case文で使われる*)
type esac =
  | Esac
  | NoEsac

(**case文で使われるbreak*)
type break =
  | Break
  | NoBreak (**breakなし*)

(**文*)
type stm =
  | Skip (**Skip*)
  | Assign of obj * modOp * exp (**x (+,-,^)= e*)
  | Swap of obj * obj (**y <=> y*)
  | Conditional of exp * stm list * stm list * exp (**if e then s else s fi e  or  if e then s fi e*)
  | Loop of exp * stm list * stm list * exp (**from e do s loop s until e or   or  from e do s until e  or  from e loop s until e*)
  | For of id * exp * exp * stm list  (**for x in (e..e) do s end*)(*追加部分for*)
  | Switch of obj * ((case * exp list) * stm list * (esac * exp list * break)) list * stm list * obj (**switch y case e s esac e break hctiws y*) (*追加部分switch*) 
  | ObjectBlock of typeId * id * stm list (**construct c x  s  destruct x*)
  | LocalBlock of dataType * id * exp * stm list * exp (**local t x = e s delocal t x = e*)
  | LocalCall of methodId * arg list (**call q(e, ... , e)*)
  | LocalUncall of methodId * arg list (**uncall q(e, ... , e)*)
  | ObjectCall of obj * methodId * arg list (**call x::q(e, ..., e)*)
  | ObjectUncall of obj * methodId * arg list (**uncall x::q(e, ..., e)*)
  | ObjectConstruction of typeId * obj (**new Foo foo*)
  | ObjectDestruction of typeId * obj (**delete Foo foo*)
  | CopyReference of dataType * obj * obj (**copy t x y*)
  | UncopyReference of dataType * obj * obj (**uncopy int x y*)
  | ArrayConstruction of (typeId * exp) * obj (**new Foo[length] fooList*)
  | ArrayDestruction of (typeId * exp) * obj (**delete Foo[length] fooList*)
  | Show of exp (**show(x)*)
  | Print of string (**print("")*)
  | Positioned of pos * stm (**位置情報つきの文（構文解析器が付ける）*)

(**式の位置情報の殻を剥がす*)
let rec strip_epos = function
  | EPos(_, e) -> strip_epos e
  | e -> e

(**いちばん外側の式の位置を取り出す*)
let epos_of = function
  | EPos(p, _) -> Some p
  | _ -> None

(**位置情報の殻を剥がす。意味論・整形・反転はすべて剥がした文を見る*)
let rec strip_pos = function
  | Positioned(_, s) -> strip_pos s
  | s -> s

(**いちばん外側の位置情報を取り出す*)
let pos_of = function
  | Positioned(p, _) -> Some p
  | _ -> None

(**メソッドの引数*)
type decl = Decl of dataType * id

(**メソッド*)
type mDecl = MDecl of methodId * decl list * stm list (**method q(...) s*)

(**クラス*)
type cDecl = CDecl of typeId * typeId option * decl list * mDecl list (**class C int x ... method q() ... *)

(**プログラム*)
type prog = Prog of cDecl list (**class .. class*)

(**位置情報をすべて取り除く。位置は同じプログラムでもソースの見た目で変わる
   ので、AST を構造として比べるときはこれを通す（整形→再パースの往復検査など）。 *)
let rec erase_pos_exp e =
  match e with
  | EPos(_, e0) -> erase_pos_exp e0
  | ArrayElement(x, e0) -> ArrayElement(x, erase_pos_exp e0)
  | Binary(o, e1, e2) -> Binary(o, erase_pos_exp e1, erase_pos_exp e2)
  | Dot(e1, e2) -> Dot(erase_pos_exp e1, erase_pos_exp e2)
  | e -> e

let erase_pos_obj o =
  let rec go = function
    | VarArray(x, ie) -> VarArray(x, Option.map erase_pos_exp ie)
    | InstVar(o1, o2) -> InstVar(go o1, go o2)
  in go o

let erase_pos_arg = function
  | Id x -> Id x
  | Exp e -> Exp (erase_pos_exp e)

let rec erase_pos_stm s =
  match s with
  | Positioned(_, s0) -> erase_pos_stm s0
  | Assign(o, op, e) -> Assign(erase_pos_obj o, op, erase_pos_exp e)
  | Swap(o1, o2) -> Swap(erase_pos_obj o1, erase_pos_obj o2)
  | Conditional(e1, s1, s2, e2) ->
     Conditional(erase_pos_exp e1, erase_pos_stms s1, erase_pos_stms s2,
                 erase_pos_exp e2)
  | Loop(e1, s1, s2, e2) ->
     Loop(erase_pos_exp e1, erase_pos_stms s1, erase_pos_stms s2, erase_pos_exp e2)
  | For(x, e1, e2, ss) ->
     For(x, erase_pos_exp e1, erase_pos_exp e2, erase_pos_stms ss)
  | Switch(o1, cases, ss, o2) ->
     Switch(erase_pos_obj o1,
            List.map (fun ((c, es1), ss1, (p, es2, b)) ->
                ((c, List.map erase_pos_exp es1), erase_pos_stms ss1,
                 (p, List.map erase_pos_exp es2, b))) cases,
            erase_pos_stms ss, erase_pos_obj o2)
  | ObjectBlock(t, x, ss) -> ObjectBlock(t, x, erase_pos_stms ss)
  | LocalBlock(t, x, e1, ss, e2) ->
     LocalBlock(t, x, erase_pos_exp e1, erase_pos_stms ss, erase_pos_exp e2)
  | LocalCall(m, args) -> LocalCall(m, List.map erase_pos_arg args)
  | LocalUncall(m, args) -> LocalUncall(m, List.map erase_pos_arg args)
  | ObjectCall(o, m, args) ->
     ObjectCall(erase_pos_obj o, m, List.map erase_pos_arg args)
  | ObjectUncall(o, m, args) ->
     ObjectUncall(erase_pos_obj o, m, List.map erase_pos_arg args)
  | ObjectConstruction(t, o) -> ObjectConstruction(t, erase_pos_obj o)
  | ObjectDestruction(t, o) -> ObjectDestruction(t, erase_pos_obj o)
  | CopyReference(d, o1, o2) -> CopyReference(d, erase_pos_obj o1, erase_pos_obj o2)
  | UncopyReference(d, o1, o2) ->
     UncopyReference(d, erase_pos_obj o1, erase_pos_obj o2)
  | ArrayConstruction((t, e), o) ->
     ArrayConstruction((t, erase_pos_exp e), erase_pos_obj o)
  | ArrayDestruction((t, e), o) ->
     ArrayDestruction((t, erase_pos_exp e), erase_pos_obj o)
  | Show e -> Show (erase_pos_exp e)
  | Skip -> Skip
  | Print str -> Print str

and erase_pos_stms l = List.map erase_pos_stm l

let erase_pos_prog (Prog cl) =
  Prog (List.map
          (fun (CDecl(c, sup, fl, ml)) ->
            CDecl(c, sup, fl,
                  List.map (fun (MDecl(m, ps, ss)) ->
                      MDecl(m, ps, erase_pos_stms ss)) ml))
          cl)
