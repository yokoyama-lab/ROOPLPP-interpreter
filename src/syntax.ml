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

(**式*)
type exp =
  | Const of int (**整数*)
  | Var of id (**変数*)
  | ArrayElement of id * exp (**配列*)
  | Nil (**Nil*)
  | Binary of binOp * exp * exp (**二項演算*)
  | Dot of exp * exp (**ドット演算子*)

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
  | Fcase
  | Ecase

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
  | Switch of obj * (case * exp * stm list * exp * break) list * stm list * obj (**switch y case e s esac e break hctiws y*) (*追加部分switch*) 
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

(**メソッドの引数*)
type decl = Decl of dataType * id

(**メソッド*)
type mDecl = MDecl of methodId * decl list * stm list (**method q(...) s*)

(**クラス*)
type cDecl = CDecl of typeId * typeId option * decl list * mDecl list (**class C int x ... method q() ... *)

(**プログラム*)
type prog = Prog of cDecl list (**class .. class*)
