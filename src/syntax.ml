type id = string
type typeId = id
type methodId = id

type dataType =
  | IntegerType
  | ObjectType of typeId
  | CopyType of typeId
  | ObjectArrayType of typeId
  | IntegerArrayType
  | ArrayType
  | ArrayElementType
  | NilType

type binOp =
  | Add
  | Sub
  | Xor
  | Mul
  | Div
  | Mod
  | Band
  | Bor
  | And
  | Or
  | Lt
  | Gt
  | Eq
  | Ne
  | Le
  | Ge

type modOp =
  | ModAdd
  | ModSub
  | ModXor
  
type exp =
  | Const of int
  | Var of id
  | ArrayElement of id * exp
  | Nil
  | Binary of binOp * exp * exp
  | Dot of exp * exp

type obj =
  | VarArray of id * exp option
  | InstVar  of obj * obj

type arg =
  | Id of id
  | Exp of exp
         
type case =
  | Case
  | Fcase
  | Ecase

type break =
  | Break
  | NoBreak
  
type stm =
  | Assign of obj * modOp * exp
  | Swap of obj * obj
  | Conditional of exp * stm list * stm list * exp
  | Loop of exp * stm list * stm list * exp
  | For of id * exp * exp * stm list (*追加部分for*)
  | Switch of obj * (case * exp * stm list * exp * break) list * stm list * obj (*追加部分switch*)
  | ObjectBlock of typeId * id * stm list
  | LocalBlock of dataType * id * exp * stm list * exp
  | LocalCall of methodId * arg list
  | LocalUncall of methodId * arg list
  | ObjectCall of obj * methodId * arg list
  | ObjectUncall of obj * methodId * arg list
  | ObjectConstruction of typeId * obj
  | ObjectDestruction of typeId * obj
  | CopyReference of dataType * obj * obj
  | UncopyReference of dataType * obj * obj
  | ArrayConstruction of (typeId * exp) * obj
  | ArrayDestruction of (typeId * exp) * obj
  | Skip
  | Show of exp
  | Print of string

type decl = Decl of dataType * id

type mDecl = MDecl of methodId * decl list * stm list

type cDecl = CDecl of typeId * typeId option * decl list * mDecl list

type prog = Prog of cDecl list
