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
         
type myfor = (*追加部分for*)
  | NFOR of exp * exp (*回数指定*)
  | AFOR of bool * id (*配列要素 bool=trueなら逆順*)

         
type stm =
  | Assign of obj * modOp * exp
  | Swap of obj * obj
  | Conditional of exp * stm list * stm list * exp
  | Loop of exp * stm list * stm list * exp
  | FOR of id * myfor * stm list (*追加部分for*)
  | Switch of bool * obj * (int * stm list * int * bool) list * obj (*追加部分switch*)         
  | ObjectBlock of typeId * id * stm list
  | LocalBlock of dataType * id * exp * stm list * exp
  | LocalCall of methodId * id list
  | LocalUncall of methodId * id list
  | ObjectCall of obj * methodId * id list
  | ObjectUncall of obj * methodId * id list
  | ObjectConstruction of typeId * obj
  | ObjectDestruction of typeId * obj
  | CopyReference of dataType * obj * obj
  | UncopyReference of dataType * obj * obj
  | ArrayConstruction of (typeId * exp) * id
  | ArrayDestruction of (typeId * exp) * id
  | Skip
  | Show of exp
  | Print of string

type decl = Decl of dataType * id

type mDecl = MDecl of methodId * decl list * stm list

type cDecl = CDecl of typeId * typeId option * decl list * mDecl list

type prog = Prog of cDecl list
