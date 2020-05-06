open Syntax

type value =
  | IntVal of int
  | ObjVal of typeId * (string * int) list
  | LocsVal of int
