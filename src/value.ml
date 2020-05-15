open Syntax

type locs = int
   
type value =
  | IntVal of int  (*整数*)
  | ObjVal of typeId * (string * int) list (*オブジェクト*)
  | LocsVal of locs (*ロケーション*)
  | LocsVec of locs list (*ロケーションのベクトルを表す*)

type env = (id * locs) list


type state =(locs * value) list
