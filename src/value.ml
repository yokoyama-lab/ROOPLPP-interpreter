open Syntax

type locs = int

type env = (id * locs) list

type value =
  | IntVal of int  (*整数*)
  | ObjVal of typeId * env (*オブジェクト*)
  | LocsVal of locs (*ロケーション*)
  | LocsVec of locs list (*ロケーションのベクトルを表す*)

type state = (locs * value) list
