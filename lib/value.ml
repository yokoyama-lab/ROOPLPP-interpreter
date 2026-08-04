(**データ型の定義（値）*)
open Syntax

(**ロケーション*)
type locs = int

(**環境*)
type env = (id * locs) list

(**値*)
type value =
  | IntVal of int  (**整数*)
  | ObjVal of typeId * env (**オブジェクト*)
  | LocsVal of locs (**ロケーション*)
  | LocsVec of locs list (**配列*)

type state = (locs * value) list
