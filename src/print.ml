(**結果の表示に使用*)
open Syntax
open Value

(**ベクトルを受け取り、文字列に変換*)
let show_vec vec = "[" ^ List.fold_left (^) "" (List.map string_of_int vec) ^ "]"

(**値をプリントするための関数*)
let show_val = function
  | IntVal(n) -> string_of_int n
  | LocsVal(locs) -> "<location> " ^ (string_of_int locs)
  | LocsVec(vec) -> show_vec vec
  | _ -> failwith "error in print_val"

(**結果をプリントする関数*)
let print_result result =
  List.iter print_endline (List.map (fun (id, v) -> id ^ " = " ^ show_val v) result)

(** 閉路を含まないデータ構造を展開して表示する *)
let show_val_rec = function
  | IntVal(n) -> "<int> " ^ string_of_int n
  | ObjVal(x,ss) ->
     let rec f str (s,n) = str ^ "; " ^ s ^ ":" ^ string_of_int n in
     "<object> " ^ x ^ List.fold_left f "{" ss ^ "}"
  | LocsVal(n) -> "<location> " ^ string_of_int n
  | LocsVec(ls) ->
     let rec f str n = str ^ " " ^ string_of_int n in
     "<vector>" ^ List.fold_left f " " ls

(**値をプリントする関数：showなどで使用*)
let print_value_rec v = print_string (show_val_rec v)
