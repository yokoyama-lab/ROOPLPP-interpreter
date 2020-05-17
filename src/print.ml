open Syntax
open Value
open Eval

let print_result result =   
let rec print_result2 =    
let rec print_vec = function
  | [] -> "]"
  | l :: tl -> "[" ^ (string_of_int l) ^ (print_vec tl)
in
let print_val = function
  | IntVal(n) -> string_of_int n
  | LocsVal(locs) -> string_of_int locs
  | LocsVec(vec) -> print_vec vec
  | _ -> failwith "error in print_val"
in
function
| [] -> ""
| (id, v) :: tl ->
   (id ^ " = " ^ (print_val v)) ^ "\n" ^ (print_result2 tl)
in                                         
print_string (print_result2 result)
