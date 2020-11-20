(**Parser_error*)

exception Parse_error of Lexing.position * Lexing.position

let concatMap f xs = List.concat (List.map f xs)
