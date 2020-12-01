{
open Parser
open Syntax
open Lexing

let unescapeInitTail (s:string) : string =
  let rec unesc s = match s with
      '\\'::c::cs when List.mem c ['\"'; '\\'; '\''] -> c :: unesc cs
    | '\\'::'n'::cs  -> '\n' :: unesc cs
    | '\\'::'t'::cs  -> '\t' :: unesc cs
    | '\"'::[]    -> []
    | c::cs      -> c :: unesc cs
    | _         -> []
                     (* explode/implode from caml FAQ *)
  in let explode (s : string) : char list =
       let rec exp i l =
         if i < 0 then l else exp (i - 1) (s.[i] :: l) in
       exp (String.length s - 1) []
     in let implode (l : char list) : string =
          let res = Buffer.create (List.length l) in
          List.iter (Buffer.add_char res) l;
          Buffer.contents res
        in implode (unesc (List.tl (explode s)))

let incr_lineno (lexbuf:Lexing.lexbuf) : unit =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
                        pos_lnum = pos.pos_lnum + 1;
                        pos_bol = pos.pos_cnum;
                      }
}

let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z' '_']
let alnum = digit | alpha | '\''
let u = ['\000'-'\255']           (* universal: any character *)

rule token = parse
  (* 定数 *)
  | digit+
    { let str = Lexing.lexeme lexbuf in
      CONST (int_of_string str) }
  | '\"' ((u # ['\"' '\\' '\n']) | ('\\' ('\"' | '\\' | '\'' | 'n' | 't')))* '\"' {let s = lexeme lexbuf in STRING (unescapeInitTail s)}

  (* コメント *)
  | "//" [^'\n']* { token lexbuf }

  (* 演算子 *)
  | '*'       { MUL }
  | '/'       { DIV }
  | '%'       { MOD }
  | '+'       { ADD }
  | '-'       { SUB }
  | '<'       { LT  }
  | "<="      { LE  }
  | '>'       { GT  }
  | ">="      { GE  }
  | '='       { EQ  }
  | "!="      { NE  }
  | '&'       { BAND }
  | '^'       { XOR }
  | '|'       { BOR }
  | "&&"      { AND }
  | "||"      { OR }
  | "<=>"     { SWAP }
  (* 追加部分for *)
  | ".."      { WDOT }
  | ','       { COMMA }
  | "::"      { WCOLON }
  | "+="      { MODADD }
  | "-="      { MODSUB }
  | "^="      { MODXOR }
  | '.'       { DOT }
  (* 追加部分siwtch *)
  | ':'	      { COLON }
  (* 括弧 *)
  | '('       { LPAREN }
  | ')'       { RPAREN }
  | '['       { LBRA }
  | ']'       { RBRA }

  (* キーワード *)
  | "class"     { CLASS }
  | "inherits"  { INHERITS }
  | "method"    { METHOD }
  | "call"      { CALL }
  | "uncall"    { UNCALL }
  | "construct" { CONSTRUCT }
  | "destruct"  { DESTRUCT }
  | "skip"      { SKIP }
  | "from"      { FROM }
  | "do"        { DO }
  | "loop"      { LOOP }
  | "until"     { UNTIL }
  (* 追加部分for *)
  | "for"	{ FOR }
  | "in"	{ IN }
  | "end"	{ END }
  (* 追加部分switch　*)
  | "switch"	{ SWITCH }
  | "hctiws"	{ HCTIWS }
  | "case"	{ CASE }
  | "fcase"	{ FCASE }
  | "ecase"	{ ECASE }
  | "esac"	{ ESAC }
  | "default"	{ DEFAULT }
  | "break"	{ BREAK }

  | "int"       { INT }
  | "nil"       { NIL }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "fi"        { FI }
  | "local"     { LOCAL }
  | "delocal"   { DELOCAL }
  | "new"       { NEW }
  | "delete"    { DELETE }
  | "copy"      { COPY }
  | "uncopy"    { UNCOPY }
  | "show"      { SHOW }
  | "print"     { PRINT }

  (* 変数 *)
  | alpha alnum*
    { ID (Lexing.lexeme lexbuf) }

  (* スペース *)
  | [' ' '\t' '\r'] {token lexbuf}
  | '\n' {incr_lineno lexbuf; token lexbuf}

  | eof { EOF }

  | _
    {
      let message = Printf.sprintf
        "unknown token %s near characters %d-%d"
        (Lexing.lexeme lexbuf)
        (Lexing.lexeme_start lexbuf)
        (Lexing.lexeme_end lexbuf)
      in
      failwith message
    }
