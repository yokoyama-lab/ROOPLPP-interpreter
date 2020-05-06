{
open Parser
open Syntax
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z' '_']
let alnum = digit | alpha | '\''

rule token = parse
  (* 定数 *)
  | digit+
    { let str = Lexing.lexeme lexbuf in
      CONST (int_of_string str) }

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
  | ','       { COMMA }
  | "::"      { WCOLON }
  | "+="      { MODADD }
  | "-="      { MODSUB }
  | "^="      { MODXOR }

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

  (* 変数 *)
  | alpha alnum*
    { ID (Lexing.lexeme lexbuf) }

  (* スペース *)
  | space+    { token lexbuf }

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
