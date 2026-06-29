{
open Parser
open Syntax
open Lexing

let unescapeInitTail (s:string) : string =
  let is_digit c = c >= '0' && c <= '9' in
  let rec unesc s = match s with
      '\\'::c::cs when List.mem c ['\"'; '\\'; '\''] -> c :: unesc cs
    | '\\'::'n'::cs  -> '\n' :: unesc cs
    | '\\'::'t'::cs  -> '\t' :: unesc cs
    (* \DDD : 10進3桁エスケープ。pretty.ml の escape_string_literal が
       制御文字（ESC, NUL 等）に対して出力する。0..255 のみ採用し、
       範囲外（\256 以上）は次の規則でバックスラッシュをそのまま通す。 *)
    | '\\'::d1::d2::d3::cs
         when is_digit d1 && is_digit d2 && is_digit d3
              && (Char.code d1 - 48) * 100 + (Char.code d2 - 48) * 10
                 + (Char.code d3 - 48) < 256 ->
       Char.chr ((Char.code d1 - 48) * 100 + (Char.code d2 - 48) * 10
                 + (Char.code d3 - 48)) :: unesc cs
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

(* 文字リテラルの整数コード。引数は前後のクォートを含む字句 'A' / '\n' 等。
   1バイト ASCII 文字のみ対応（マルチバイト UTF-8 は字句規則が弾く）。 *)
let decode_char (s:string) : int =
  if String.length s = 3 then Char.code s.[1]   (* 'A' *)
  else match s.[2] with                         (* '\n' '\t' '\\' '\'' '\"' *)
    | 'n' -> Char.code '\n'
    | 't' -> Char.code '\t'
    | c   -> Char.code c
}

let digit = ['0'-'9']
let hexdigit = ['0'-'9' 'a'-'f' 'A'-'F']
let bindigit = ['0'-'1']
let alpha = ['A'-'Z' 'a'-'z' '_']
let alnum = digit | alpha | '\''
let u = ['\000'-'\255']           (* universal: any character *)

rule token = parse
  (* 定数。16進 0x.. / 2進 0b.. / 文字 'A' はいずれも CONST に落とす
     （int_of_string が 0x/0b/0o を解釈。文字は ASCII コード）。 *)
  | ("0x"|"0X") hexdigit+
    { CONST (int_of_string (Lexing.lexeme lexbuf)) }
  | ("0b"|"0B") bindigit+
    { CONST (int_of_string (Lexing.lexeme lexbuf)) }
  | digit+
    { let str = Lexing.lexeme lexbuf in
      CONST (int_of_string str) }
  | '\'' ((u # ['\'' '\\' '\n']) | ('\\' ('\'' | '\\' | '\"' | 'n' | 't'))) '\''
    { CONST (decode_char (Lexing.lexeme lexbuf)) }
  | '\"' ((u # ['\"' '\\' '\n']) | ('\\' ('\"' | '\\' | '\'' | 'n' | 't')) | ('\\' digit digit digit))* '\"' {let s = lexeme lexbuf in STRING (unescapeInitTail s)}

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
