type token =
  | ID of (string)
  | CONST of (int)
  | MUL
  | DIV
  | MOD
  | ADD
  | SUB
  | LT
  | LE
  | GT
  | GE
  | EQ
  | NE
  | BAND
  | XOR
  | BOR
  | AND
  | OR
  | SWAP
  | COMMA
  | WCOLON
  | MODADD
  | MODSUB
  | MODXOR
  | LPAREN
  | RPAREN
  | LBRA
  | RBRA
  | CLASS
  | INHERITS
  | METHOD
  | CALL
  | UNCALL
  | CONSTRUCT
  | DESTRUCT
  | SKIP
  | FROM
  | DO
  | LOOP
  | UNTIL
  | INT
  | NIL
  | IF
  | THEN
  | ELSE
  | FI
  | LOCAL
  | DELOCAL
  | NEW
  | DELETE
  | COPY
  | UNCOPY
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.prog
