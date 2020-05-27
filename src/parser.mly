%{
open Syntax

let parse_error s = print_endline s

let anyId2obj = function
  | x, None   -> Var x
  | x, Some e -> ArrayElement (x,e)
%}

// リテラル
%token <string> ID     // x, y, abc, ...
%token <int> CONST     // 0, 1, 2, ...

// 演算子
%token MUL       // '*'
%token DIV       // '/'
%token MOD       // '%'
%token ADD       // '+'
%token SUB       // '-'
%token LT        // '<'
%token LE        // "<="
%token GT        // '>'
%token GE        // ">="
%token EQ        // '='
%token NE        // "!="
%token BAND      // '&'
%token XOR       // '^'
%token BOR       // '|'
%token AND       // "&&"
%token OR        // "||"
%token SWAP      // "<=>"
%token COMMA     // ','
%token WCOLON    // "::"
%token MODADD    // "+="
%token MODSUB    // "-="
%token MODXOR    // "^="

// 括弧
%token LPAREN    // '('
%token RPAREN    // ')'
%token LBRA      // '['
%token RBRA      // ']'

// キーワード
%token CLASS     // "class"
%token INHERITS  // "inherits"
%token METHOD    // "method"
%token CALL      // "call"
%token UNCALL    // "uncall"
%token CONSTRUCT // "construct"
%token DESTRUCT  // "destruct"
%token SKIP      // "skip"
%token FROM      // "from"
%token DO        // "do"
%token LOOP      // "loop"
%token UNTIL     // "until"
%token INT       // "int"
%token NIL       // "nil"
%token IF        // "if"
%token THEN      // "then"
%token ELSE      // "else"
%token FI        // "fi"
%token LOCAL     // "local"
%token DELOCAL   // "delocal"
%token NEW       // "new"
%token DELETE    // "delete"
%token COPY      // "copy"
%token UNCOPY    // "uncopy"

// 制御記号
%token EOF       // end_of_file

// operator precedence
%left OR                /* lowest precedence */
%left AND
%left BOR
%left XOR
%left BAND
%nonassoc LT LE GT GE
%nonassoc EQ NE
%left ADD SUB
%left MUL DIV MOD       /* highest precedence */

%start main
%type <Syntax.prog> main

%%

// 開始記号
main:
  | prog EOF { Prog $1 }

// 式
exp:
  | CONST        { Const $1             } // 定数
  | anyId        { anyId2obj $1         } // 変数 or 配列要素
  | NIL          { Nil                  } // nil
  | exp MUL exp  { Binary(Mul,  $1, $3) } // e1 * e2
  | exp DIV exp  { Binary(Div,  $1, $3) } // e1 / e2
  | exp MOD exp  { Binary(Mod,  $1, $3) } // e1 % e2
  | exp ADD exp  { Binary(Add,  $1, $3) } // e1 + e2
  | exp SUB exp  { Binary(Sub,  $1, $3) } // e1 - e2
  | exp LT exp   { Binary(Lt,   $1, $3) } // e1 < e2
  | exp LE exp   { Binary(Le,   $1, $3) } // e1 <= e2
  | exp GT exp   { Binary(Gt,   $1, $3) } // e1 > e2
  | exp GE exp   { Binary(Ge,   $1, $3) } // e1 >= e2
  | exp EQ exp   { Binary(Eq,   $1, $3) } // e1 = e2
  | exp NE exp   { Binary(Ne,   $1, $3) } // e1 != e2
  | exp BAND exp { Binary(Band, $1, $3) } // e1 & e2
  | exp XOR exp  { Binary(Xor,  $1, $3) } // e1 ^ e2
  | exp BOR exp  { Binary(Bor,  $1, $3) } // e1 | e2
  | exp AND exp  { Binary(And,  $1, $3) } // e1 && e2
  | exp OR exp   { Binary(Or,   $1, $3) } // e1 && e2
  | LPAREN exp RPAREN { $2 }              // ( e )

modop:
  | MODADD { ModAdd }
  | MODSUB { ModSub }
  | MODXOR { ModXor }

typeId:
  | ID   { $1 }

arrayTypeName:
  | typeId LBRA exp RBRA { ($1, $3) }
  | INT    LBRA exp RBRA { ("int", $3) }

anyIds1:
  | anyIds1 COMMA anyId { $1 @ [$3]}
  | anyId               { [$1] }

anyIds:
  | anyIds1 { $1 }
  |         { [] }

anyId:
  | ID LBRA exp RBRA { ($1, Some $3) }
  | ID               { ($1, None) }

// statement
stms1:
  | stms1 stm { $1 @ [$2]}
  | stm       { [$1] }

stm:
  | anyId modop exp
    { Assign($1, $2, $3) } // x (+,-,^)= e
  | IF exp THEN stms1 else_opt FI exp
    { Conditional($2, $4, $5, $7) } // if e then s else s fi e  or  if e then s fi e
  | FROM exp do_opt loop_opt UNTIL exp
    { Loop($2, $3, $4, $6) } // from e do s loop s until e or   or  from e do s until e  or  from e loop s until e
  | CALL methodName LPAREN anyIds RPAREN
    { LocalCall($2, $4) } // call q(x, ... , x)
  | UNCALL methodName LPAREN anyIds RPAREN
    { LocalUncall($2, $4) } // uncall q(x, ... , x)
  | CALL anyId WCOLON methodName LPAREN anyIds RPAREN
    { ObjectCall($2, $4, $6) } // call x::q(x, ..., x)
  | UNCALL anyId WCOLON methodName LPAREN anyIds RPAREN
    { ObjectUncall($2, $4, $6) } // uncall x::q(x, ..., x)
  | LOCAL dataType ID EQ exp stms1 DELOCAL dataType ID EQ exp // TODO: check ids and types
    { LocalBlock($2, $3, $5, $6, $11) } // local ... delocal ... dataTypes must be equal.
  | CONSTRUCT typeId ID stms1 DESTRUCT ID    // TODO: check ids
    { ObjectBlock($2, $3, $4) } // construct c x  s  destruct x
  | NEW arrayTypeName ID
    { ArrayConstruction($2, $3) } // new Foo[length] fooList
  | NEW typeId anyId
    { ObjectConstruction($2, $3) } // new Foo foo
  | DELETE arrayTypeName ID
    { ArrayDestruction($2, $3) } // delete Foo[length] fooList
  | DELETE typeId anyId
    { ObjectDestruction($2, $3) } // delete Foo foo
  | COPY dataType anyId anyId
    { CopyReference($2, $3, $4) } // copy int x y
  | UNCOPY dataType anyId anyId
    { UncopyReference($2, $3, $4) }            // uncopy int x y
  | SKIP
    { Skip } // skip
  | anyId SWAP anyId
    { Swap($1, $3) } // x <=> x

else_opt:
  | ELSE stms1 { $2 }
  |            { [Skip] }

do_opt:
  | DO stms1 { $2 }
  |          { [Skip] }

loop_opt:
  | LOOP stms1 { $2 }
  |            { [Skip] }

dataType:
  | INT LBRA RBRA    { IntegerArrayType   } // int[]
  | INT              { IntegerType        } // int
  | typeId LBRA RBRA { ObjectArrayType $1 } // Foo[]
  | typeId           { ObjectType $1      } // Foo

// method
methodName:
  | ID { $1 }

methDec:
  | METHOD methodName LPAREN varDecCommas RPAREN stms1
    { MDecl($2, $4, $6) } // method q(t x, ... , t x) s

methDecs:
  | methDecs methDec { $1 @ [$2] }
  | methDec          { [$1]      }

inherits:
  | INHERITS typeId { Some $2 }
  |                 { None }

varDec:
  | dataType ID { Decl($1,$2) }

varDecCommas1:
  | varDecCommas1 COMMA varDec { $1 @ [$3] }
  | varDec                     { [$1]      }

varDecCommas:
  | varDecCommas1 { $1 }
  |               { [] }

varDecs1:
  | varDecs1 varDec { $1 @ [$2] }
  | varDec          { [$1] }

varDecs:
  | varDecs1 { $1 }
  |          { [] }

// class decfinition
aClass:
  | CLASS typeId inherits varDecs methDecs { CDecl($2, $3, $4, $5) }

classList:
  | classList aClass  { $1 @ [$2] }
  |                   { [] }

// program
prog:
  | classList     { $1 }

%%
