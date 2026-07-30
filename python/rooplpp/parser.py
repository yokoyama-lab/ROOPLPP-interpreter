"""Recursive descent parser for ROOPL++ (from parser.mly).

Uses Pratt parsing for expressions to handle operator precedence.
"""
from __future__ import annotations
from .lexer import Token, TT
from .syntax import *
import sys


class ParseError(Exception):
    def __init__(self, token: Token, msg: str = ""):
        self.token = token
        super().__init__(f"Parse error at {token.line}.{token.col}" +
                         (f": {msg}" if msg else ""))


class Parser:
    def __init__(self, tokens: list[Token]):
        self.tokens = tokens
        self.pos = 0

    # --- Utilities ---

    def peek(self) -> Token:
        return self.tokens[self.pos]

    def advance(self) -> Token:
        t = self.tokens[self.pos]
        self.pos += 1
        return t

    def expect(self, tt: TT) -> Token:
        t = self.advance()
        if t.type != tt:
            raise ParseError(t, f"expected {tt.name}, got {t.type.name}")
        return t

    def match(self, tt: TT) -> Token | None:
        if self.peek().type == tt:
            return self.advance()
        return None

    def at(self, *tts: TT) -> bool:
        return self.peek().type in tts

    # --- Expression parsing (Pratt parser) ---

    # Precedence table (higher = binds tighter)
    PREC = {
        TT.OR: 1, TT.AND: 2, TT.BOR: 3, TT.XOR: 4, TT.BAND: 5,
        TT.LT: 6, TT.LE: 6, TT.GT: 6, TT.GE: 6,
        TT.EQ: 7, TT.NE: 7,
        TT.ADD: 8, TT.SUB: 8,
        TT.MUL: 9, TT.DIV: 9, TT.MOD: 9,
        TT.DOT: 11,
    }

    # Non-associative operators (no chaining)
    NONASSOC = {TT.LT, TT.LE, TT.GT, TT.GE, TT.EQ, TT.NE}

    BINOP_MAP = {
        TT.ADD: BinOp.Add, TT.SUB: BinOp.Sub, TT.XOR: BinOp.Xor,
        TT.MUL: BinOp.Mul, TT.DIV: BinOp.Div, TT.MOD: BinOp.Mod,
        TT.BAND: BinOp.Band, TT.BOR: BinOp.Bor,
        TT.AND: BinOp.And, TT.OR: BinOp.Or,
        TT.LT: BinOp.Lt, TT.GT: BinOp.Gt,
        TT.EQ: BinOp.Eq, TT.NE: BinOp.Ne,
        TT.LE: BinOp.Le, TT.GE: BinOp.Ge,
    }

    def _span(self, start_tok) -> Pos:
        """start_tok から直前に読んだトークンまでの範囲。

        列は 0 起点に直す（字句解析器は 1 起点。OCaml 側 parser.mly の span と
        同じ表現にするため）。
        """
        prev = self.tokens[self.pos - 1] if self.pos > 0 else start_tok
        return Pos(start_tok.line, start_tok.col - 1,
                   prev.line, max(prev.end_col, prev.col) - 1)

    def parse_exp(self, min_prec: int = 0) -> Exp:
        start = self.peek()
        left = self._parse_prefix()
        while True:
            tt = self.peek().type
            if tt not in self.PREC or self.PREC[tt] < min_prec:
                break
            if tt == TT.DOT:
                self.advance()
                right = self._parse_prefix()
                left = Dot(left, right)
                continue
            prec = self.PREC[tt]
            op = self.BINOP_MAP[tt]
            self.advance()
            # For non-associative operators, use prec+1 for right side
            # For left-associative operators, use prec+1 too
            right = self.parse_exp(prec + 1)
            # 失敗しうる形（二項演算）には位置を付ける
            left = EPos(self._span(start), Binary(op, left, right))
        return left

    def _parse_prefix(self) -> Exp:
        t = self.peek()
        if t.type == TT.CONST:
            self.advance()
            return Const(t.value)
        if t.type == TT.NIL:
            self.advance()
            return Nil()
        if t.type == TT.LPAREN:
            self.advance()
            e = self.parse_exp()
            self.expect(TT.RPAREN)
            return e
        if t.type == TT.SUB:
            self.advance()
            # Unary minus: -e becomes Binary(Sub, Const(0), e)
            e = self.parse_exp(10)  # UNARY precedence
            return EPos(self._span(t), Binary(BinOp.Sub, Const(0), e))
        if t.type == TT.ID:
            return self._anyid_as_exp()
        raise ParseError(t, f"unexpected token {t.type.name} in expression")

    def _anyid_as_exp(self) -> Exp:
        """Parse anyId and convert to expression via anyId2obj."""
        start = self.peek()
        obj = self.parse_anyid()
        return EPos(self._span(start), _anyid2obj(obj))

    # --- anyId parsing (used in both expression and statement context) ---

    def parse_anyid(self) -> Obj:
        """Parse anyId: ID | ID[exp] | anyId.anyId"""
        t = self.expect(TT.ID)
        if self.match(TT.LBRA):
            idx = self.parse_exp()
            self.expect(TT.RBRA)
            obj = VarArray(t.value, idx)
        else:
            obj = VarArray(t.value)
        # Handle dot chaining
        while self.match(TT.DOT):
            t2 = self.expect(TT.ID)
            if self.match(TT.LBRA):
                idx = self.parse_exp()
                self.expect(TT.RBRA)
                right = VarArray(t2.value, idx)
            else:
                right = VarArray(t2.value)
            obj = InstVar(obj, right)
        return obj

    # --- Argument parsing ---

    def parse_arg(self) -> Arg:
        """Parse method call argument. ID alone -> IdArg; otherwise -> ExpArg."""
        if self.at(TT.ID):
            # Look ahead: if ID is followed by comma or rparen, treat as IdArg
            saved = self.pos
            t = self.advance()
            if self.at(TT.COMMA, TT.RPAREN):
                return IdArg(t.value)
            # Otherwise, it's part of an expression: backtrack
            self.pos = saved
        return ExpArg(self.parse_exp())

    def parse_anyids(self) -> list[Arg]:
        """Parse argument list (possibly empty)."""
        if self.at(TT.RPAREN):
            return []
        args = [self.parse_arg()]
        while self.match(TT.COMMA):
            args.append(self.parse_arg())
        return args

    # --- Type parsing ---

    def parse_datatype(self) -> DataType:
        if self.match(TT.INT):
            if self.match(TT.LBRA):
                self.expect(TT.RBRA)
                return IntegerArrayType()
            return IntegerType()
        t = self.expect(TT.ID)
        if self.match(TT.LBRA):
            self.expect(TT.RBRA)
            return ObjectArrayType(t.value)
        return ObjectType(t.value)

    def parse_arraytypename(self) -> tuple[str, Exp] | None:
        """Try to parse arrayTypeName: typeId[exp] or int[exp].
        Returns None if not an array type (backtracks)."""
        saved = self.pos
        if self.match(TT.INT):
            if self.match(TT.LBRA):
                e = self.parse_exp()
                self.expect(TT.RBRA)
                return ("int", e)
            self.pos = saved
            return None
        if self.at(TT.ID):
            t = self.advance()
            if self.match(TT.LBRA):
                e = self.parse_exp()
                self.expect(TT.RBRA)
                return (t.value, e)
            self.pos = saved
            return None
        return None

    # --- Statement parsing ---

    def parse_stms(self) -> list[Stm]:
        """Parse one or more statements.

        文には位置を付ける（診断で「どの行で落ちたか」を正確に言うため。
        OCaml 側 parser.mly の at_pos に対応）。
        """
        stms = [self._parse_stm_at()]
        while self._at_stm_start():
            stms.append(self._parse_stm_at())
        return stms

    def _parse_stm_at(self) -> Stm:
        t = self.peek()
        stm = self.parse_stm()
        return Positioned(self._span(t), stm)

    def _at_stm_start(self) -> bool:
        """Check if current token can start a statement."""
        return self.at(
            TT.ID, TT.IF, TT.FROM, TT.FOR, TT.SWITCH,
            TT.CALL, TT.UNCALL, TT.LOCAL, TT.CONSTRUCT,
            TT.NEW, TT.DELETE, TT.COPY, TT.UNCOPY,
            TT.SKIP, TT.SHOW, TT.PRINT
        )

    def parse_stm(self) -> Stm:
        t = self.peek()

        if t.type == TT.SKIP:
            self.advance()
            return Skip()

        if t.type == TT.SHOW:
            self.advance()
            self.expect(TT.LPAREN)
            e = self.parse_exp()
            self.expect(TT.RPAREN)
            return Show(e)

        if t.type == TT.PRINT:
            self.advance()
            self.expect(TT.LPAREN)
            s = self.expect(TT.STRING)
            self.expect(TT.RPAREN)
            return Print(s.value)

        if t.type == TT.IF:
            return self._parse_conditional()

        if t.type == TT.FROM:
            return self._parse_loop()

        if t.type == TT.FOR:
            return self._parse_for()

        if t.type == TT.SWITCH:
            return self._parse_switch()

        if t.type == TT.CALL:
            return self._parse_call(is_uncall=False)

        if t.type == TT.UNCALL:
            return self._parse_call(is_uncall=True)

        if t.type == TT.LOCAL:
            return self._parse_local()

        if t.type == TT.CONSTRUCT:
            return self._parse_construct()

        if t.type == TT.NEW:
            return self._parse_new()

        if t.type == TT.DELETE:
            return self._parse_delete()

        if t.type == TT.COPY:
            return self._parse_copy(is_uncopy=False)

        if t.type == TT.UNCOPY:
            return self._parse_copy(is_uncopy=True)

        if t.type == TT.ID:
            return self._parse_id_stm()

        raise ParseError(t, f"unexpected token {t.type.name}")

    def _parse_id_stm(self) -> Stm:
        """Parse statement starting with ID: assignment, swap, or error."""
        obj = self.parse_anyid()
        if self.at(TT.MODADD, TT.MODSUB, TT.MODXOR):
            op_map = {TT.MODADD: ModOp.ModAdd, TT.MODSUB: ModOp.ModSub,
                      TT.MODXOR: ModOp.ModXor}
            t = self.advance()
            e = self.parse_exp()
            return Assign(obj, op_map[t.type], e)
        if self.match(TT.SWAP):
            obj2 = self.parse_anyid()
            return Swap(obj, obj2)
        raise ParseError(self.peek(), "expected +=, -=, ^=, or <=>")

    def _parse_conditional(self) -> Stm:
        self.expect(TT.IF)
        e1 = self.parse_exp()
        self.expect(TT.THEN)
        s1 = self.parse_stms()
        if self.match(TT.ELSE):
            s2 = self.parse_stms()
        else:
            s2 = [Skip()]
        self.expect(TT.FI)
        e2 = self.parse_exp()
        return Conditional(e1, s1, s2, e2)

    def _parse_loop(self) -> Stm:
        self.expect(TT.FROM)
        e1 = self.parse_exp()
        if self.match(TT.DO):
            s1 = self.parse_stms()
        else:
            s1 = [Skip()]
        if self.match(TT.LOOP):
            s2 = self.parse_stms()
        else:
            s2 = [Skip()]
        self.expect(TT.UNTIL)
        e2 = self.parse_exp()
        return Loop(e1, s1, s2, e2)

    def _parse_for(self) -> Stm:
        self.expect(TT.FOR)
        var = self.expect(TT.ID).value
        self.expect(TT.IN)
        self.expect(TT.LPAREN)
        e1 = self.parse_exp()
        self.expect(TT.WDOT)
        e2 = self.parse_exp()
        self.expect(TT.RPAREN)
        self.expect(TT.DO)
        body = self.parse_stms()
        self.expect(TT.END)
        return For(var, e1, e2, body)

    def _parse_switch(self) -> Stm:
        self.expect(TT.SWITCH)
        obj1 = self.parse_anyid()
        cases = self._parse_switch_cases()
        self.expect(TT.DEFAULT)
        default = self.parse_stms()
        self.expect(TT.BREAK)
        self.expect(TT.HCTIWS)
        obj2 = self.parse_anyid()
        return Switch(obj1, cases, default, obj2)

    def _parse_switch_cases(self) -> list[CaseEntry]:
        cases = []
        while not self.at(TT.DEFAULT):
            cases.append(self._parse_switch_case())
        return cases

    def _parse_switch_case(self) -> CaseEntry:
        # case marker
        if self.match(TT.CASE):
            c1 = Case.Case
            exps1 = self._parse_exps1()
        else:
            c1 = Case.NoCase
            exps1 = []
        # body
        body = self.parse_stms()
        # esac marker
        if self.match(TT.ESAC):
            exps2 = self._parse_exps1()
            if self.match(TT.BREAK):
                return (c1, exps1), body, (Esac.Esac, exps2, Break.Break)
            return (c1, exps1), body, (Esac.Esac, exps2, Break.NoBreak)
        return (c1, exps1), body, (Esac.NoEsac, [], Break.NoBreak)

    def _parse_exps1(self) -> list[Exp]:
        """Parse colon-separated expression list."""
        exps = [self.parse_exp()]
        while self.match(TT.COLON):
            exps.append(self.parse_exp())
        return exps

    def _parse_call(self, is_uncall: bool) -> Stm:
        self.advance()  # CALL or UNCALL
        # Try object call: anyId :: methodName (args)
        # or local call: methodName (args)
        # Disambiguate: parse anyId, then check for ::
        obj = self.parse_anyid()
        if self.match(TT.WCOLON):
            mid = self.expect(TT.ID).value
            self.expect(TT.LPAREN)
            args = self.parse_anyids()
            self.expect(TT.RPAREN)
            if is_uncall:
                return ObjectUncall(obj, mid, args)
            return ObjectCall(obj, mid, args)
        else:
            # Local call: obj must be VarArray(name, None)
            if not isinstance(obj, VarArray) or obj.index is not None:
                raise ParseError(self.peek(), "expected method name for local call")
            mid = obj.name
            self.expect(TT.LPAREN)
            args = self.parse_anyids()
            self.expect(TT.RPAREN)
            if is_uncall:
                return LocalUncall(mid, args)
            return LocalCall(mid, args)

    def _parse_local(self) -> Stm:
        self.expect(TT.LOCAL)
        dt = self.parse_datatype()
        name = self.expect(TT.ID).value
        self.expect(TT.EQ)
        e1 = self.parse_exp()
        body = self.parse_stms()
        self.expect(TT.DELOCAL)
        dt2 = self.parse_datatype()
        name2 = self.expect(TT.ID).value
        self.expect(TT.EQ)
        e2 = self.parse_exp()
        if name != name2:
            print(f"Warning: LOCAL/DELOCAL variable names do not match: {name} vs {name2}",
                  file=sys.stderr)
        return LocalBlock(dt, name, e1, body, e2)

    def _parse_construct(self) -> Stm:
        self.expect(TT.CONSTRUCT)
        tid = self.expect(TT.ID).value
        name = self.expect(TT.ID).value
        body = self.parse_stms()
        self.expect(TT.DESTRUCT)
        name2 = self.expect(TT.ID).value
        if name != name2:
            print(f"Warning: CONSTRUCT/DESTRUCT variable names do not match: {name} vs {name2}",
                  file=sys.stderr)
        return ObjectBlock(tid, name, body)

    def _parse_new(self) -> Stm:
        self.expect(TT.NEW)
        # Try arrayTypeName first: typeId[exp] or int[exp]
        atn = self.parse_arraytypename()
        if atn is not None:
            tid, size = atn
            obj = self.parse_anyid()
            return ArrayConstruction(tid, size, obj)
        # Otherwise: typeId anyId
        tid = self.expect(TT.ID).value
        obj = self.parse_anyid()
        return ObjectConstruction(tid, obj)

    def _parse_delete(self) -> Stm:
        self.expect(TT.DELETE)
        atn = self.parse_arraytypename()
        if atn is not None:
            tid, size = atn
            obj = self.parse_anyid()
            return ArrayDestruction(tid, size, obj)
        tid = self.expect(TT.ID).value
        obj = self.parse_anyid()
        return ObjectDestruction(tid, obj)

    def _parse_copy(self, is_uncopy: bool) -> Stm:
        self.advance()  # COPY or UNCOPY
        dt = self.parse_datatype()
        o1 = self.parse_anyid()
        o2 = self.parse_anyid()
        if is_uncopy:
            return UncopyReference(dt, o1, o2)
        return CopyReference(dt, o1, o2)

    # --- Declaration parsing ---

    def parse_vardec(self) -> Decl:
        dt = self.parse_datatype()
        name = self.expect(TT.ID).value
        return Decl(dt, name)

    def parse_vardecs(self) -> list[Decl]:
        """Parse zero or more variable declarations (field declarations)."""
        decls = []
        while self._at_datatype() and self._is_vardec():
            decls.append(self.parse_vardec())
        return decls

    def _at_datatype(self) -> bool:
        return self.at(TT.INT, TT.ID)

    def _is_vardec(self) -> bool:
        """Lookahead to distinguish varDec from methodDec or other constructs."""
        if self.at(TT.INT):
            return True
        # ID followed by ID or ID[] ID -> varDec
        # ID followed by other -> not varDec (could be a method name via parse context)
        saved = self.pos
        if self.at(TT.ID):
            self.advance()
            if self.at(TT.ID):
                self.pos = saved
                return True
            if self.at(TT.LBRA):
                self.advance()
                if self.at(TT.RBRA):
                    self.advance()
                    if self.at(TT.ID):
                        self.pos = saved
                        return True
                self.pos = saved
                return False
            self.pos = saved
            return False
        return False

    def parse_vardeccommas(self) -> list[Decl]:
        """Parse comma-separated variable declarations (method params)."""
        if self.at(TT.RPAREN):
            return []
        decls = [self.parse_vardec()]
        while self.match(TT.COMMA):
            decls.append(self.parse_vardec())
        return decls

    def parse_methdec(self) -> MDecl:
        self.expect(TT.METHOD)
        name = self.expect(TT.ID).value
        self.expect(TT.LPAREN)
        params = self.parse_vardeccommas()
        self.expect(TT.RPAREN)
        body = self.parse_stms()
        return MDecl(name, params, body)

    def parse_methdecs(self) -> list[MDecl]:
        methods = [self.parse_methdec()]
        while self.at(TT.METHOD):
            methods.append(self.parse_methdec())
        return methods

    def parse_class(self) -> CDecl:
        self.expect(TT.CLASS)
        name = self.expect(TT.ID).value
        inherits = None
        if self.match(TT.INHERITS):
            inherits = self.expect(TT.ID).value
        fields = self.parse_vardecs()
        methods = self.parse_methdecs()
        return CDecl(name, inherits, fields, methods)

    def parse_prog(self) -> Prog:
        classes = []
        while self.at(TT.CLASS):
            classes.append(self.parse_class())
        self.expect(TT.EOF)
        return Prog(classes)


def _anyid2obj(obj: Obj) -> Exp:
    """Convert Obj (from anyId) to Exp (for expression context). Mirrors OCaml anyId2obj."""
    match obj:
        case VarArray(name, None): return Var(name)
        case VarArray(name, idx): return ArrayElement(name, idx)
        case InstVar(o1, o2): return Dot(_anyid2obj(o1), _anyid2obj(o2))


def parse(source: str) -> Prog:
    """Parse ROOPL++ source code into a Prog AST."""
    from .lexer import tokenize
    tokens = tokenize(source)
    p = Parser(tokens)
    return p.parse_prog()
