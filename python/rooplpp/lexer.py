"""Lexer for ROOPL++ (from lexer.mll)."""
from __future__ import annotations
from dataclasses import dataclass
from enum import Enum, auto
import re


class TT(Enum):
    """Token types."""
    # Literals
    CONST = auto()
    STRING = auto()
    ID = auto()
    # Operators
    MUL = auto(); DIV = auto(); MOD = auto()
    ADD = auto(); SUB = auto()
    LT = auto(); LE = auto(); GT = auto(); GE = auto()
    EQ = auto(); NE = auto()
    BAND = auto(); XOR = auto(); BOR = auto()
    AND = auto(); OR = auto()
    SWAP = auto()
    WDOT = auto(); WCOLON = auto()
    MODADD = auto(); MODSUB = auto(); MODXOR = auto()
    DOT = auto(); COLON = auto(); COMMA = auto()
    # Brackets
    LPAREN = auto(); RPAREN = auto()
    LBRA = auto(); RBRA = auto()
    # Keywords
    CLASS = auto(); INHERITS = auto(); METHOD = auto()
    CALL = auto(); UNCALL = auto()
    CONSTRUCT = auto(); DESTRUCT = auto()
    SKIP = auto()
    FROM = auto(); DO = auto(); LOOP = auto(); UNTIL = auto()
    FOR = auto(); IN = auto(); END = auto()
    SWITCH = auto(); HCTIWS = auto()
    CASE = auto(); FCASE = auto(); ECASE = auto(); ESAC = auto()
    DEFAULT = auto(); BREAK = auto()
    INT = auto(); NIL = auto()
    IF = auto(); THEN = auto(); ELSE = auto(); FI = auto()
    LOCAL = auto(); DELOCAL = auto()
    NEW = auto(); DELETE = auto()
    COPY = auto(); UNCOPY = auto()
    SHOW = auto(); PRINT = auto()
    EOF = auto()
    # 字句解析器が知らない文字（エラーの位置を運ぶためだけに使う）
    UNKNOWN = auto()


KEYWORDS = {
    "class": TT.CLASS, "inherits": TT.INHERITS, "method": TT.METHOD,
    "call": TT.CALL, "uncall": TT.UNCALL,
    "construct": TT.CONSTRUCT, "destruct": TT.DESTRUCT,
    "skip": TT.SKIP,
    "from": TT.FROM, "do": TT.DO, "loop": TT.LOOP, "until": TT.UNTIL,
    "for": TT.FOR, "in": TT.IN, "end": TT.END,
    "switch": TT.SWITCH, "hctiws": TT.HCTIWS,
    "case": TT.CASE, "fcase": TT.FCASE, "ecase": TT.ECASE, "esac": TT.ESAC,
    "default": TT.DEFAULT, "break": TT.BREAK,
    "int": TT.INT, "nil": TT.NIL,
    "if": TT.IF, "then": TT.THEN, "else": TT.ELSE, "fi": TT.FI,
    "local": TT.LOCAL, "delocal": TT.DELOCAL,
    "new": TT.NEW, "delete": TT.DELETE,
    "copy": TT.COPY, "uncopy": TT.UNCOPY,
    "show": TT.SHOW, "print": TT.PRINT,
}

# Multi-char operators (order matters: longest match first)
OPERATORS = [
    ("<=>", TT.SWAP), ("<=", TT.LE), (">=", TT.GE),
    ("!=", TT.NE), ("&&", TT.AND), ("||", TT.OR),
    ("::", TT.WCOLON), ("..", TT.WDOT),
    ("+=", TT.MODADD), ("-=", TT.MODSUB), ("^=", TT.MODXOR),
]

SINGLE_OPS = {
    '*': TT.MUL, '/': TT.DIV, '%': TT.MOD,
    '+': TT.ADD, '-': TT.SUB,
    '<': TT.LT, '>': TT.GT, '=': TT.EQ,
    '&': TT.BAND, '^': TT.XOR, '|': TT.BOR,
    '.': TT.DOT, ':': TT.COLON, ',': TT.COMMA,
    '(': TT.LPAREN, ')': TT.RPAREN,
    '[': TT.LBRA, ']': TT.RBRA,
}


@dataclass
class Token:
    type: TT
    value: object  # int for CONST, str for ID/STRING, None otherwise
    line: int
    col: int
    # このトークンの次の桁（1 起点。位置の範囲を作るのに使う）
    end_col: int = 0


class ParseError(Exception):
    """構文・字句のエラー。落ちた位置のトークンを運ぶ。

    字句解析器も同じ例外を投げる（知らない文字を素の SyntaxError で落とすと、
    main.py のキャレットつき診断に載らないため）。parser.py が再輸出している
    ので `from rooplpp.parser import ParseError` も従来どおり使える。
    """

    def __init__(self, token: "Token", msg: str = ""):
        self.token = token
        super().__init__(f"Parse error at {token.line}.{token.col}" +
                         (f": {msg}" if msg else ""))


def _unescape(s: str) -> str:
    """Unescape a string literal (strip quotes, process escape sequences)."""
    result = []
    i = 1  # skip opening quote
    while i < len(s) - 1:  # skip closing quote
        if s[i] == '\\' and i + 1 < len(s) - 1:
            c = s[i + 1]
            if c == 'n': result.append('\n')
            elif c == 't': result.append('\t')
            elif c in ('"', '\\', "'"): result.append(c)
            else: result.append(s[i]); result.append(c)
            i += 2
        else:
            result.append(s[i])
            i += 1
    return ''.join(result)


def tokenize(source: str) -> list[Token]:
    tokens = []
    i = 0
    line = 1
    line_start = 0

    while i < len(source):
        col = i - line_start + 1

        # Whitespace
        if source[i] in ' \t\r':
            i += 1
            continue
        if source[i] == '\n':
            line += 1
            i += 1
            line_start = i
            continue

        # Line comments
        if source[i:i+2] == '//':
            while i < len(source) and source[i] != '\n':
                i += 1
            continue

        # String literals
        if source[i] == '"':
            j = i + 1
            while j < len(source) and source[j] != '"':
                if source[j] == '\\':
                    j += 1
                j += 1
            j += 1  # include closing quote
            tokens.append(Token(TT.STRING, _unescape(source[i:j]), line, col,
                                col + (j - i)))
            i = j
            continue

        # Integer constants
        if source[i].isdigit():
            j = i
            while j < len(source) and source[j].isdigit():
                j += 1
            tokens.append(Token(TT.CONST, int(source[i:j]), line, col,
                                col + (j - i)))
            i = j
            continue

        # Identifiers and keywords
        if source[i].isalpha() or source[i] == '_':
            j = i
            while j < len(source) and (source[j].isalnum() or source[j] in ('_', "'")):
                j += 1
            word = source[i:j]
            tt = KEYWORDS.get(word, TT.ID)
            tokens.append(Token(tt, word if tt == TT.ID else None, line, col,
                                col + (j - i)))
            i = j
            continue

        # Multi-character operators
        matched = False
        for op, tt in OPERATORS:
            if source[i:i+len(op)] == op:
                tokens.append(Token(tt, None, line, col, col + len(op)))
                i += len(op)
                matched = True
                break
        if matched:
            continue

        # Single-character operators
        if source[i] in SINGLE_OPS:
            tokens.append(Token(SINGLE_OPS[source[i]], None, line, col, col + 1))
            i += 1
            continue

        # 知らない文字。素の例外で落とさず構文エラーと同じ経路に載せる
        # （「余分な ; はエラー」という修正ヒントに到達できていなかった）
        raise ParseError(Token(TT.UNKNOWN, source[i], line, col, col + 1),
                         f"unknown character {source[i]!r}")

    eof_col = i - line_start + 1
    tokens.append(Token(TT.EOF, None, line, eof_col, eof_col))
    return tokens
