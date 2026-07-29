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
            tokens.append(Token(TT.STRING, _unescape(source[i:j]), line, col))
            i = j
            continue

        # Integer constants
        if source[i].isdigit():
            j = i
            while j < len(source) and source[j].isdigit():
                j += 1
            tokens.append(Token(TT.CONST, int(source[i:j]), line, col))
            i = j
            continue

        # Identifiers and keywords
        if source[i].isalpha() or source[i] == '_':
            j = i
            while j < len(source) and (source[j].isalnum() or source[j] in ('_', "'")):
                j += 1
            word = source[i:j]
            tt = KEYWORDS.get(word, TT.ID)
            tokens.append(Token(tt, word if tt == TT.ID else None, line, col))
            i = j
            continue

        # Multi-character operators
        matched = False
        for op, tt in OPERATORS:
            if source[i:i+len(op)] == op:
                tokens.append(Token(tt, None, line, col))
                i += len(op)
                matched = True
                break
        if matched:
            continue

        # Single-character operators
        if source[i] in SINGLE_OPS:
            tokens.append(Token(SINGLE_OPS[source[i]], None, line, col))
            i += 1
            continue

        raise SyntaxError(
            f"unknown token '{source[i]}' at line {line}, column {col}")

    tokens.append(Token(TT.EOF, None, line, i - line_start + 1))
    return tokens
