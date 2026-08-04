"""AST data type definitions for ROOPL++ (from syntax.ml)."""
from __future__ import annotations
from dataclasses import dataclass
from enum import Enum, auto
from typing import Optional


# --- Type identifiers ---
Id = str
TypeId = str
MethodId = str


# --- Data types ---
class DataType:
    pass

@dataclass(frozen=True)
class IntegerType(DataType):
    pass

@dataclass(frozen=True)
class ObjectType(DataType):
    type_id: TypeId

@dataclass(frozen=True)
class CopyType(DataType):
    type_id: TypeId

@dataclass(frozen=True)
class ObjectArrayType(DataType):
    type_id: TypeId

@dataclass(frozen=True)
class IntegerArrayType(DataType):
    pass

@dataclass(frozen=True)
class ArrayType(DataType):
    pass

@dataclass(frozen=True)
class ArrayElementType(DataType):
    pass

@dataclass(frozen=True)
class NilType(DataType):
    pass


# --- Binary operators ---
class BinOp(Enum):
    Add = auto()
    Sub = auto()
    Xor = auto()
    Mul = auto()
    Div = auto()
    Mod = auto()
    Band = auto()
    Bor = auto()
    And = auto()
    Or = auto()
    Lt = auto()
    Gt = auto()
    Eq = auto()
    Ne = auto()
    Le = auto()
    Ge = auto()


# --- Modification operators ---
class ModOp(Enum):
    ModAdd = auto()
    ModSub = auto()
    ModXor = auto()


# --- Expressions ---
@dataclass(frozen=True)
class Pos:
    """位置（診断用。意味論には影響しない）。文にも式にも付く。

    列は 0 起点で、end_col は最後の文字の次を指す（キャレットの幅に使う）。
    """
    line: int
    col: int
    end_line: int = 0
    end_col: int = 0


class Exp:
    pass


@dataclass(frozen=True)
class EPos(Exp):
    """位置情報つきの式（構文解析器が付ける）。"""
    pos: Pos
    exp: Exp

@dataclass(frozen=True)
class Const(Exp):
    value: int

@dataclass(frozen=True)
class Var(Exp):
    name: Id

@dataclass(frozen=True)
class ArrayElement(Exp):
    name: Id
    index: Exp

@dataclass(frozen=True)
class Nil(Exp):
    pass

@dataclass(frozen=True)
class Binary(Exp):
    op: BinOp
    left: Exp
    right: Exp

@dataclass(frozen=True)
class Dot(Exp):
    left: Exp
    right: Exp


# --- L-value objects ---
class Obj:
    pass

@dataclass(frozen=True)
class VarArray(Obj):
    name: Id
    index: Optional[Exp] = None

@dataclass(frozen=True)
class InstVar(Obj):
    obj: Obj
    field: Obj


# --- Method call arguments ---
class Arg:
    pass

@dataclass(frozen=True)
class IdArg(Arg):
    name: Id

@dataclass(frozen=True)
class ExpArg(Arg):
    exp: Exp


# --- Switch case/esac markers ---
class Case(Enum):
    Case = auto()
    NoCase = auto()

class Esac(Enum):
    Esac = auto()
    NoEsac = auto()

class Break(Enum):
    Break = auto()
    NoBreak = auto()


# --- Statements ---
class Stm:
    pass


@dataclass(frozen=True)
class Positioned(Stm):
    """位置情報つきの文（構文解析器が付ける）。"""
    pos: Pos
    stm: Stm

@dataclass(frozen=True)
class Skip(Stm):
    pass

@dataclass(frozen=True)
class Assign(Stm):
    obj: Obj
    op: ModOp
    exp: Exp

@dataclass(frozen=True)
class Swap(Stm):
    left: Obj
    right: Obj

@dataclass(frozen=True)
class Conditional(Stm):
    test: Exp
    then_branch: list[Stm]
    else_branch: list[Stm]
    fi: Exp

@dataclass(frozen=True)
class Loop(Stm):
    from_exp: Exp
    do_body: list[Stm]
    loop_body: list[Stm]
    until: Exp

@dataclass(frozen=True)
class For(Stm):
    var: Id
    start: Exp
    end: Exp
    body: list[Stm]

# Switch case entry: ((case_marker, exps), stms, (esac_marker, exps, break_marker))
CaseEntry = tuple[tuple[Case, list[Exp]], list[Stm], tuple[Esac, list[Exp], Break]]

@dataclass(frozen=True)
class Switch(Stm):
    obj1: Obj
    cases: list[CaseEntry]
    default: list[Stm]
    obj2: Obj

@dataclass(frozen=True)
class ObjectBlock(Stm):
    type_id: TypeId
    name: Id
    body: list[Stm]

@dataclass(frozen=True)
class LocalBlock(Stm):
    dtype: DataType
    name: Id
    init: Exp
    body: list[Stm]
    final: Exp

@dataclass(frozen=True)
class LocalCall(Stm):
    method: MethodId
    args: list[Arg]

@dataclass(frozen=True)
class LocalUncall(Stm):
    method: MethodId
    args: list[Arg]

@dataclass(frozen=True)
class ObjectCall(Stm):
    obj: Obj
    method: MethodId
    args: list[Arg]

@dataclass(frozen=True)
class ObjectUncall(Stm):
    obj: Obj
    method: MethodId
    args: list[Arg]

@dataclass(frozen=True)
class ObjectConstruction(Stm):
    type_id: TypeId
    obj: Obj

@dataclass(frozen=True)
class ObjectDestruction(Stm):
    type_id: TypeId
    obj: Obj

@dataclass(frozen=True)
class CopyReference(Stm):
    dtype: DataType
    obj1: Obj
    obj2: Obj

@dataclass(frozen=True)
class UncopyReference(Stm):
    dtype: DataType
    obj1: Obj
    obj2: Obj

@dataclass(frozen=True)
class ArrayConstruction(Stm):
    type_id: TypeId
    size: Exp
    obj: Obj

@dataclass(frozen=True)
class ArrayDestruction(Stm):
    type_id: TypeId
    size: Exp
    obj: Obj

@dataclass(frozen=True)
class Show(Stm):
    exp: Exp

@dataclass(frozen=True)
class Print(Stm):
    text: str


# --- Declarations ---
@dataclass(frozen=True)
class Decl:
    dtype: DataType
    name: Id


# --- Method declaration ---
@dataclass(frozen=True)
class MDecl:
    name: MethodId
    params: list[Decl]
    body: list[Stm]


# --- Class declaration ---
@dataclass(frozen=True)
class CDecl:
    name: TypeId
    inherits: Optional[TypeId]
    fields: list[Decl]
    methods: list[MDecl]


# --- Program ---
@dataclass(frozen=True)
class Prog:
    classes: list[CDecl]


# --- 位置情報の除去 -------------------------------------------------------

def strip_epos(e: Exp) -> Exp:
    """式の位置情報の殻を剥がす。"""
    while isinstance(e, EPos):
        e = e.exp
    return e


def erase_pos_exp(e: Exp) -> Exp:
    e = strip_epos(e)
    if isinstance(e, ArrayElement):
        return ArrayElement(e.name, erase_pos_exp(e.index))
    if isinstance(e, Binary):
        return Binary(e.op, erase_pos_exp(e.left), erase_pos_exp(e.right))
    if isinstance(e, Dot):
        return Dot(erase_pos_exp(e.left), erase_pos_exp(e.right))
    return e


def erase_pos_obj(o):
    if isinstance(o, VarArray):
        return VarArray(o.name, None if o.index is None else erase_pos_exp(o.index))
    if isinstance(o, InstVar):
        return InstVar(erase_pos_obj(o.obj), erase_pos_obj(o.field))
    return o


def erase_pos_arg(a):
    return ExpArg(erase_pos_exp(a.exp)) if isinstance(a, ExpArg) else a


def strip_pos(s: Stm) -> Stm:
    """位置情報の殻を剥がす（いちばん外側だけ）。"""
    while isinstance(s, Positioned):
        s = s.stm
    return s


def pos_of(s: Stm) -> Optional[Pos]:
    """いちばん外側の位置情報を取り出す。"""
    return s.pos if isinstance(s, Positioned) else None


def erase_pos_stm(s: Stm) -> Stm:
    """位置情報をすべて取り除く。AST を構造として比べるときに使う。"""
    s = strip_pos(s)
    if isinstance(s, Assign):
        return Assign(erase_pos_obj(s.obj), s.op, erase_pos_exp(s.exp))
    if isinstance(s, Swap):
        return Swap(erase_pos_obj(s.left), erase_pos_obj(s.right))
    if isinstance(s, Conditional):
        return Conditional(erase_pos_exp(s.test), erase_pos_stms(s.then_branch),
                           erase_pos_stms(s.else_branch), erase_pos_exp(s.fi))
    if isinstance(s, Loop):
        return Loop(erase_pos_exp(s.from_exp), erase_pos_stms(s.do_body),
                    erase_pos_stms(s.loop_body), erase_pos_exp(s.until))
    if isinstance(s, For):
        return For(s.var, erase_pos_exp(s.start), erase_pos_exp(s.end),
                   erase_pos_stms(s.body))
    if isinstance(s, Switch):
        return Switch(erase_pos_obj(s.obj1),
                      [((c, [erase_pos_exp(e) for e in es1]),
                        erase_pos_stms(body),
                        (p, [erase_pos_exp(e) for e in es2], b))
                       for ((c, es1), body, (p, es2, b)) in s.cases],
                      erase_pos_stms(s.default), erase_pos_obj(s.obj2))
    if isinstance(s, ObjectBlock):
        return ObjectBlock(s.type_id, s.name, erase_pos_stms(s.body))
    if isinstance(s, LocalBlock):
        return LocalBlock(s.dtype, s.name, erase_pos_exp(s.init),
                          erase_pos_stms(s.body), erase_pos_exp(s.final))
    if isinstance(s, LocalCall):
        return LocalCall(s.method, [erase_pos_arg(a) for a in s.args])
    if isinstance(s, LocalUncall):
        return LocalUncall(s.method, [erase_pos_arg(a) for a in s.args])
    if isinstance(s, ObjectCall):
        return ObjectCall(erase_pos_obj(s.obj), s.method,
                          [erase_pos_arg(a) for a in s.args])
    if isinstance(s, ObjectUncall):
        return ObjectUncall(erase_pos_obj(s.obj), s.method,
                            [erase_pos_arg(a) for a in s.args])
    if isinstance(s, ObjectConstruction):
        return ObjectConstruction(s.type_id, erase_pos_obj(s.obj))
    if isinstance(s, ObjectDestruction):
        return ObjectDestruction(s.type_id, erase_pos_obj(s.obj))
    if isinstance(s, CopyReference):
        return CopyReference(s.dtype, erase_pos_obj(s.obj1), erase_pos_obj(s.obj2))
    if isinstance(s, UncopyReference):
        return UncopyReference(s.dtype, erase_pos_obj(s.obj1), erase_pos_obj(s.obj2))
    if isinstance(s, ArrayConstruction):
        return ArrayConstruction(s.type_id, erase_pos_exp(s.size),
                                 erase_pos_obj(s.obj))
    if isinstance(s, ArrayDestruction):
        return ArrayDestruction(s.type_id, erase_pos_exp(s.size),
                                erase_pos_obj(s.obj))
    if isinstance(s, Show):
        return Show(erase_pos_exp(s.exp))
    return s


def erase_pos_stms(l: list[Stm]) -> list[Stm]:
    return [erase_pos_stm(s) for s in l]


def erase_pos_prog(p: Prog) -> Prog:
    return Prog([
        CDecl(c.name, c.inherits, c.fields,
              [MDecl(m.name, m.params, erase_pos_stms(m.body)) for m in c.methods])
        for c in p.classes
    ])
