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
class Exp:
    pass

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
