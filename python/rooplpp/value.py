"""Runtime value and environment types (from value.ml)."""
from __future__ import annotations
from dataclasses import dataclass
from .syntax import Id, TypeId

# Location type
Locs = int

# Environment: variable name -> location
Env = dict[str, Locs]

# --- Values ---
class Value:
    pass

@dataclass(frozen=True)
class IntVal(Value):
    value: int

@dataclass(frozen=True)
class ObjVal(Value):
    type_id: TypeId
    env: Env

@dataclass(frozen=True)
class LocsVal(Value):
    locs: Locs

@dataclass(frozen=True)
class LocsVec(Value):
    locs: list[Locs]

# Store: location -> value
State = dict[Locs, Value]
