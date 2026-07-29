"""Runtime value printer (from print.ml)."""
from .value import *


def show_vec(vec: list[Locs]) -> str:
    return "[" + "".join(str(l) for l in vec) + "]"


def show_val(v: Value) -> str:
    match v:
        case IntVal(n): return str(n)
        case LocsVal(l): return f"<location> {l}"
        case LocsVec(vec): return show_vec(vec)
        case _: raise RuntimeError("error in print_val")


def print_result(result: list[tuple[str, Value]]) -> None:
    for name, v in result:
        print(f"{name} = {show_val(v)}")


def show_val_rec(v: Value) -> str:
    match v:
        case IntVal(n): return f"<int> {n}"
        case ObjVal(x, ss):
            fields = "".join(f"; {s}:{n}" for s, n in ss.items())
            return f"<object> {x}{{{fields}}}"
        case LocsVal(n): return f"<location> {n}"
        case LocsVec(ls):
            nums = " ".join(f" {n}" for n in ls)
            return f"<vector>{nums}"


def print_value_rec(v: Value) -> None:
    print(show_val_rec(v), end="")
