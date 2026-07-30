"""Program inverter: reverses execution semantics (from invert.ml)."""
from .syntax import *


def invert_stm(stm: Stm) -> Stm:
    match stm:
        # 位置は反転しても同じ文を指すので、そのまま持ち越す
        case Positioned(p, s0):
            return Positioned(p, invert_stm(s0))
        case Skip():
            return Skip()
        case Assign(obj, op, exp):
            inv = {ModOp.ModAdd: ModOp.ModSub,
                   ModOp.ModSub: ModOp.ModAdd,
                   ModOp.ModXor: ModOp.ModXor}[op]
            return Assign(obj, inv, exp)
        case Swap(o1, o2):
            return Swap(o1, o2)
        case Conditional(e1, s1, s2, e2):
            return Conditional(e2, invert(s1), invert(s2), e1)
        case Loop(e1, s1, s2, e2):
            return Loop(e2, invert(s1), invert(s2), e1)
        case For(x, e1, e2, body):
            return For(x, e2, e1, invert(body))
        case Switch(obj1, cases, default, obj2):
            inv_cases = [((c1, e2), invert(s), (c2, e1, b))
                         for (c1, e1), s, (c2, e2, b) in cases]
            return Switch(obj2, _invert_cases(inv_cases), invert(default), obj1)
        case ObjectBlock(tid, name, body):
            return ObjectBlock(tid, name, invert(body))
        case LocalBlock(dt, name, e1, body, e2):
            return LocalBlock(dt, name, e2, invert(body), e1)
        case LocalCall(mid, args):
            return LocalUncall(mid, args)
        case LocalUncall(mid, args):
            return LocalCall(mid, args)
        case ObjectCall(obj, mid, args):
            return ObjectUncall(obj, mid, args)
        case ObjectUncall(obj, mid, args):
            return ObjectCall(obj, mid, args)
        case ObjectConstruction(tid, obj):
            return ObjectDestruction(tid, obj)
        case ObjectDestruction(tid, obj):
            return ObjectConstruction(tid, obj)
        case CopyReference(dt, o1, o2):
            return UncopyReference(dt, o1, o2)
        case UncopyReference(dt, o1, o2):
            return CopyReference(dt, o1, o2)
        case ArrayConstruction(tid, size, obj):
            return ArrayDestruction(tid, size, obj)
        case ArrayDestruction(tid, size, obj):
            return ArrayConstruction(tid, size, obj)
        case Show() | Print():
            return stm


def invert(stml: list[Stm]) -> list[Stm]:
    return [invert_stm(s) for s in reversed(stml)]


def invert_method(m: MDecl) -> MDecl:
    return MDecl(m.name, m.params, invert(m.body))


def _invert_cases(cases: list) -> list:
    """Invert switch case structure (handles break/fall-through rewriting)."""
    if len(cases) <= 1:
        return cases
    (c1, e1), s, (c2, e2, b) = cases[0]
    if b == Break.Break:
        return [cases[0]] + _invert_cases(cases[1:])
    # Collect cases up to and including next Break
    append_k = []
    rest = cases
    for i, ((_, _), _, (_, _, bi)) in enumerate(cases):
        append_k.append(cases[i])
        if bi == Break.Break:
            break
    # Cases after the break group
    search_kn = []
    found_break = False
    for ((_, _), _, (_, _, bi)) in cases:
        if found_break:
            search_kn.append(((_, _), _, (_, _, bi)))
        if bi == Break.Break:
            found_break = True
    # Actually need to recompute from the original list properly
    search_kn = []
    found = False
    for entry in cases:
        if found:
            search_kn.append(entry)
        (_, _), _, (_, _, bi) = entry
        if bi == Break.Break and not found:
            found = True

    def invert_cases2(cs):
        cs2 = []
        for (c1, e1), s, (c2, e2, b) in cs:
            if c1 == Case.Case and c2 == Esac.Esac:
                cs2.append(((c1, e1), s, (c2, e2, b)))
            elif c1 == Case.Case and c2 == Esac.NoEsac:
                cs2.append(((Case.NoCase, e1), s, (Esac.Esac, e2, b)))
            else:
                cs2.append(((Case.Case, e1), s, (Esac.NoEsac, e2, b)))
        # Reverse and fix break markers
        if cs2:
            (c1, e1), s, (c2, e2, b) = cs2[0]
            cs2[0] = ((c1, e1), s, (c2, e2, Break.Break))
            cs2 = list(reversed(cs2))
            (c1, e1), s, (c2, e2, b) = cs2[0]
            cs2[0] = ((c1, e1), s, (c2, e2, Break.NoBreak))
        return cs2

    return invert_cases2(append_k) + _invert_cases(search_kn)


def invert_prog(prog: Prog) -> Prog:
    inv_classes = []
    for c in prog.classes:
        inv_methods = [invert_method(m) for m in c.methods]
        inv_classes.append(CDecl(c.name, c.inherits, c.fields, inv_methods))
    return Prog(inv_classes)
