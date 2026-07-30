"""Pretty printer: AST to source code string (from pretty.ml)."""
from .syntax import *


def indent(n: int) -> str:
    return "    " * n


def pretty_datatype(dt: DataType) -> str:
    match dt:
        case IntegerType(): return "int"
        case ObjectType(tid): return tid
        case CopyType(tid): return tid
        case ObjectArrayType(tid): return tid + "[]"
        case IntegerArrayType(): return "int[]"
        case ArrayType() | ArrayElementType() | NilType(): return ""


def pretty_binop(op: BinOp) -> str:
    return {
        BinOp.Add: "+", BinOp.Sub: "-", BinOp.Xor: "^",
        BinOp.Mul: "*", BinOp.Div: "/", BinOp.Mod: "%",
        BinOp.Band: "&", BinOp.Bor: "|",
        BinOp.And: "&&", BinOp.Or: "||",
        BinOp.Lt: "<", BinOp.Gt: ">",
        BinOp.Eq: "=", BinOp.Ne: "!=",
        BinOp.Le: "<=", BinOp.Ge: ">=",
    }[op]


def pretty_modop(op: ModOp) -> str:
    return {ModOp.ModAdd: "+=", ModOp.ModSub: "-=", ModOp.ModXor: "^="}[op]


def pretty_exp(exp: Exp) -> str:
    match exp:
        case Const(n): return str(n)
        case Var(name): return name
        case ArrayElement(name, idx): return f"{name}[{pretty_exp(idx)}]"
        case Nil(): return "nil"
        case Binary(op, e1, e2):
            # 被演算子が二項演算なら括弧で囲む（pretty.ml の paren_binary と同じ）。
            # -inverse の出力を再パースできるようにするための括弧付け。
            return f"{_paren_binary(e1)} {pretty_binop(op)} {_paren_binary(e2)}"
        case Dot(e1, e2):
            return f"{pretty_exp(e1)}.{pretty_exp(e2)}"


def _paren_binary(exp: Exp) -> str:
    """二項演算の被演算子だけ括弧で囲む（添字 e[..] や Dot は不要）。"""
    if isinstance(exp, Binary):
        return "(" + pretty_exp(exp) + ")"
    return pretty_exp(exp)


def pretty_obj(obj: Obj) -> str:
    match obj:
        case VarArray(name, None): return name
        case VarArray(name, idx): return f"{name}[{pretty_exp(idx)}]"
        case InstVar(o1, o2): return f"{pretty_obj(o1)}.{pretty_obj(o2)}"


def pretty_actargs(args: list[Arg]) -> str:
    def pretty_actarg(a: Arg) -> str:
        match a:
            case IdArg(name): return name
            case ExpArg(e): return pretty_exp(e)
    return ", ".join(pretty_actarg(a) for a in args)


def pretty_stms(stms: list[Stm], n: int) -> str:
    return "".join(indent(n) + pretty_stm(s, n) + "\n" for s in stms)


def pretty_stm(stm: Stm, n: int) -> str:
    match stm:
        # 位置情報は出力しない（-inverse の出力は再パースされるため）
        case Positioned(_, s0):
            return pretty_stm(s0, n)
        case Assign(obj, op, exp):
            return f"{pretty_obj(obj)} {pretty_modop(op)} {pretty_exp(exp)}"
        case Swap(o1, o2):
            return f"{pretty_obj(o1)} <=> {pretty_obj(o2)}"
        case Conditional(e1, s1, s2, e2):
            return (f"if {pretty_exp(e1)} then\n"
                    + pretty_stms(s1, n + 1)
                    + indent(n) + "else\n"
                    + pretty_stms(s2, n + 1)
                    + indent(n) + f"fi {pretty_exp(e2)}")
        case Loop(e1, s1, s2, e2):
            return (f"from {pretty_exp(e1)} do\n"
                    + pretty_stms(s1, n + 1)
                    + indent(n) + "loop\n"
                    + pretty_stms(s2, n + 1)
                    + indent(n) + f"until {pretty_exp(e2)}")
        case For(var, e1, e2, body):
            return (f"for {var} in ({pretty_exp(e1)}..{pretty_exp(e2)}) do\n"
                    + pretty_stms(body, n + 1)
                    + indent(n) + "end")
        case Switch(obj1, cases, default, obj2):
            def pretty_case_marker(c):
                return "case" if c == Case.Case else ""
            def pretty_esac_marker(e):
                return "esac" if e == Esac.Esac else ""
            def pretty_exps(exps):
                return ":".join(pretty_exp(e) for e in exps)
            def pretty_break(b):
                return "break " if b == Break.Break else ""
            parts = ""
            for (c1, exps1), s, (c2, exps2, b) in cases:
                parts += (pretty_case_marker(c1) + " " + pretty_exps(exps1) + "\n"
                         + pretty_stms(s, n + 1)
                         + indent(n + 1) + pretty_esac_marker(c2) + " " + pretty_exps(exps2) + "\n"
                         + indent(n + 1) + pretty_break(b) + "\n"
                         + indent(n))
            return (f"switch {pretty_obj(obj1)}\n"
                    + indent(n) + parts
                    + "default\n"
                    + pretty_stms(default, n + 1)
                    + indent(n + 1) + "break\n"
                    + indent(n) + f"hctiws {pretty_obj(obj2)}")
        case ObjectBlock(tid, name, body):
            return (f"construct {tid} {name}\n"
                    + pretty_stms(body, n + 1) + "\n"
                    + indent(n) + f"destruct {name}")
        case LocalBlock(dt, name, e1, body, e2):
            return (f"local {pretty_datatype(dt)} {name} = {pretty_exp(e1)}\n"
                    + pretty_stms(body, n)
                    + indent(n) + f"delocal {pretty_datatype(dt)} {name} = {pretty_exp(e2)}")
        case LocalCall(mid, args):
            return f"call {mid}({pretty_actargs(args)})"
        case LocalUncall(mid, args):
            return f"uncall {mid}({pretty_actargs(args)})"
        case ObjectCall(obj, mid, args):
            return f"call {pretty_obj(obj)}::{mid}({pretty_actargs(args)})"
        case ObjectUncall(obj, mid, args):
            return f"uncall {pretty_obj(obj)}::{mid}({pretty_actargs(args)})"
        case ObjectConstruction(tid, obj):
            return f"new {tid} {pretty_obj(obj)}"
        case ObjectDestruction(tid, obj):
            return f"delete {tid} {pretty_obj(obj)}"
        case CopyReference(dt, o1, o2):
            return f"copy {pretty_datatype(dt)} {pretty_obj(o1)} {pretty_obj(o2)}"
        case UncopyReference(dt, o1, o2):
            return f"uncopy {pretty_datatype(dt)} {pretty_obj(o1)} {pretty_obj(o2)}"
        case ArrayConstruction(tid, exp, obj):
            return f"new {tid}[{pretty_exp(exp)}] {pretty_obj(obj)}"
        case ArrayDestruction(tid, exp, obj):
            return f"delete {tid}[{pretty_exp(exp)}] {pretty_obj(obj)}"
        case Skip():
            return "skip"
        case Show(exp):
            return f"show({pretty_exp(exp)})"
        case Print(text):
            return f'print("{_escape(text)}")'


def _escape(s: str) -> str:
    return s.replace("\\", "\\\\").replace('"', '\\"').replace("\n", "\\n").replace("\t", "\\t")


def pretty_decl(d: Decl) -> str:
    return f"{pretty_datatype(d.dtype)} {d.name}"


def pretty_fields(fields: list[Decl]) -> str:
    # OCaml の pretty_fields は各フィールドの後ろに "\n    " を付ける
    # （フィールドが 1 つでもあると空行が 1 行入る）。差分テストで一致させるため
    # ここも同じ形にしている。
    return "".join(pretty_decl(f) + "\n    " for f in fields)


def pretty_args(params: list[Decl]) -> str:
    return ", ".join(pretty_decl(p) for p in params)


def pretty_method(m: MDecl) -> str:
    return f"method {m.name}({pretty_args(m.params)})\n" + pretty_stms(m.body, 2)


def pretty_methods(methods: list[MDecl]) -> str:
    return "\n    ".join(pretty_method(m) for m in methods)


def pretty_class(c: CDecl) -> str:
    inher = f" inherits {c.inherits}" if c.inherits else ""
    return (f"class {c.name}{inher}\n    "
            + pretty_fields(c.fields) + "\n    "
            + pretty_methods(c.methods))


def pretty_prog(prog: Prog) -> str:
    return "\n".join(pretty_class(c) for c in prog.classes) + "\n"
