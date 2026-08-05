"""Evaluator: expressions, statements, programs (from eval.ml)."""
from __future__ import annotations
from .syntax import *
from .value import *
from .pretty import pretty_exp, pretty_stms, pretty_stm
from .invert import invert
from . import printer as Printer
from . import diagnostics as Diag


class ExprError(RuntimeError):
    """式の中で起きた失敗。位置 (行, 列, 終了行, 終了列) を運ぶ（Util.Expr_error）。

    いちばん内側の位置つき式のラッパが付け、外側はメッセージだけを積み増して
    位置は保つ。文のラッパがこれを受け取って StmError に格上げする。
    """

    def __init__(self, span: tuple[int, int, int, int], msg: str):
        self.span = span
        super().__init__(msg)


class StmError(RuntimeError):
    """すでに「どの文で起きたか」が付いている実行時エラー（Util.Runtime_error に対応）。

    文脈のない失敗は素の RuntimeError で投げ、文を包むラッパがその文の pretty
    表示と WHERE 行を付けて StmError に格上げする。これにより入れ子の文ごとに
    同じ文が積まれるのを防ぐ。
    """


# --- Environment/Store operations ---

def ext_envs(env: Env, x: str, v: Locs) -> Env:
    return {**env, x: v}


def ext_st(st: State, x: Locs, v: Value) -> State:
    return {**st, x: v}


def ext_st_zero(st: State, locs: Locs, n: int) -> State:
    for i in range(n):
        st = ext_st(st, locs + i, IntVal(0))
    return st


def lookup_envs(x: str, env: Env) -> Locs:
    if x in env:
        return env[x]
    raise RuntimeError(f"ERROR:unbound variable: {x}")


def lookup_st(x: Locs, st: State) -> Value:
    if x in st:
        return st[x]
    raise RuntimeError(f"ERROR:unbound locations: {x}")


def lookup_val(x: str, env: Env, st: State) -> Value:
    return lookup_st(lookup_envs(x, env), st)


def lookup_vec(index: int, vec: list[Locs]) -> Locs:
    if index < 0:
        raise RuntimeError("ERROR:negative index in lookup_vec")
    if index >= len(vec):
        raise RuntimeError("ERROR:index out of bounds in lookup_vec")
    return vec[index]


# --- Arithmetic helpers ---

def safe_div(n1: int, n2: int) -> int:
    if n2 == 0:
        raise RuntimeError("ERROR:division by zero")
    # Python integer division truncates towards negative infinity;
    # OCaml truncates towards zero. Use int() to match OCaml behavior.
    return int(n1 / n2)


def safe_mod(n1: int, n2: int) -> int:
    if n2 == 0:
        raise RuntimeError("ERROR:modulo by zero")
    # Match OCaml mod semantics (sign follows dividend)
    return n1 - (int(n1 / n2)) * n2


def bin_op(f, v1: Value, v2: Value) -> Value:
    match v1, v2:
        case IntVal(n1), IntVal(n2):
            return IntVal(f(n1, n2))
        case _:
            raise RuntimeError("ERROR:integer values expected")


def rel_op(op, v1: Value, v2: Value) -> Value:
    match v1, v2:
        case IntVal(n1), IntVal(n2):
            return IntVal(1 if op(n1 != 0, n2 != 0) else 0)
        case _:
            raise RuntimeError("ERROR:integer values expected")


def comp_op_int(f, v1: Value, v2: Value) -> Value:
    """Comparison that extracts int values (for <, >, <=, >=)."""
    match v1, v2:
        case IntVal(n1), IntVal(n2):
            return IntVal(1 if f(n1, n2) else 0)
        case _:
            raise RuntimeError("ERROR:integer values expected")


def comp_op_eq(f, v1: Value, v2: Value) -> Value:
    """Equality comparison using structural equality (for =, !=)."""
    return IntVal(1 if f(v1, v2) else 0)


# --- Expression evaluation ---

def eval_exp(exp: Exp, env: Env, st: State) -> Value:
    def lval_val(y: Exp, env: Env, ienv: Env | None = None):
        """l 値のロケーションと値を返す。

        ienv は**添字の式**を評価する環境。名前の解決 env とは別に持ち回る:
        ドットの右側（o.xs[i]）ではフィールド名 xs をオブジェクト側の環境で
        引くが、添字 i は呼び出し側のスコープの変数である。両方を env2 にすると
        o.xs[k] の k がオブジェクトのフィールド k を指してしまう。"""
        if ienv is None:
            ienv = env
        match strip_epos(y):
            case Var(x):
                lv = lookup_envs(x, env)
                return lv, lookup_st(lv, st)
            case ArrayElement(x, e):
                x_index = match_int(eval_exp(e, ienv, st), "array index must be an integer")
                locsvecx = match_locsvec(lookup_val(x, env, st))
                if not (0 <= x_index < len(locsvecx)):
                    raise RuntimeError(
                        f"ERROR:Array index {x}[{x_index}] is out of bounds in this statement")
                locsx = x_index + locsvecx[0]
                return locsx, lookup_st(locsx, st)
            case Dot(x, xi):
                _, locs = lval_val(x, env, ienv)
                match locs:
                    case LocsVal(l):
                        match lookup_st(l, st):
                            case ObjVal(_, env2):
                                # 名前は env2 で引くが、添字は ienv のまま
                                return lval_val(xi, env2, ienv)
                            case _:
                                raise RuntimeError("ERROR:expected object value for dot access")
                    case _:
                        raise RuntimeError("ERROR:expected location value for dot access")
            case _:
                raise RuntimeError("ERROR:not an l-value expression")

    match exp:
        # 位置つきの式：中で落ちたらいちばん内側の位置を例外に載せる
        case EPos(p, e):
            try:
                return eval_exp(e, env, st)
            except ExprError:
                raise
            except RuntimeError as err:
                raise ExprError((p.line, p.col, p.end_line, p.end_col),
                                str(err)) from None
        case Const(n):
            return IntVal(n)
        case Var(x):
            return lookup_st(lookup_envs(x, env), st)
        case ArrayElement(id, e):
            index = match_int(eval_exp(e, env, st), "array index must be an integer")
            locs = lookup_envs(id, env)
            lv = match_locsvec(lookup_st(locs, st))
            if index >= 0 and index < len(lv):
                locs2 = index + lv[0]
            else:
                raise RuntimeError(
                    pretty_exp(exp) + f"\nERROR:Array index {id}[{index}] is out of bounds in this statement")
            return lookup_st(locs2, st)
        case Nil():
            return IntVal(0)
        case Dot(x, xi):
            _, v = lval_val(Dot(x, xi), env)
            return v
        case Binary(b, e1, e2):
            ops = {
                BinOp.Add: lambda: bin_op(lambda a, b: a + b, eval_exp(e1, env, st), eval_exp(e2, env, st)),
                BinOp.Sub: lambda: bin_op(lambda a, b: a - b, eval_exp(e1, env, st), eval_exp(e2, env, st)),
                BinOp.Xor: lambda: bin_op(lambda a, b: a ^ b, eval_exp(e1, env, st), eval_exp(e2, env, st)),
                BinOp.Mul: lambda: bin_op(lambda a, b: a * b, eval_exp(e1, env, st), eval_exp(e2, env, st)),
                BinOp.Div: lambda: bin_op(safe_div, eval_exp(e1, env, st), eval_exp(e2, env, st)),
                BinOp.Mod: lambda: bin_op(safe_mod, eval_exp(e1, env, st), eval_exp(e2, env, st)),
                BinOp.Band: lambda: bin_op(lambda a, b: a & b, eval_exp(e1, env, st), eval_exp(e2, env, st)),
                BinOp.Bor: lambda: bin_op(lambda a, b: a | b, eval_exp(e1, env, st), eval_exp(e2, env, st)),
                BinOp.And: lambda: rel_op(lambda a, b: a and b, eval_exp(e1, env, st), eval_exp(e2, env, st)),
                BinOp.Or: lambda: rel_op(lambda a, b: a or b, eval_exp(e1, env, st), eval_exp(e2, env, st)),
                BinOp.Lt: lambda: comp_op_int(lambda a, b: a < b, eval_exp(e1, env, st), eval_exp(e2, env, st)),
                BinOp.Gt: lambda: comp_op_int(lambda a, b: a > b, eval_exp(e1, env, st), eval_exp(e2, env, st)),
                BinOp.Eq: lambda: comp_op_eq(lambda a, b: a == b, eval_exp(e1, env, st), eval_exp(e2, env, st)),
                BinOp.Ne: lambda: comp_op_eq(lambda a, b: a != b, eval_exp(e1, env, st), eval_exp(e2, env, st)),
                BinOp.Le: lambda: comp_op_int(lambda a, b: a <= b, eval_exp(e1, env, st), eval_exp(e2, env, st)),
                BinOp.Ge: lambda: comp_op_int(lambda a, b: a >= b, eval_exp(e1, env, st), eval_exp(e2, env, st)),
            }
            try:
                return ops[b]()
            except ExprError as e:
                # 位置は内側のものを保ち、メッセージだけ積み増す
                raise ExprError(
                    e.span,
                    pretty_exp(exp) + "\n" + str(e) + " in this expression") from None
            except RuntimeError as e:
                raise RuntimeError(pretty_exp(exp) + "\n" + str(e) + " in this expression")


# --- Helper functions ---

def free_vars(e: Exp) -> list[Id]:
    """式に自由に現れる変数名（いまの環境で解決されるものだけ）。

    ドットの右側はオブジェクト側の環境で解決されるので数えない。"""
    match e:
        case Const(_) | Nil():
            return []
        case Var(x):
            return [x]
        case ArrayElement(x, idx):
            return [x] + free_vars(idx)
        case Binary(_, e1, e2):
            return free_vars(e1) + free_vars(e2)
        case Dot(x, _):
            return free_vars(x)
        case EPos(_, e1):
            return free_vars(e1)
        case _:
            return []


def obj_has_index(y: Obj) -> bool:
    """l 値が添字を含むか。含むなら書き込みでロケーションが動きうる。"""
    match y:
        case VarArray(_, None):
            return False
        case VarArray(_, _):
            return True
        case InstVar(x, xi):
            return obj_has_index(x) or obj_has_index(xi)
        case _:
            return False


def match_int(v: Value, msg: str = "") -> int:
    match v:
        case IntVal(n): return n
        case _: raise RuntimeError(f"ERROR:{msg}" if msg else "ERROR:integer expected")


def match_locsvec(v: Value) -> list[Locs]:
    match v:
        case LocsVec(vec): return vec
        case _: raise RuntimeError("ERROR:expected array value")


def gen_locsvec(n: int, locs: Locs) -> list[Locs]:
    return list(range(locs, locs + n))


def max_locs(st: State) -> int:
    return max(st.keys()) if st else 0


def search_a(args: list[Arg], env: Env, st: State, locs: Locs) -> list[Locs]:
    result = []
    for arg in args:
        match arg:
            case IdArg(id):
                result.append(lookup_envs(id, env))
            case ExpArg(_):
                locs += 1
                result.append(locs)
        locs += 1
    return result


def argv(arg: Arg, env: Env, st: State) -> Value:
    match arg:
        case IdArg(id): return lookup_val(id, env, st)
        case ExpArg(e): return eval_exp(e, env, st)


def remove_a(argl: list[Arg], locsl: list[Locs], vl: list[Value], st: State) -> State:
    for arg, locs, v in zip(argl, locsl, vl):
        match arg:
            case IdArg(_):
                pass
            case ExpArg(_):
                if lookup_st(locs, st) == v:
                    st = {k: val for k, val in st.items() if k != locs}
                else:
                    raise RuntimeError("ERROR: formal argument and actual argument are not same value in this statement")
    return st


def is_field_zero(st: State, locs: Locs, n: int) -> bool:
    for i in range(n):
        if lookup_st(locs + i, st) != IntVal(0):
            return False
    return True


def lookup_meth(x: str, vl: list[Value], meth: list[MDecl]) -> MDecl:
    for m in meth:
        if m.name == x and len(vl) == len(m.params):
            return m
    raise RuntimeError(f"ERROR: Method {x} does not exist or wrong number of arguments for the function")


def lookup_map(id: str, map_: list) -> tuple:
    for cid, fm in map_:
        if cid == id:
            return fm
    raise RuntimeError(f"ERROR:class {id} is not valid")


def ext_env_field(fields: list[Decl], n: int) -> Env:
    env = {}
    for f in fields:
        env[f.name] = n
        n += 1
    return env


# --- Statement evaluation ---

def eval_state(stml: list[Stm], env: Env, map_: list, st: State) -> State:
    for stm0 in stml:
        # 位置情報は診断専用なので、意味論に渡す前に剥がす
        stm = strip_pos(stm0)
        p = pos_of(stm0)
        at = Diag.at_line(p.line) if p is not None else ""
        try:
            st = _update(stm, env, map_, st)
        # 式の中で落ちた場合は、文ではなく式の範囲を位置として使う
        except ExprError as e:
            raise StmError(
                pretty_stms([stm], 0) + "\n" + str(e)
                + Diag.where_line(stm, env, st) + Diag.at_span(e.span)
            ) from None
        except StmError as e:
            # すでに文が付いている。いちばん内側のラッパだけが値と位置を足す。
            if Diag.has_where(str(e)):
                raise
            raise StmError(str(e) + Diag.where_line(stm, env, st) + at) from None
        except RuntimeError as e:
            raise StmError(
                pretty_stms([stm], 0) + "\n" + str(e)
                + Diag.where_line(stm, env, st) + at
            ) from None
    return st


def _update(stm: Stm, env: Env, map_: list, st: State) -> State:
    def isTrue(v: Value) -> bool:
        match v:
            case IntVal(0): return False
            case IntVal(_): return True
            case _: raise RuntimeError("ERROR:Integer value expected in the condition of this statement")

    def isFalse(v: Value) -> bool:
        return not isTrue(v)

    def f_modop(op: ModOp):
        return {
            ModOp.ModAdd: lambda a, b: a + b,
            ModOp.ModSub: lambda a, b: a - b,
            ModOp.ModXor: lambda a, b: a ^ b,
        }[op]

    def lval_val_in(st_: State, y: Obj, env: Env,
                    ienv: Env | None = None) -> tuple[Locs, Value]:
        """l 値のロケーションと値を返す。ストアを引数に取るのは、書き込んだ後の
        状態でもう一度解決して「l 値が自分の書き込みで動いていないか」を見るため。
        ienv は添字の式を評価する環境（eval_exp 側の lval_val と同じ理由）。"""
        if ienv is None:
            ienv = env
        match y:
            case VarArray(x, None):
                lv = lookup_envs(x, env)
                return lv, lookup_st(lv, st_)
            case VarArray(x, idx):
                x_index = match_int(eval_exp(idx, ienv, st_), "array index must be an integer")
                locsvecx = match_locsvec(lookup_val(x, env, st_))
                if x_index >= 0 and x_index < len(locsvecx):
                    locsx = x_index + locsvecx[0]
                else:
                    raise RuntimeError(
                        pretty_stms([stm], 0) + f"\nERROR:Array index {x}[{x_index}] is out of bounds in this statement")
                return locsx, lookup_st(locsx, st_)
            case InstVar(x, xi):
                _, locs = lval_val_in(st_, x, env, ienv)
                match locs:
                    case LocsVal(l):
                        match lookup_st(l, st_):
                            case ObjVal(_, env2):
                                # 名前は env2 で引くが、添字は ienv のまま
                                return lval_val_in(st_, xi, env2, ienv)
                            case _:
                                raise RuntimeError("ERROR:expected object value for instance variable access")
                    case _:
                        raise RuntimeError("ERROR:expected location value for instance variable access")

    def lval_val(y: Obj, env: Env) -> tuple[Locs, Value]:
        return lval_val_in(st, y, env)

    def check_locs_stable(st_: State, targets: list[tuple[Obj, Locs]]) -> None:
        """書き込みで l 値のロケーションが動いていないことを確認する。

        coq/roopl.v の E_aassign / E_aswap の前提「eval ei b = eval ei a」
        （添字が自分の書き込みで変わらない）にあたる。添字を含まない l 値は
        動きようがないので調べない。"""
        for y, lv in targets:
            if obj_has_index(y):
                lv2, _ = lval_val_in(st_, y, env)
                if lv2 != lv:
                    raise StmError(
                        pretty_stms([stm], 0)
                        + "\nERROR:The array index of this statement must not be"
                          " changed by the statement itself")

    def mycall(locs: Locs, locs2: Locs, invert_flag: int) -> State:
        nonlocal st
        match stm:
            case LocalCall(mid0, args) | LocalUncall(mid0, args) | ObjectCall(_, mid0, args) | ObjectUncall(_, mid0, args):
                vl = [argv(a, env, st) for a in args]
                st_val = lookup_st(locs2, st)
                match st_val:
                    case ObjVal(id, envf):
                        pass
                    case _:
                        raise StmError(pretty_stms([stm], 0) + "\nERROR:expected object value for method call")
                try:
                    fl, meth = lookup_map(id, map_)
                except RuntimeError as e:
                    raise StmError(pretty_stms([stm], 0) + "\n" + str(e) + " in this statement")
                m = lookup_meth(mid0, vl, meth)
                pidl = [p.name for p in m.params]
                ml = max_locs(st)
                arg_locsl = search_a(args, env, st, ml)
                env2 = envf
                for pid, al in zip(pidl, arg_locsl):
                    env2 = ext_envs(env2, pid, al)
                env3 = ext_envs(env2, "this", locs)
                st2 = st
                for al, v in zip(arg_locsl, vl):
                    st2 = ext_st(st2, al, v)
                mstml = invert(m.body) if invert_flag == 1 else m.body
                st3 = eval_state(mstml, env3, map_, st2)
                try:
                    return remove_a(args, arg_locsl, vl, st3)
                except RuntimeError as e:
                    raise StmError(pretty_stms([stm], 0) + "\n" + str(e))
            case _:
                raise RuntimeError("not implemented")

    match stm:
        case Print(text):
            print(text, end="")
            return st

        case Show(e):
            v = eval_exp(e, env, st)
            Printer.print_value_rec(v)
            return st

        case Skip():
            return st

        case Assign(y, op, e):
            lvx, vx = lval_val(y, env)
            v = eval_exp(e, env, st)
            try:
                v2 = bin_op(f_modop(op), vx, v)
            except RuntimeError:
                raise StmError(pretty_stms([stm], 0) + "\nERROR:Integer value expected in this statement")
            st2 = ext_st(st, lvx, v2)
            # 可逆性の副条件。整数変数への代入は構文的に（coq/roopl.v の E_assign の
            # x ∉ fv(e)）、フィールドと配列要素は意味的に（E_fassign / E_aassign の
            # eval e b = eval e a）検査する。後者は別名を構文で近似せずに済む
            match y:
                case VarArray(x, None):
                    if x in free_vars(e):
                        raise StmError(
                            pretty_stms([stm], 0)
                            + f"\nERROR:Variable {x} must not occur on both sides"
                              " of this assignment")
                case _:
                    if eval_exp(e, env, st2) != v:
                        raise StmError(
                            pretty_stms([stm], 0)
                            + "\nERROR:The right-hand side of this assignment must not be"
                              " changed by the assignment itself")
            check_locs_stable(st2, [(y, lvx)])
            return st2

        case Swap(y1, y2):
            lv1, v1 = lval_val(y1, env)
            lv2, v2 = lval_val(y2, env)
            st2 = ext_st(st, lv1, v2)
            st3 = ext_st(st2, lv2, v1)
            check_locs_stable(st3, [(y1, lv1), (y2, lv2)])
            return st3

        case Loop(e1, stml1, stml2, e2):
            def eval_loop(st_):
                if isFalse(eval_exp(e2, env, st_)):
                    st2 = eval_state(stml2, env, map_, st_)
                    if isFalse(eval_exp(e1, env, st2)):
                        st3 = eval_state(stml1, env, map_, st2)
                        return eval_loop(st3)
                    else:
                        raise StmError(pretty_stms([stm], 0) + "\nERROR:Assertion should be false in this statement")
                else:
                    assert isTrue(eval_exp(e2, env, st_)), \
                        pretty_stms([stm], 0) + "\nERROR:assertion is incorrect in this statement"
                    return st_

            if isTrue(eval_exp(e1, env, st)):
                st2 = eval_state(stml1, env, map_, st)
                return eval_loop(st2)
            else:
                raise StmError(pretty_stms([stm], 0) + "\nERROR:Assertion should be true in this statement")

        case For(x, e1, e2, body):
            # 範囲は入口で一度だけ評価し、体がループ変数 x と範囲式 e1/e2 を
            # 変えないことを毎周検査する。この 2 つが可逆性の前提で（逆は
            # for x in (e2..e1) do 逆体 end）、どちらかが崩れると逆向きの実行
            # が同じ道をたどらない。形式化は coq/roopl.v の for_up / for_down。
            # 糖衣が局所ブロックなので、E_local の x ∉ fv(e1), x ∉ fv(e2) が
            # そのまま範囲式にかかる（範囲が x を指すと出口の表明が恒真になる）
            if x in free_vars(e1) or x in free_vars(e2):
                raise StmError(
                    pretty_stms([stm], 0)
                    + "\nERROR:The range of this for statement must not mention"
                      f" the loop variable {x}")

            def int_of(e, st_):
                return match_int(eval_exp(e, env, st_), "for range must be integer")

            n1 = int_of(e1, st)
            n2 = int_of(e2, st)
            d = 1 if n1 < n2 else -1
            l = max_locs(st) + 1
            env2 = ext_envs(env, x, l)

            def for_con(i, st_):
                while True:
                    st2 = ext_st(st_, l, IntVal(i))
                    st3 = eval_state(body, env2, map_, st2)
                    # 体はループ変数を変えてはならない（毎周検査する）
                    if lookup_val(x, env2, st2) != lookup_val(x, env2, st3):
                        raise StmError(pretty_stms([stm], 0) + f"\nERROR:Variable {x} must not change in this for statement")
                    # 体は範囲の値も変えてはならない
                    if int_of(e1, st3) != n1 or int_of(e2, st3) != n2:
                        raise StmError(pretty_stms([stm], 0) + "\nERROR:The range of this for statement must not change in its body")
                    if i == n2:
                        return {k: v for k, v in st3.items() if k != l}
                    i, st_ = i + d, st3

            return for_con(n1, st)

        case Switch(obj1, cases, default_stml, obj2):
            return _eval_switch(stm, obj1, cases, default_stml, obj2, env, map_, st, lval_val, isTrue, isFalse)

        case Conditional(e1, stml1, stml2, e2):
            if isTrue(eval_exp(e1, env, st)):
                st2 = eval_state(stml1, env, map_, st)
                if isTrue(eval_exp(e2, env, st2)):
                    return st2
                raise StmError(pretty_stms([stm], 0) + "\nERROR:Assertion should be true in this statement")
            elif isFalse(eval_exp(e1, env, st)):
                st2 = eval_state(stml2, env, map_, st)
                if isFalse(eval_exp(e2, env, st2)):
                    return st2
                raise StmError(pretty_stms([stm], 0) + "\nERROR:Assertion should be false in this statement")
            raise StmError(pretty_stms([stm], 0) + "\nERROR:Assertion should be false in this statement")

        case LocalCall(mid, args):
            l = lookup_envs("this", env)
            l2 = match_locsval(lookup_st(l, st))
            return mycall(l, l2, 0)

        case LocalUncall(mid, args):
            l = lookup_envs("this", env)
            l2 = match_locsval(lookup_st(l, st))
            return mycall(l, l2, 1)

        case ObjectCall(obj, mid, args):
            l, v = lval_val(obj, env)
            l2 = match_locsval(v)
            return mycall(l, l2, 0)

        case ObjectUncall(obj, mid, args):
            l, v = lval_val(obj, env)
            l2 = match_locsval(v)
            return mycall(l, l2, 1)

        case ObjectBlock(tid, name, body):
            try:
                fl, ml = lookup_map(tid, map_)
            except RuntimeError as e:
                raise StmError(pretty_stms([stm], 0) + "\n" + str(e) + " in this statement")
            ml_ = max_locs(st)
            l = ml_ + 1; l0 = ml_ + 2; l1 = ml_ + 3
            env2 = ext_envs(env, name, l)
            envf = ext_env_field(fl, l1)
            st2 = ext_st_zero(st, l1, len(fl))
            st3 = ext_st(st2, l0, ObjVal(tid, envf))
            st4 = ext_st(st3, l, LocsVal(l0))
            st5 = eval_state(body, env2, map_, st4)
            if is_field_zero(st5, l1, len(fl)):
                return st5
            raise StmError(pretty_stms([stm], 0) + f"\nERROR:{name}'s instance field is not zero-cleared in this statement")

        case ObjectConstruction(tid, obj):
            try:
                fl, ml = lookup_map(tid, map_)
            except RuntimeError as e:
                raise StmError(pretty_stms([stm], 0) + "\n" + str(e) + " in this statement")
            l, v = lval_val(obj, env)
            if v != IntVal(0):
                raise StmError(pretty_stms([stm], 0) + "\nERROR: variable is not nil in this statement")
            ml_ = max_locs(st)
            l0 = ml_ + 1; l1 = ml_ + 2
            envf = ext_env_field(fl, l1)
            st2 = ext_st_zero(st, l1, len(fl))
            st3 = ext_st(st2, l0, ObjVal(tid, envf))
            return ext_st(st3, l, LocsVal(l0))

        case ObjectDestruction(tid, obj):
            try:
                fl, _ = lookup_map(tid, map_)
            except RuntimeError as e:
                raise StmError(pretty_stms([stm], 0) + "\n" + str(e) + " in this statement")
            l, _ = lval_val(obj, env)
            l0 = match_locsval(lookup_st(l, st))
            match lookup_st(l0, st):
                case ObjVal(_, envf):
                    pass
                case _:
                    raise RuntimeError("ERROR:expected object value for destruction")
            l1 = list(envf.values())[0] if envf else 0
            if is_field_zero(st, l1, len(fl)):
                st2 = st
                for i in range(len(fl)):
                    st2 = {k: v for k, v in st2.items() if k != l1 + i}
                st3 = {k: v for k, v in st2.items() if k != l0}
                return ext_st(st3, l, IntVal(0))
            raise StmError(pretty_stms([stm], 0) + "\nERROR:All instance field is not zero-cleared in this statement")

        case ArrayConstruction(tid, e, obj):
            l, v = lval_val(obj, env)
            if v != IntVal(0):
                raise StmError(pretty_stms([stm], 0) + "\nERROR:Variable is not nil in this statement")
            n = match_int(eval_exp(e, env, st), "array size must be integer")
            # 要素数 0 以下は degenerate（OCaml 側は負で無限再帰していた）
            if n < 1:
                raise StmError(
                    pretty_stms([stm], 0)
                    + f"\nERROR:Array size must be at least 1, but it is {n} in this statement")
            ml_ = max_locs(st)
            st2 = ext_st(st, l, LocsVec(gen_locsvec(n, ml_ + 1)))
            return ext_st_zero(st2, max_locs(st2) + 1, n)

        case ArrayDestruction(tid, e, obj):
            n = match_int(eval_exp(e, env, st), "array size must be integer")
            if n < 1:
                raise StmError(
                    pretty_stms([stm], 0)
                    + f"\nERROR:Array size must be at least 1, but it is {n} in this statement")
            veclocs, _ = lval_val(obj, env)
            vec = match_locsvec(lookup_st(veclocs, st))
            l = lookup_vec(0, vec)
            if is_field_zero(st, l, n):
                st2 = st
                for loc in vec:
                    st2 = {k: v for k, v in st2.items() if k != loc}
                return ext_st(st2, veclocs, IntVal(0))
            raise StmError(pretty_stms([stm], 0) + "\nERROR:All array elements is not zero-cleared in this statement")

        case CopyReference(dt, obj1, obj2):
            locsx2, v = lval_val(obj2, env)
            if v != IntVal(0):
                raise StmError(pretty_stms([stm], 0) + "\nERROR:variable of right is not nil in this statement")
            _, vx = lval_val(obj1, env)
            return ext_st(st, locsx2, vx)

        case UncopyReference(dt, obj1, obj2):
            _, v1 = lval_val(obj1, env)
            l, v2 = lval_val(obj2, env)
            if v1 == v2:
                return ext_st(st, l, IntVal(0))
            raise StmError(pretty_stms([stm], 0) + "\nERROR:both variable's reference is not same in this statement")

        case LocalBlock(dt, name, e1, body, e2):
            # 入口・出口の式が自分自身を参照すると表明が恒真になり、逆向きの実行が
            # 値を復元できない（coq/roopl.v の E_local の x ∉ fv(e1), x ∉ fv(e2)）
            if name in free_vars(e1) or name in free_vars(e2):
                raise StmError(
                    pretty_stms([stm], 0)
                    + f"\nERROR:Local variable {name} must not occur in its own"
                      " local/delocal expression")
            v1 = eval_exp(e1, env, st)
            l = max_locs(st) + 1
            env2 = ext_envs(env, name, l)
            st2 = ext_st(st, l, v1)
            st3 = eval_state(body, env2, map_, st2)
            v2 = eval_exp(e2, env2, st3)
            if lookup_st(l, st3) == v2:
                return {k: v for k, v in st3.items() if k != l}
            raise RuntimeError(
                pretty_stms([stm], 0) +
                f"\nERROR: Variable {name} = {Printer.show_val(lookup_st(l, st3))}, But it should be {Printer.show_val(v2)} in this statement")


def match_locsval(v: Value) -> Locs:
    match v:
        case LocsVal(l): return l
        case _: raise RuntimeError("ERROR:expected location value for 'this'")


# --- Switch evaluator ---

def _eval_switch(stm, obj1, cases, default_stml, obj2, env, map_, st, lval_val, isTrue, isFalse):
    def isMatch(obj, q, env, st):
        if len(q) == 0:
            return False
        locs, _ = lval_val(obj, env)
        v = lookup_st(locs, st)
        return any(eval_exp(x, env, st) == v for x in q)

    def search_break(cs):
        result = []
        for (_, _), s, (_, q2, b) in cs:
            result.append((s, q2))
            if b == Break.Break:
                break
        return result

    def eval_case1(sq, obj2, length, env, map_, st):
        if not sq:
            raise RuntimeError("ERROR:empty case list in switch statement")
        if len(sq) == 1:
            s, q = sq[0]
            st2 = eval_state(s, env, map_, st)
            locs, _ = lval_val(obj2, env)
            v = lookup_st(locs, st2)
            if length > 0 and length - 1 < len(q):
                if v == eval_exp(q[length - 1], env, st):
                    return st2
                raise RuntimeError(
                    pretty_stms([stm], 0) + "\nERROR:assertion is incorrect:should be " +
                    pretty_exp(q[length - 1]) + " in this switch statement")
            raise StmError(pretty_stms([stm], 0) + "\nERROR:assertion index out of bounds in switch statement")
        s, _ = sq[0]
        st2 = eval_state(s, env, map_, st)
        return eval_case1(sq[1:], obj2, length, env, map_, st2)

    def eval_case2(obj1, q1, sq, obj2, n, env, map_, st):
        def countMatch(obj1, q, n, env, st):
            _, v = lval_val(obj1, env)
            for i, e in enumerate(q):
                if v == eval_exp(e, env, st):
                    return n + i
            raise RuntimeError("ERROR:no matching value found in switch case expression list")

        count = countMatch(obj1, q1, 1, env, st)
        for i, (s, q) in enumerate(sq):
            if not q:
                raise RuntimeError("ERROR:empty expression list in switch case")
            if count == n + i:
                st2 = eval_state(s, env, map_, st)
                locs, _ = lval_val(obj2, env)
                v = lookup_st(locs, st2)
                if v == eval_exp(q[0], env, st):
                    return st2
                raise RuntimeError(
                    pretty_stms([stm], 0) + "\nERROR:assertion is incorrect:should be " +
                    pretty_exp(q[0]) + " in this switch statement")
            st = eval_state(s, env, map_, st)
        raise RuntimeError("ERROR:no matching case found in switch case evaluation")

    def eval_cases(obj1, cs, default_s, obj2, env, map_, st):
        if not cs:
            return eval_state(default_s, env, map_, st)
        ((c1, q1), s1, (c2, q2, b1)) = cs[0]
        rest = cs[1:]

        if len(q1) == 1 and len(q2) == 1 and isMatch(obj1, q1, env, st):
            st2 = eval_state(s1, env, map_, st)
            if isMatch(obj2, q2, env, st2):
                return st2
            raise RuntimeError(
                pretty_stms([stm], 0) + "\nERROR:assertion is incorrect:should be " +
                pretty_exp(q2[0]) + " in this switch statement")

        if ((len(q1) == 1 and c2 == Esac.NoEsac) or len(q2) >= 2) and isMatch(obj1, q1, env, st):
            sq = search_break(cs)
            return eval_case1(sq, obj2, len(sq), env, map_, st)

        if len(q1) >= 2 and isMatch(obj1, q1, env, st):
            sq = search_break(cs)
            return eval_case2(obj1, q1, sq, obj2, 1, env, map_, st)

        if len(cs) == 1 and not isMatch(obj1, q1, env, st):
            return eval_state(default_s, env, map_, st)

        st2 = eval_cases(obj1, rest, default_s, obj2, env, map_, st)
        # 通らなかった枝の出口表明は偽でなければならない。さもないと出口の値が
        # 枝を識別できず、逆向きの実行が枝を選び直せない（形式化では
        # coq/roopl.v の rev_switch が入れ子の条件分岐の else 側として
        # E_if_f でこれを検査している）。
        if (len(q2) == 1 and not isMatch(obj1, q1, env, st)
                and isMatch(obj2, q2, env, st2)):
            raise StmError(
                pretty_stms([stm], 0) + "\nERROR:assertion is incorrect:the exit value " +
                pretty_exp(q2[0]) + " of a case that was not taken must not match in this switch statement")
        return st2

    return eval_cases(obj1, cases, default_stml, obj2, env, map_, st)


# --- Program initialization ---

def gen_env(fid: list[str]) -> Env:
    env: Env = {}
    n = 1
    for name in fid:
        env[name] = n
        n += 1
    env["this"] = n
    return env


def gen_st(env: Env, objval: Value) -> State:
    items = list(env.items())
    st: State = {}
    for i, (name, locs) in enumerate(items):
        if i < len(items) - 1:
            st[locs] = IntVal(0)
        else:
            # Last entry is "this"
            st[locs] = LocsVal(locs + 1)
            st[locs + 1] = objval
    return st


def gen_result(env: Env, st: State) -> list[tuple[str, Value]]:
    result = []
    for name, locs in env.items():
        v = lookup_st(locs, st)
        match v:
            case LocsVec(vec):
                for i, l in enumerate(vec):
                    result.append((f"{name}[{i}]", lookup_st(l, st)))
            case _:
                result.append((name, v))
    return result


def lookup_class_map(clist: list[CDecl], cid: str) -> CDecl:
    for c in clist:
        if c.name == cid:
            return c
    raise RuntimeError(f"ERROR:class {cid} is not exist")


def map_field(clist: list[CDecl], c: CDecl) -> list[Decl]:
    if c.inherits is None:
        return c.fields
    parent = lookup_class_map(clist, c.inherits)
    return map_field(clist, parent) + c.fields


def map_method(clist: list[CDecl], c: CDecl) -> list[MDecl]:
    if c.inherits is None:
        return c.methods
    parent = lookup_class_map(clist, c.inherits)
    parent_methods = map_method(clist, parent)
    sub_names = {m.name for m in c.methods}
    filtered = [m for m in parent_methods if m.name not in sub_names]
    return filtered + c.methods


def gen_map(clist: list[CDecl]) -> list[tuple[str, tuple[list[Decl], list[MDecl]]]]:
    return [(c.name, (map_field(clist, c), map_method(clist, c))) for c in clist]


def lookup_class(method_id: str, map_: list) -> tuple[str, list[Stm]]:
    for cid, (fl, ml) in map_:
        for m in ml:
            if m.name == method_id:
                return cid, m.body
    raise RuntimeError(f"ERROR:class {method_id} was not found")


def eval_prog_state(prog: Prog, library: Prog | None = None):
    """プログラムを実行し、(結果, 最終ストア) を返す（eval_prog_state に対応）。"""
    lib_classes = library.classes if library else []
    map0 = gen_map(lib_classes)
    map_ = map0 + gen_map(prog.classes)
    mid, mainstml = lookup_class("main", map_)
    field, _ = lookup_map(mid, map_)
    fid = [f.name for f in field]
    env = gen_env(fid)
    st = gen_st(env, ObjVal(mid, env))
    st2 = eval_state(mainstml, env, map_, st)
    result = gen_result(env, st2)
    return [(k, v) for k, v in result if k != "this"], st2


def eval_prog(prog: Prog, library: Prog | None = None) -> list[tuple[str, Value]]:
    """結果のリストだけを返す。"""
    return eval_prog_state(prog, library)[0]
