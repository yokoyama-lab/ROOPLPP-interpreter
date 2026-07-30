"""診断メッセージの整形（lib/diagnostics.ml の移植）。

実行時エラー・構文エラー・終了時のゼロクリア検査を、人間にも LLM にも読める
形へ整形する。OCaml 実装と**同じ文面**を出すことを目標にしており、
`python/tests/test_differential.py` が両実装の出力を突き合わせている。
"""
from __future__ import annotations

from .syntax import (
    Assign, ArrayConstruction, ArrayDestruction, ArrayElement, Binary, Const,
    CopyReference, Conditional, Dot, For, LocalBlock, LocalCall, LocalUncall,
    Loop, Nil, ObjectBlock, ObjectCall, ObjectConstruction, ObjectDestruction,
    ObjectUncall, EPos, Positioned, Print, Show, Skip, Swap, Switch,
    UncopyReference, Var, VarArray, InstVar, IdArg, ExpArg,
)
from .value import IntVal, LocsVal, LocsVec, ObjVal

WHERE_MARKER = "WHERE:"
# 構文解析器が付けた行番号。eval.py が文のラッパで積む
AT_MARKER = "AT:"


def at_line(n: int) -> str:
    return "\n" + AT_MARKER + str(n)


def at_span(span: tuple[int, int, int, int]) -> str:
    """式の範囲つきの位置マーカ（行:列:終了行:終了列）"""
    return "\n%s%d:%d:%d:%d" % (AT_MARKER, *span)


# ------------------------------------------------------------------
# 文字列ユーティリティ
# ------------------------------------------------------------------

def strip_context(s: str) -> str:
    """"... in this statement in this expression" のような文脈接尾辞を落とす"""
    changed = True
    while changed:
        changed = False
        for suffix in (" in this statement", " in this expression"):
            if s.endswith(suffix):
                s = s[: -len(suffix)]
                changed = True
    return s.strip()


def normalize(s: str) -> str:
    """行の同一性をゆるく見るための正規化（空白と括弧を落とす）"""
    return "".join(c.lower() for c in s if c not in " \t\r()")


def _contains_at_boundary(needle: str, hay: str) -> bool:
    """識別子の途中で始まる一致を除いた包含判定
    （"local int t = 0" が "delocal int t = 0" に一致しないように）"""
    if not needle:
        return True
    start = 0
    while True:
        i = hay.find(needle, start)
        if i < 0:
            return False
        if i == 0 or not (hay[i - 1].isalnum() or hay[i - 1] == "_"):
            return True
        start = i + 1


# ------------------------------------------------------------------
# 実行時エラーメッセージの分解
# ------------------------------------------------------------------

def exact_pos(raw: str):
    """生メッセージから構文解析器由来の位置を取り出す。

    "AT:<行>"（文）と "AT:<行>:<列>:<終了行>:<終了列>"（式）の両方を読む。
    戻り値は (行, None) か (行, (列, 終了行, 終了列))。
    """
    found = None
    for ln in raw.split("\n"):
        ln = ln.strip()
        if not ln.startswith(AT_MARKER):
            continue
        parts = ln[len(AT_MARKER):].strip().split(":")
        try:
            nums = [int(p) for p in parts]
        except ValueError:
            continue
        if len(nums) == 1:
            found = (nums[0], None)
        elif len(nums) == 4:
            found = (nums[0], (nums[1], nums[2], nums[3]))
    return found


def exact_line(raw: str) -> int | None:
    p = exact_pos(raw)
    return None if p is None else p[0]


def split_runtime_message(raw: str) -> tuple[list[str], str, list[str]]:
    """生メッセージを (トレース, 本体メッセージ, 変数の値) に分ける"""
    lines = raw.split("\n")
    # 位置マーカは本文にもトレースにも出さない
    lines = [ln for ln in lines if not ln.strip().startswith(AT_MARKER)]
    where = [
        ln.strip()[len(WHERE_MARKER):].strip()
        for ln in lines
        if ln.strip().startswith(WHERE_MARKER)
    ]
    where = [w for w in where if w]
    lines = [ln for ln in lines if not ln.strip().startswith(WHERE_MARKER)]

    err_index = None
    for i, ln in enumerate(lines):
        if "ERROR" in ln:
            err_index = i
    if err_index is None:
        trace = [ln.strip() for ln in lines if ln.strip()]
        return trace, "\n".join(lines).strip(), where

    trace = [ln.strip() for ln in lines[:err_index] if ln.strip()]
    err_line = lines[err_index]
    p = err_line.index("ERROR")
    rest = err_line[p + 5:]
    if rest.startswith(":"):
        rest = rest[1:]
    return trace, strip_context(rest.strip()), where


# ------------------------------------------------------------------
# 修正ヒント
# ------------------------------------------------------------------

def fix_hints(message: str) -> list[str]:
    m = message.lower()
    hints: list[str] = []

    def has(needle: str) -> bool:
        return needle in m

    if has("array index") and has("out of bounds"):
        hints.append("`new int[n] xs` allocates xs[0] .. xs[n-1] only; check the size given to `new` and the index expression.")
        hints.append("`for i in (a..b)` is inclusive at both ends, and counts DOWN when a > b (so an empty range like (0..n-1) with n = 0 runs twice).")
        hints.append("`&&` and `||` do not short-circuit: `i < n && xs[i] = 0` still evaluates xs[i]. Clamp the index (e.g. xs[i * (i < n)]) or allocate a sentinel element.")
    if has("negative index"):
        hints.append("The index expression evaluated to a negative number; ROOPL++ arrays are 0-based with no wrap-around.")
    if has("all array elements is not zero-cleared"):
        hints.append("`delete int[n] xs` requires every element to be 0. Clear the array first, e.g. by `uncall`ing whatever filled it.")
        hints.append("If the array holds the result you want to keep, make it a field of the main class instead of deleting it.")
    if has("all instance field is not zero-cleared"):
        hints.append("`delete C x` requires every field of the object to be 0 / nil first; uncall the methods that filled them.")
    if has("assertion should be true"):
        hints.append("In `from e1 do s1 loop s2 until e2`, the entry assertion e1 must hold when the loop is entered. Check the initial value of the loop variable.")
    if has("assertion should be false"):
        hints.append("In `from e1 do s1 loop s2 until e2`, e1 must be FALSE on every iteration after the first, so it must characterise the entry state only (e.g. `from i = 0` with `i += 1` in s2).")
    if has("assertion is incorrect"):
        hints.append("The exit condition did not match the branch that was actually taken: `if e1 then s1 else s2 fi e2` requires e2 to be true after s1 and false after s2.")
    if has("but it should be"):
        hints.append("`delocal t x = e` requires x to equal e at the end of the block; e has to recompute the variable's final value from what is still available.")
        hints.append("If the final value cannot be recomputed, log it (e.g. into an array) and delocalise against the logged value.")
    if has("formal argument and actual argument are not same value"):
        hints.append("Arguments that are expressions -- including array elements such as a[i] -- are passed by value and must be unchanged when the method returns.")
        hints.append("To let a method write into an array, pass the array and the index separately: `call m(a, i)` and update `a[i]` inside.")
    if has("not nil"):
        hints.append("`new`, `construct` and `copy` require their target to be nil; the variable still refers to an object. Delete or uncopy it first.")
    if has("both variable's reference is not same"):
        hints.append("`uncopy t x y` requires x and y to refer to the same object; uncopy against the very reference that `copy` duplicated.")
    if has("division by zero") or has("modulo by zero"):
        hints.append("Guard the divisor, or restructure so the divisor is a constant known to be non-zero.")
    if has("unbound variable"):
        hints.append("The identifier is not a field of the main class, not a parameter, and not in scope of an enclosing `local` block.")
        hints.append("Check the spelling, and remember that a `local` variable is only visible until its matching `delocal`.")
    if has("method") and has("does not exist"):
        hints.append("Check the method name and that the number of arguments matches the declaration; ROOPL++ resolves methods by name only.")
    if has("mismatched argument list lengths"):
        hints.append("The call passes a different number of arguments than the method declares.")
    if has("must not change") and has("for statement"):
        hints.append("The loop variable of `for i in (a..b)` is read-only inside the body; use a separate local variable if you need to modify it.")
    if has("integer value") or has("integer values expected"):
        hints.append("An arithmetic or `+=`/`-=`/`^=` operand was an object or array reference instead of an int.")
    if has("expected object value") or has("expected location value"):
        hints.append("The variable is nil (or not an object) at this point; construct it with `new`/`construct` before calling into it.")
    if has("no matching case") or has("switch"):
        hints.append("Every executed `switch` needs a matching `case`, and the closing `hctiws` value must select the same branch that was taken.")
    if has("class") and (has("not valid") or has("not found") or has("not exist")):
        hints.append("The class name is unknown; check the spelling, or load the standard library with `-library`.")
    if not hints:
        hints.append("Check the statement shown above against the reversibility condition it has to satisfy (entry/exit assertions, zero-cleared targets, unchanged value arguments).")
    return hints


# ------------------------------------------------------------------
# ソース抜粋
# ------------------------------------------------------------------

def _excerpt_line(width: int, marked: bool, n: int, text: str) -> str:
    return "  %s %*d | %s" % (">" if marked else " ", width, n, text)


def source_excerpt(src: str, line: int) -> list[str]:
    lines = src.split("\n")
    if line <= 0 or line > len(lines):
        return []
    start = max(1, line - 1)
    stop = min(len(lines), line + 1)
    width = len(str(stop))
    return [
        _excerpt_line(width, n == line, n, lines[n - 1])
        for n in range(start, stop + 1)
    ]


def source_excerpt_caret(src: str, line: int, col: int, length: int = 1) -> list[str]:
    lines = src.split("\n")
    if line <= 0 or line > len(lines):
        return []
    width = len(str(min(len(lines), line + 1)))
    out = []
    if line > 1:
        out.append(_excerpt_line(width, False, line - 1, lines[line - 2]))
    out.append(_excerpt_line(width, True, line, lines[line - 1]))
    out.append("  %s %*s | %s%s"
               % (" ", width, "", " " * max(0, col), "^" * max(1, length)))
    if line < len(lines):
        out.append(_excerpt_line(width, False, line + 1, lines[line]))
    return out


def candidate_lines(src: str, needle: str) -> list[int]:
    key = normalize(needle)
    if len(key) < 4:
        return []
    return [
        i + 1
        for i, ln in enumerate(src.split("\n"))
        if _contains_at_boundary(key, normalize(ln))
    ]


def _needle_variants(t: str) -> list[str]:
    out = [t]
    for kw in (" do", " then", " loop"):
        if t.endswith(kw):
            out.append(t[: -len(kw)])
    return out


def locate(src: str, trace: list[str]):
    def candidates_of(t: str) -> list[int]:
        for v in _needle_variants(t):
            c = candidate_lines(src, v)
            if c:
                return c
        return []

    for t in trace:
        c = candidates_of(t)
        if len(c) == 1:
            return ("exact", c[0])
    if trace:
        c = candidates_of(trace[0])
        if 0 < len(c) <= 5:
            return ("candidates", c)
    return ("unknown", None)


# ------------------------------------------------------------------
# エラー整形
# ------------------------------------------------------------------

def _bullets(label: str, items: list[str]) -> list[str]:
    if not items:
        return []
    return ["", label + ":"] + ["  - " + i for i in items]


def format_runtime_error(raw: str, src: str | None = None, file: str | None = None) -> str:
    trace, message, where = split_runtime_message(raw)
    out = ["ROOPL++ execution error", "  message: " + message]
    if file:
        out.append("  file: " + file)
    exact = exact_pos(raw)
    if exact is not None:
        # 構文解析器が付けた位置があればそれを使う（推定は要らない）
        line, span = exact
        if span is not None and span[1] == line:
            out.append("  line: %d, columns %d-%d" % (line, span[0], span[2]))
        else:
            out.append("  line: %d" % line)
        if src is not None:
            if span is not None and span[1] == line:
                # 同じ行に収まる式なら、その範囲にキャレットを引く
                ex = source_excerpt_caret(src, line, span[0],
                                          max(1, span[2] - span[0]))
            else:
                ex = source_excerpt(src, line)
            if ex:
                out += ["", "Source:"] + ex
    elif src is not None:
        kind, val = locate(src, trace)
        if kind == "exact":
            out.append("  line: %d (best-effort match on the statement text)" % val)
            ex = source_excerpt(src, val)
            if ex:
                out += ["", "Source:"] + ex
        elif kind == "candidates":
            out.append(
                "  line: %s (candidates; the statement text occurs on several lines)"
                % ", ".join(str(n) for n in val)
            )
    if trace:
        out += ["", "Trace (outermost first):"] + [
            "  %d: %s" % (i + 1, t) for i, t in enumerate(trace)
        ]
    if where:
        out += ["", "Values on entry to this statement:"] + ["  " + w for w in where]
    out += _bullets("Fix hints", fix_hints(message))
    return "\n".join(out)


def format_parse_error(line: int, col: int, end_line: int, end_col: int,
                       src: str | None = None, file: str | None = None) -> str:
    out = [
        "ROOPL++ parse error",
        "  message: unexpected token (the parser could not continue here)",
    ]
    if file:
        out.append("  file: " + file)
    out.append(
        "  location: line %d, column %d - line %d, column %d" % (line, col, end_line, end_col)
    )
    if src is not None:
        ex = source_excerpt_caret(src, line, col)
        if ex:
            out += ["", "Source:"] + ex
    out += _bullets("Fix hints", [
        "Statements are not separated by any symbol; a stray `;` or `,` is a parse error.",
        "Every block has to be closed by its keyword: `if e then s else s fi e`, `from e do s loop s until e`, `for x in (a..b) do s end`, `local t x = e s delocal t x = e`, `construct C x s destruct x`.",
        "Only `+=`, `-=`, `^=` and `<=>` update a variable; there is no `=`, `*=` or `/=` assignment.",
        "A method body may not be empty -- use `skip`.",
    ])
    return "\n".join(out)


# ------------------------------------------------------------------
# 終了時のゼロクリア検査
# ------------------------------------------------------------------

def is_zero(v) -> bool:
    return isinstance(v, IntVal) and v.value == 0


def describe(v, st, visited: set | None = None, depth: int = 2) -> str:
    if visited is None:
        visited = set()
    if isinstance(v, IntVal):
        return str(v.value)
    if isinstance(v, LocsVal):
        l = v.locs
        if l in visited:
            return "<object @%d> (already shown)" % l
        visited.add(l)
        inner = st.get(l)
        if isinstance(inner, ObjVal):
            if depth <= 0:
                return "<%s @%d> {...}" % (inner.type_id, l)
            fields = []
            for f, loc in inner.env.items():
                if f == "this":
                    continue
                fv = st.get(loc)
                if fv is not None and not is_zero(fv):
                    fields.append("%s = %s" % (f, describe(fv, st, visited, depth - 1)))
            if not fields:
                return "<%s @%d> (allocated, all fields zero)" % (inner.type_id, l)
            return "<%s @%d> { %s }" % (inner.type_id, l, "; ".join(fields))
        if inner is not None:
            return describe(inner, st, visited, depth - 1)
        return "<location %d>" % l
    if isinstance(v, LocsVec):
        cells = []
        for i, loc in enumerate(v.locs):
            cv = st.get(loc)
            if cv is not None and not is_zero(cv):
                cells.append("[%d] = %s" % (i, describe(cv, st, visited, depth - 1)))
        more = 0
        if len(cells) > 8:
            more = len(cells) - 8
            cells = cells[:8]
        if more:
            cells.append("... %d more" % more)
        if not cells:
            return "<int[%d]> (allocated, all elements zero)" % len(v.locs)
        return "<int[%d]> { %s }" % (len(v.locs), "; ".join(cells))
    if isinstance(v, ObjVal):
        return "<%s object>" % v.type_id
    return str(v)


def garbage_report(result, st, limit: int = 40) -> str:
    total = len(result)
    dirty = [(k, v) for k, v in result if not is_zero(v)]
    if not dirty:
        return ("ROOPL++ zero-clear check: all %d value(s) are zero-cleared "
                "(no garbage left)." % total)
    visited: set = set()
    shown = dirty[:limit]
    lines = ["  %s = %s" % (k, describe(v, st, visited)) for k, v in shown]
    omitted = len(dirty) - len(shown)
    if omitted > 0:
        lines.append("  ... and %d more" % omitted)
    return "\n".join(
        ["ROOPL++ zero-clear check: %d of %d value(s) are NOT zero-cleared:"
         % (len(dirty), total)]
        + lines
        + ["  note: a reversible program ends clean when every variable of the main class is",
           "        back to 0 / nil. Whatever is listed here is either the intended output or",
           "        leftover garbage -- objects and arrays above were allocated and never deleted."]
    )


# ------------------------------------------------------------------
# エラーが起きた文に現れる変数の値
# ------------------------------------------------------------------

def ids_of_exp(e) -> list[str]:
    match e:
        case EPos(_, e0):
            return ids_of_exp(e0)
        case Const(_) | Nil():
            return []
        case Var(x):
            return [x]
        case ArrayElement(x, idx):
            return [x] + ids_of_exp(idx)
        case Binary(_, e1, e2):
            return ids_of_exp(e1) + ids_of_exp(e2)
        case Dot(e1, e2):
            return ids_of_exp(e1) + ids_of_exp(e2)
    return []


def ids_of_obj(o) -> list[str]:
    match o:
        case VarArray(x, None):
            return [x]
        case VarArray(x, idx):
            return [x] + ids_of_exp(idx)
        case InstVar(o1, o2):
            return ids_of_obj(o1) + ids_of_obj(o2)
    return []


def ids_of_arg(a) -> list[str]:
    match a:
        case IdArg(x):
            return [x]
        case ExpArg(e):
            return ids_of_exp(e)
    return []


def ids_of_stm(stm) -> list[str]:
    """文に直接現れる識別子（入れ子の文の中までは見ない）"""
    match stm:
        # 位置情報の殻は素通し
        case Positioned(_, s0):
            return ids_of_stm(s0)
        case Skip() | Print(_):
            return []
        case Assign(o, _, e):
            return ids_of_obj(o) + ids_of_exp(e)
        case Swap(o1, o2):
            return ids_of_obj(o1) + ids_of_obj(o2)
        case Conditional(e1, _, _, e2):
            return ids_of_exp(e1) + ids_of_exp(e2)
        case Loop(e1, _, _, e2):
            return ids_of_exp(e1) + ids_of_exp(e2)
        case For(x, e1, e2, _):
            return [x] + ids_of_exp(e1) + ids_of_exp(e2)
        case Switch(o1, _, _, o2):
            return ids_of_obj(o1) + ids_of_obj(o2)
        case ObjectBlock(_, x, _):
            return [x]
        case LocalBlock(_, x, e1, _, e2):
            return [x] + ids_of_exp(e1) + ids_of_exp(e2)
        case LocalCall(_, args) | LocalUncall(_, args):
            return [i for a in args for i in ids_of_arg(a)]
        case ObjectCall(o, _, args) | ObjectUncall(o, _, args):
            return ids_of_obj(o) + [i for a in args for i in ids_of_arg(a)]
        case ObjectConstruction(_, o) | ObjectDestruction(_, o):
            return ids_of_obj(o)
        case CopyReference(_, o1, o2) | UncopyReference(_, o1, o2):
            return ids_of_obj(o1) + ids_of_obj(o2)
        case ArrayConstruction((_, e), o) | ArrayDestruction((_, e), o):
            return ids_of_exp(e) + ids_of_obj(o)
        case Show(e):
            return ids_of_exp(e)
    return []


def _dedup(xs: list[str]) -> list[str]:
    seen = []
    for x in xs:
        if x not in seen:
            seen.append(x)
    return seen


def where_line(stm, env, st) -> str:
    """エラーになった文に現れる変数の、その時点の値を "WHERE:" 行にする"""
    visited: set = set()
    shown = []
    for x in _dedup(ids_of_stm(stm)):
        if x == "this":
            continue
        locs = env.get(x)
        if locs is None:
            continue
        v = st.get(locs)
        if v is None:
            continue
        shown.append("%s = %s" % (x, describe(v, st, visited, depth=1)))
    return "\n" + WHERE_MARKER + "; ".join(shown)


def has_where(raw: str) -> bool:
    return any(ln.strip().startswith(WHERE_MARKER) for ln in raw.split("\n"))
