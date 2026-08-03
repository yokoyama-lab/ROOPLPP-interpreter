"""エラー経路のテスト（test/error_test.ml の対応物）。

可逆言語では「表明が正しく落ちること」が意味論そのものなので、Python 側でも
同じ経路を検査する。メッセージには文の pretty 表示が積まれるので、完全一致では
なく「ERROR 本文を含むこと」で見る。
"""
from __future__ import annotations

import pytest

from rooplpp.eval import eval_prog
from rooplpp.parser import parse

PROLOGUE = "class Program\n int x\n int y\n int[] a\n method main()\n"


def run(src: str) -> None:
    eval_prog(parse(src))


def prog(body: str) -> str:
    return PROLOGUE + body


def assert_error(needle: str, src: str) -> None:
    with pytest.raises(RuntimeError) as excinfo:
        run(src)
    assert needle in str(excinfo.value), f"expected {needle!r} in:\n{excinfo.value}"


CASES = [
    ("array read out of bounds", "is out of bounds",
     prog("  new int[2] a\n  x += a[2]\n")),
    ("array write out of bounds", "is out of bounds",
     prog("  new int[2] a\n  a[5] += 1\n")),
    # 代入の副条件（可逆性そのもの）。coq/roopl.v の E_assign は構文的な
    # x ∉ fv(e) を、E_fassign / E_aassign / E_aswap は意味的な「書き込みが
    # 右辺と添字の値を変えない」を要求する
    ("assignment target occurs on the right", "must not occur on both sides",
     prog("  x += 3\n  x += x\n")),
    ("subtraction target occurs on the right", "must not occur on both sides",
     prog("  x += 3\n  x -= x\n")),
    ("assignment reads the cell it writes",
     "must not be changed by the assignment itself",
     prog("  new int[2] a\n  a[0] += 5\n  a[0] += a[0]\n")),
    ("field assignment reads the field it writes",
     "must not be changed by the assignment itself",
     "class T\n int f\n method noop()\n  skip\n\n"
     "class Program\n T t\n method main()\n  new T t\n  t.f += 5\n  t.f += t.f\n"),
    ("array swap moves its own index",
     "must not be changed by the statement itself",
     prog("  new int[2] a\n  a[0] += 1\n  a[a[0]] <=> a[0]\n")),
    # 局所ブロックの表明が自分自身を参照すると恒真になる（E_local の x ∉ fv(e)）
    ("delocal expression mentions its own variable", "must not occur in its own",
     prog("  local int t = 0\n  t += 3\n  x += t\n  delocal int t = t\n")),
    ("local expression mentions its own variable", "must not occur in its own",
     prog("  local int t = t\n  x += 1\n  delocal int t = 0\n")),
    ("division by zero", "division by zero", prog("  x += 1 / y\n")),
    ("modulo by zero", "modulo by zero", prog("  x += 1 % y\n")),
    ("conditional exit assertion (then)", "Assertion should be true",
     prog("  if x = 0 then\n   x += 1\n  else\n   skip\n  fi x = 0\n")),
    ("conditional exit assertion (else)", "Assertion should be false",
     prog("  if x != 0 then\n   skip\n  else\n   x += 1\n  fi x != 0\n")),
    ("loop entry assertion false on entry", "Assertion should be true",
     prog("  x += 5\n  from x = 0 loop\n   x -= 1\n  until x = 0\n")),
    ("loop entry assertion still true", "Assertion should be false",
     prog("  local int i = 0\n  from i >= 0 loop\n   i += 1\n  until i = 3\n"
          "  delocal int i = 3\n")),
    ("delocal mismatch", "But it should be",
     prog("  local int t = 0\n  t += 3\n  x += t\n  delocal int t = 0\n")),
    ("delete array not zero-cleared", "All array elements is not zero-cleared",
     prog("  new int[2] a\n  a[0] += 1\n  delete int[2] a\n")),
    ("unbound variable", "unbound variable", prog("  x += z\n")),
    ("no main method", "was not found",
     "class C\n int v\n method notMain()\n  v += 1\n"),
    ("for loop variable modified", "must not change",
     prog("  for i in (0..2) do\n   i += 1\n  end\n")),
    # ループ変数の不変性は最初の 1 周だけでなく毎周検査する
    ("for loop variable changed from the second iteration on", "must not change",
     prog("  for i in (1..3) do\n   if i = 2 then\n    i += 5\n   else\n    skip\n"
          "   fi i = 7\n  end\n")),
    # 範囲式も体で変わってはならない（逆は for i in (e2..e1)）
    ("for range changed in the body", "range of this for statement",
     prog("  x += 3\n  for i in (1..x) do\n   x += 1\n  end\n")),
    # 出口の値が枝を識別できなければ、逆向きの実行が枝を選び直せない
    ("switch exit values do not distinguish the branches", "was not taken",
     prog("  x += 2\n  switch x\n   case 1 y += 10 esac 10 break\n"
          "   case 2 y += 10 esac 10 break\n   default skip break\n  hctiws y\n")),
    ("array where an integer is expected", "expected array value",
     prog("  x += a[0]\n")),
]


@pytest.mark.unit
@pytest.mark.parametrize("name,needle,src", CASES, ids=[c[0] for c in CASES])
def test_error_paths(name: str, needle: str, src: str) -> None:
    assert_error(needle, src)


CONTROLS = [
    ("clean program", prog("  local int t = 0\n  t += 3\n  x += t\n  t -= 3\n"
                           "  delocal int t = 0\n")),
    ("well-formed loop", prog("  local int i = 0\n  from i = 0 loop\n   i += 1\n"
                              "  until i = 3\n  delocal int i = 3\n  x += 1\n")),
    ("delete after zero-clearing", prog("  new int[2] a\n  a[0] += 1\n  a[0] -= 1\n"
                                        "  delete int[2] a\n")),
    # 別のセルを読む代入・添字に使う変数への代入は可逆なので通る
    ("assignment reading another cell",
     prog("  new int[2] a\n  a[1] += 5\n  a[0] += a[1]\n  a[0] -= 5\n  a[1] -= 5\n"
          "  delete int[2] a\n")),
    ("assignment whose index mentions a variable",
     prog("  new int[2] a\n  x += 1\n  a[x] += x\n  a[x] -= 1\n  x -= 1\n"
          "  delete int[2] a\n")),
]


@pytest.mark.unit
@pytest.mark.parametrize("name,src", CONTROLS, ids=[c[0] for c in CONTROLS])
def test_control_cases_do_not_raise(name: str, src: str) -> None:
    run(src)
