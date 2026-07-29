"""diagnostics.py の単体テスト（test/diagnostics_test.ml の対応物）。"""
from __future__ import annotations

import pytest

from rooplpp import diagnostics as d
from rooplpp.syntax import (Assign, ArrayElement, Binary, BinOp, Const, Loop,
                            ModOp, Skip, Var, VarArray)
from rooplpp.value import IntVal, LocsVal, LocsVec, ObjVal

RAW = (
    "total += a[i]\n"
    "a[i]\n"
    "ERROR:Array index a[4] is out of bounds in this statement in this expression\n"
    "WHERE:total = 6; i = 4"
)

SRC = "\n".join([
    "class Program",
    "    int[] a",
    "    int total",
    "    method main()",
    "        new int[4] a",
    "        for i in (0..4) do",
    "            total += a[i]",
    "        end",
])


@pytest.mark.unit
def test_split_trace() -> None:
    trace, _, _ = d.split_runtime_message(RAW)
    assert trace == ["total += a[i]", "a[i]"]


@pytest.mark.unit
def test_split_message_drops_context() -> None:
    _, msg, _ = d.split_runtime_message(RAW)
    assert msg == "Array index a[4] is out of bounds"


@pytest.mark.unit
def test_split_where() -> None:
    _, _, where = d.split_runtime_message(RAW)
    assert where == ["total = 6; i = 4"]


@pytest.mark.unit
def test_empty_where_is_dropped() -> None:
    _, _, where = d.split_runtime_message("ERROR:boom\nWHERE:")
    assert where == []


@pytest.mark.unit
def test_has_where() -> None:
    assert d.has_where(RAW)
    assert not d.has_where("ERROR:boom")


@pytest.mark.unit
def test_local_does_not_match_delocal() -> None:
    assert d._contains_at_boundary("localintt=0", "localintt=0")
    assert not d._contains_at_boundary("localintt=0", "delocalintt=0")


@pytest.mark.unit
def test_candidate_lines() -> None:
    assert d.candidate_lines(SRC, "total += a[i]") == [7]
    assert d.candidate_lines(SRC, "for i in ( 0 .. 4 ) do") == [6]


@pytest.mark.unit
@pytest.mark.parametrize("message,needle", [
    ("Array index a[4] is out of bounds", "allocates"),
    ("All array elements is not zero-cleared", "`delete int[n] xs`"),
    ("Variable t = 3, But it should be 0", "delocal"),
    ("formal argument and actual argument are not same value", "index separately"),
])
def test_fix_hints(message: str, needle: str) -> None:
    assert any(needle in h for h in d.fix_hints(message))


@pytest.mark.unit
def test_unknown_message_still_gets_a_hint() -> None:
    assert d.fix_hints("totally unknown failure")


@pytest.mark.unit
def test_format_runtime_error_sections() -> None:
    out = d.format_runtime_error(RAW, src=SRC, file="p.rplpp")
    for needle in ["ROOPL++ execution error",
                   "message: Array index a[4] is out of bounds",
                   "file: p.rplpp", "line: 7", "Source:", "> 7 |",
                   "Trace (outermost first):",
                   "Values on entry to this statement:",
                   "total = 6; i = 4", "Fix hints:"]:
        assert needle in out


@pytest.mark.unit
def test_format_runtime_error_without_source() -> None:
    out = d.format_runtime_error(RAW)
    assert "Array index a[4] is out of bounds" in out
    assert "Source:" not in out


@pytest.mark.unit
def test_format_parse_error_has_caret() -> None:
    src = "class Program\n    int x\n    method main()\n        x = 1"
    out = d.format_parse_error(4, 10, 4, 11, src=src, file="p.rplpp")
    for needle in ["ROOPL++ parse error", "file: p.rplpp",
                   "line 4, column 10", "^", "Fix hints:"]:
        assert needle in out


@pytest.mark.unit
def test_ids_of_stm_assignment() -> None:
    stm = Assign(VarArray("total", None), ModOp.ModAdd, ArrayElement("a", Var("i")))
    assert d.ids_of_stm(stm) == ["total", "a", "i"]


@pytest.mark.unit
def test_ids_of_stm_loop_looks_at_guards_only() -> None:
    stm = Loop(Var("i"), [Skip()], [Skip()], Var("n"))
    assert d.ids_of_stm(stm) == ["i", "n"]


@pytest.mark.unit
def test_garbage_report_clean() -> None:
    out = d.garbage_report([("x", IntVal(0)), ("y", IntVal(0))], {1: IntVal(0)})
    assert out == ("ROOPL++ zero-clear check: all 2 value(s) are zero-cleared "
                   "(no garbage left).")


@pytest.mark.unit
def test_garbage_report_lists_non_zero() -> None:
    out = d.garbage_report([("x", IntVal(0)), ("y", IntVal(3))], {})
    assert "1 of 2 value(s) are NOT zero-cleared" in out
    assert "y = 3" in out


@pytest.mark.unit
def test_garbage_report_expands_object() -> None:
    st = {5: ObjVal("Counter", {"this": 5, "n": 6}), 6: IntVal(7)}
    out = d.garbage_report([("c", LocsVal(5))], st)
    assert "<Counter @5>" in out and "n = 7" in out


@pytest.mark.unit
def test_garbage_report_expands_array() -> None:
    st = {3: IntVal(0), 4: IntVal(9)}
    out = d.garbage_report([("xs", LocsVec([3, 4]))], st)
    assert "[1] = 9" in out


@pytest.mark.unit
def test_garbage_report_handles_cycles() -> None:
    st = {1: ObjVal("Node", {"next": 2}), 2: LocsVal(1)}
    out = d.garbage_report([("head", LocsVal(1))], st)
    assert "already shown" in out
