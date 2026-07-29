"""OCaml 実装との差分テスト。

`python/` は `lib/` の手作業移植なので、両者が同じ振る舞いをしていることを
誰も検査していなかった。ここで example/ の全プログラムについて標準出力と
終了コードを突き合わせる（CLAUDE.md の「両実装を同期させる」の実体）。
"""
from __future__ import annotations

import pathlib

import pytest

from .conftest import EXAMPLE_DIR, run_ocaml, run_python

EXAMPLES = sorted(p.name for p in EXAMPLE_DIR.glob("*.rplpp"))


@pytest.mark.integration
@pytest.mark.parametrize("name", EXAMPLES)
def test_same_output_as_ocaml(name: str, ocaml_bin: pathlib.Path) -> None:
    rel = f"example/{name}"
    o = run_ocaml(ocaml_bin, [rel])
    p = run_python([rel])
    assert p.stdout == o.stdout, f"{name}: stdout differs"
    assert p.returncode == o.returncode, f"{name}: exit code differs"


@pytest.mark.integration
@pytest.mark.parametrize("name", ["fib.rplpp", "algo_zagier.rplpp", "algo_bwt.rplpp"])
def test_same_inverse_as_ocaml(name: str, ocaml_bin: pathlib.Path) -> None:
    rel = f"example/{name}"
    o = run_ocaml(ocaml_bin, ["-inverse", rel])
    p = run_python(["-inverse", rel])
    assert p.stdout == o.stdout


@pytest.mark.integration
def test_no_zero_check_flag(ocaml_bin: pathlib.Path) -> None:
    o = run_ocaml(ocaml_bin, ["-no-zero-check", "example/fib.rplpp"])
    p = run_python(["-no-zero-check", "example/fib.rplpp"])
    assert p.stdout == o.stdout
    assert "zero-clear check" not in p.stdout
