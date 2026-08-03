#!/usr/bin/env python3
"""ROOPL++ interpreter CLI (from main.ml).

OCaml 実装 (bin/main.ml) と同じ出力・同じ終了コードになるようにしている。
両者の一致は python/tests/test_differential.py で検査する。
"""
from __future__ import annotations

import argparse
import os
import sys

from rooplpp import diagnostics as Diag
from rooplpp.eval import StmError, eval_prog_state
from rooplpp.invert import invert_prog
from rooplpp.parser import ParseError, parse
from rooplpp.pretty import pretty_prog
from rooplpp.printer import print_result


def _parse_error_position(err: ParseError) -> tuple[int, int, int, int]:
    """ParseError から (行, 列, 終了行, 終了列) を取り出す。

    **列は 0 起点に直す。** 字句解析器の列は 1 起点だが、診断の規約は 0 起点
    （parser.py の Pos と同じ。OCaml 側は pos_cnum - pos_bol）。ここが素通しに
    なっていて、構文エラーのキャレットだけが OCaml と 1 桁ずれていた。
    """
    tok = err.token
    line = getattr(tok, "line", 0) or 0
    col = max((getattr(tok, "col", 0) or 0) - 1, 0)
    # 幅はトークン自身が持っている（value は記号のトークンでは None なので使えない）。
    # 幅の無いトークン（EOF）は OCaml 側も開始＝終了になる
    end_col = max((getattr(tok, "end_col", 0) or 0) - 1, col)
    return line, col, line, end_col


def main() -> None:
    ap = argparse.ArgumentParser(description="ROOPLPP interpreter")
    ap.add_argument("file", help="ROOPL++ source file")
    ap.add_argument("-inverse", action="store_true",
                    help="print the inverted program instead of running it")
    ap.add_argument("-library", action="store_true",
                    help="load library/Library.rplpp before the program")
    ap.add_argument("-no-zero-check", dest="no_zero_check", action="store_true",
                    help="suppress the zero-clear (garbage) report after execution")
    args = ap.parse_args()

    try:
        with open(args.file) as f:
            source = f.read()
    except OSError as e:
        print("ROOPL++ error\n  message: " + str(e))
        sys.exit(1)

    try:
        prog = parse(source)
    except ParseError as e:
        line, col, eline, ecol = _parse_error_position(e)
        print(Diag.format_parse_error(line, col, eline, ecol,
                                      src=source, file=args.file))
        sys.exit(1)

    if args.inverse:
        print(pretty_prog(invert_prog(prog)), end="")
        return

    try:
        library = None
        if args.library:
            lib_path = os.path.join(os.path.dirname(args.file), "..",
                                    "library", "Library.rplpp")
            if not os.path.exists(lib_path):
                lib_path = os.path.join(os.path.dirname(__file__), "..",
                                        "library", "Library.rplpp")
            with open(lib_path) as f:
                library = parse(f.read())
        result, st = eval_prog_state(prog, library)
        print_result(result)
        if not args.no_zero_check:
            print(Diag.garbage_report(result, st))
    except (StmError, RuntimeError) as e:
        print()
        print(Diag.format_runtime_error(str(e), src=source, file=args.file))
        sys.exit(1)


if __name__ == "__main__":
    main()
