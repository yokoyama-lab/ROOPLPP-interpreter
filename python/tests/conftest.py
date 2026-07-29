"""共通のフィクスチャ（リポジトリ位置と OCaml 実装のバイナリ）。"""
from __future__ import annotations

import pathlib
import shutil
import subprocess

import pytest

REPO = pathlib.Path(__file__).resolve().parents[2]
PYTHON_DIR = REPO / "python"
EXAMPLE_DIR = REPO / "example"
OCAML_BIN = REPO / "_build" / "default" / "bin" / "main.exe"


@pytest.fixture(scope="session")
def repo() -> pathlib.Path:
    return REPO


@pytest.fixture(scope="session")
def ocaml_bin() -> pathlib.Path:
    """参照実装（OCaml）。ビルドされていなければ差分テストはスキップする。"""
    if not OCAML_BIN.exists():
        pytest.skip("OCaml interpreter not built (run `dune build`)")
    return OCAML_BIN


def run_python(args: list[str]) -> subprocess.CompletedProcess[str]:
    """OCaml 版と同じ CWD・同じ引数で起動する（エラー出力に出るパスまで一致させるため）。"""
    env = dict(**{k: v for k, v in __import__("os").environ.items()})
    env["PYTHONPATH"] = str(PYTHON_DIR)
    return subprocess.run(
        ["python3", str(PYTHON_DIR / "main.py"), *args],
        cwd=REPO, capture_output=True, text=True, check=False, env=env,
    )


def run_ocaml(binary: pathlib.Path, args: list[str]) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [str(binary), *args],
        cwd=REPO, capture_output=True, text=True, check=False,
    )
