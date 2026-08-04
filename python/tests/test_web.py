"""オンラインインタプリタ (web/execute.php) の回帰テスト。

PHP の内蔵サーバを立てて本物のリクエストを投げる（`php://input` は CLI SAPI
では読めないので、php-cli で直接叩くことはできない）。

いちばん見たいのは**実行時間の上限が本当に効くこと**。ROOPL++ は停止しない
プログラムを書けるのに、以前は `set_time_limit`（PHP スクリプト自身にしか
効かない）だけで、`proc_open` の子は永久に回り続けた。終了コード 124 を見る
分岐はあったのに `timeout` を呼んでいなかったので、そこは死んだコードだった。
"""
from __future__ import annotations

import http.client
import json
import os
import pathlib
import shutil
import socket
import subprocess
import time

import pytest

from .conftest import REPO

WEB_DIR = REPO / "web"
# テストでは上限を短くする（web/execute.php が環境変数を見る）
TIMEOUT_SECS = 2

CLEAN = ("class Program\n    int x\n    method main()\n        x += 7\n")

# 停止しないプログラム: 出口条件 i = -1 に到達せず、入口条件 i = 0 は
# 2 周目以降ずっと偽なので、二重ガードを満たしたまま回り続ける
NEVER_STOPS = (
    "class Program\n    int x\n    method main()\n"
    "        local int i = 0\n"
    "        from i = 0 loop\n"
    "            i += 1\n"
    "        until i = -1\n"
    "        delocal int i = -1\n"
)


def _free_port() -> int:
    with socket.socket() as s:
        s.bind(("127.0.0.1", 0))
        return int(s.getsockname()[1])


@pytest.fixture(scope="module")
def web_server(ocaml_bin: pathlib.Path):
    """php -S でオンラインインタプリタを立てる。php が無ければスキップ。"""
    if shutil.which("php") is None:
        pytest.skip("php not installed")
    if shutil.which("timeout") is None:
        pytest.skip("timeout(1) not available")
    (WEB_DIR / "programs").mkdir(exist_ok=True)
    port = _free_port()
    env = dict(os.environ, ROOPLPP_WEB_TIMEOUT=str(TIMEOUT_SECS))
    proc = subprocess.Popen(
        ["php", "-S", f"127.0.0.1:{port}", "-t", str(WEB_DIR)],
        stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, env=env,
    )
    try:
        deadline = time.time() + 10
        while time.time() < deadline:
            try:
                with socket.create_connection(("127.0.0.1", port), timeout=0.2):
                    break
            except OSError:
                time.sleep(0.05)
        else:
            pytest.skip("php built-in server did not start")
        yield port
    finally:
        proc.terminate()
        proc.wait(timeout=5)


def _post(port: int, payload: dict, read_timeout: float) -> str:
    conn = http.client.HTTPConnection("127.0.0.1", port, timeout=read_timeout)
    try:
        conn.request("POST", "/execute.php", json.dumps(payload),
                     {"Content-Type": "application/json"})
        return conn.getresponse().read().decode()
    finally:
        conn.close()


@pytest.mark.integration
def test_runs_a_program(web_server: int) -> None:
    body = json.loads(_post(web_server, {"prog": CLEAN}, read_timeout=30))
    assert "x = 7" in body[0]


@pytest.mark.integration
def test_inverse_flag(web_server: int) -> None:
    body = json.loads(_post(web_server, {"prog": CLEAN, "invert": True},
                            read_timeout=30))
    assert "x -= 7" in body[0]


@pytest.mark.integration
def test_a_program_that_never_stops_is_cut_off(web_server: int) -> None:
    """上限を過ぎたら打ち切られる。効いていなければここで永久に待つ。"""
    started = time.time()
    body = json.loads(_post(web_server, {"prog": NEVER_STOPS},
                            read_timeout=TIMEOUT_SECS + 20))
    elapsed = time.time() - started
    assert "timed out" in body[0], f"expected a timeout, got: {body[0]!r}"
    # 上限のすぐ後で返ること（上限が無ければそもそも返ってこない）
    assert elapsed < TIMEOUT_SECS + 15


@pytest.mark.integration
def test_invalid_json_is_rejected(web_server: int) -> None:
    conn = http.client.HTTPConnection("127.0.0.1", web_server, timeout=30)
    try:
        conn.request("POST", "/execute.php", "not json",
                     {"Content-Type": "application/json"})
        res = conn.getresponse()
        assert res.status == 400
    finally:
        conn.close()
