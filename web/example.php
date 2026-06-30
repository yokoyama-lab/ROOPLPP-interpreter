<?php
// example/ 配下のサンプルプログラムを読み出す。
// filename はパス区切りを含まないファイル名のみ許可し（パストラバーサル防止）、
// 解決後のパスが example/ ディレクトリ配下にあることも確認する（多重防御）。
require __DIR__ . '/bootstrap.php';

$filename = filter_input(INPUT_GET, "filename");
$example_dir = realpath(__DIR__ . "/../example");

if ($filename === null || $filename === false
    || !preg_match('/^[A-Za-z0-9_.-]+\.rplpp$/', $filename)) {
    json_error(400, "Invalid filename");
}

$filepath = realpath("$example_dir/$filename");
if ($example_dir === false || $filepath === false
    || strpos($filepath, $example_dir . DIRECTORY_SEPARATOR) !== 0) {
    json_error(404, "Example not found");
}

json_out([file_get_contents($filepath)]);
