<?php
// example/ 配下のサンプルプログラムを読み出す。
// filename はパス区切りを含まないファイル名のみ許可し（パストラバーサル防止）、
// 解決後のパスが example/ ディレクトリ配下にあることも確認する（多重防御）。

$filename = filter_input(INPUT_GET, "filename");

$dir = dirname(__FILE__);
$example_dir = realpath("$dir/../example");

if ($filename === null || $filename === false
    || !preg_match('/^[A-Za-z0-9_.-]+\.rplpp$/', $filename)) {
    header("HTTP/1.1 400 Bad Request");
    header('Content-type:application/json; charset=utf8');
    echo json_encode(["error" => "Invalid filename"]);
    exit;
}

$filepath = realpath("$example_dir/$filename");
if ($example_dir === false || $filepath === false
    || strpos($filepath, $example_dir . DIRECTORY_SEPARATOR) !== 0) {
    header("HTTP/1.1 404 Not Found");
    header('Content-type:application/json; charset=utf8');
    echo json_encode(["error" => "Example not found"]);
    exit;
}

$con = file_get_contents($filepath);

header('Content-type:application/json; charset=utf8');
echo json_encode(array($con));
