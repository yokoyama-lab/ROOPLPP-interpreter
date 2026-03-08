<?php
// 共有URLをパラメタにして、保存されているプログラムを呼び出す

$hash = filter_input(INPUT_GET, "hash");

// パストラバーサル防止：ハッシュは英数字のみ許可
if ($hash === null || $hash === false || !preg_match('/^[a-f0-9]+$/i', $hash)) {
    header("HTTP/1.1 400 Bad Request");
    echo json_encode(["error" => "Invalid hash format"]);
    exit;
}

$dir = dirname(__FILE__);
$filepath = "$dir/programs/$hash.rplpp";

if (!file_exists($filepath)) {
    header("HTTP/1.1 404 Not Found");
    echo json_encode(["error" => "Program not found"]);
    exit;
}

$con = file_get_contents($filepath);

header('Content-type:application/json; charset=utf8');
echo json_encode(array($con));
