<?php
// 共有URLをパラメタにして、保存されているプログラムを呼び出す
require __DIR__ . '/bootstrap.php';

$hash = filter_input(INPUT_GET, "hash");

// パストラバーサル防止：ハッシュは英数字のみ許可
if ($hash === null || $hash === false || !preg_match('/^[a-f0-9]+$/i', $hash)) {
    json_error(400, "Invalid hash format");
}

$filepath = __DIR__ . "/programs/$hash.rplpp";
if (!file_exists($filepath)) {
    json_error(404, "Program not found");
}

json_out([file_get_contents($filepath)]);
