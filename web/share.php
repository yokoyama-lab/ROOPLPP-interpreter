<?php
// 共有URLを発行する
require __DIR__ . '/bootstrap.php';

$post = read_json_body();
if (!isset($post['prog'])) {
    json_error(400, "Invalid request");
}

// プログラムを保存
$prog_text = (string)$post['prog'];
$prog_hash = substr(sha1($prog_text), 0, 8);
$res = file_put_contents(__DIR__ . "/programs/$prog_hash.rplpp", $prog_text);
if ($res === false) {
    json_error(500, "Failed to save program");
}

json_out([$prog_hash]);
