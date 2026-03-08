<?php
// 共有URLを発行する

$dir = dirname(__FILE__);

$max_input_size = 1024 * 1024;
$json_string = file_get_contents("php://input", false, null, 0, $max_input_size);
$post = json_decode($json_string, true);

if ($post === null || !isset($post['prog'])) {
    header("HTTP/1.1 400 Bad Request");
    echo json_encode(["error" => "Invalid request"]);
    exit;
}

// プログラムを保存
$prog_text = $post['prog'];
$prog_hash = substr(sha1($prog_text), 0, 8);
$res = file_put_contents("$dir/programs/$prog_hash.rplpp", $prog_text);
if ($res === FALSE) {
    header("HTTP/1.1 500 Internal Server Error");
    exit;
}

header('Content-type:application/json; charset=utf8');
echo json_encode([$prog_hash]);
exit;
