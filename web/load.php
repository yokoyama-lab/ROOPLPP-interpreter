<?php
// 共有URLをパラメタにして、保存されているプログラムを呼び出す

$hash = filter_input(INPUT_GET, "hash");

$dir = dirname(__FILE__);
$con = file_get_contents("$dir/programs/$hash.rplpp");

header('Content-type:application/json; charset=utf8');
echo json_encode(array($con));