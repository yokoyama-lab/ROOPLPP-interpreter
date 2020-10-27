<?php
$filename = filter_input(INPUT_GET, "filename");

$dir = dirname(__FILE__);
$con = file_get_contents("$dir/../example/$filename");

header('Content-type:application/json; charset=utf8');

echo json_encode(array($con));