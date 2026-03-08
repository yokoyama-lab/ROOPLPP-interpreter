<?php
set_time_limit(10);             // 時間制限の設定

// 入力サイズ制限（1MB）
$max_input_size = 1024 * 1024;
$input_length = (int)($_SERVER['CONTENT_LENGTH'] ?? 0);
if ($input_length > $max_input_size) {
    header("HTTP/1.1 413 Payload Too Large");
    echo json_encode(["error" => "Input too large"]);
    exit;
}

// $string 内に含まれるOS毎に異なる改行文字を
// $to に統一する(ここでは\n)
function convertEOL(string $string, string $to = "\n")
{
    return strtr($string, array(
        "\r\n" => $to,
        "\r" => $to,
        "\n" => $to,
    ));
}

$dir = dirname(__FILE__);
$rplpp_path = realpath("$dir/../src/rplpp");
if ($rplpp_path === false) {
    header("HTTP/1.1 500 Internal Server Error");
    echo json_encode(["error" => "Interpreter not found"]);
    exit;
}

$json_string = file_get_contents("php://input", false, null, 0, $max_input_size);
$post = json_decode($json_string, true);

if ($post === null) {
    header("HTTP/1.1 400 Bad Request");
    echo json_encode(["error" => "Invalid JSON"]);
    exit;
}

// コマンド引数を配列で構築
$cmd_args = [$rplpp_path];

// 引数
$invert = $post['invert'] ?? false;
if ($invert) { $cmd_args[] = "-inverse"; }

// プログラムを保存
$prog_text = $post['prog'] ?? '';

// ライブラリ使用時はプログラムに結合
$library = $post['library'] ?? false;
if ($library) {
    $library_path = realpath("$dir/../library/Library.rplpp");
    if ($library_path !== false) {
        $library_text = (string)file_get_contents($library_path);
        $prog_text = $library_text.$prog_text;
    }
}

$prog_hash = substr(sha1($prog_text), 0, 8);

// ハッシュ形式の検証
if (!preg_match('/^[a-f0-9]{8}$/', $prog_hash)) {
    header("HTTP/1.1 500 Internal Server Error");
    exit;
}

$prog_file = "$dir/programs/$prog_hash.rplpp";
$res = file_put_contents($prog_file, $prog_text);
if ($res === FALSE) {
    header("HTTP/1.1 500 Internal Server Error");
    exit;
}
$cmd_args[] = $prog_file;

$cmd = implode(' ', array_map('escapeshellarg', $cmd_args));

$cwd = "/tmp";
$descriptorspec = array(
    0 => array("pipe", "r"),
    1 => array("pipe", "w"),
    2 => array("pipe", "w")
);
$env = array();

$output = '';
$process = proc_open($cmd, $descriptorspec, $pipes, $cwd, $env);

if (is_resource($process)) {

    fwrite($pipes[0], $prog_text);
    fclose($pipes[0]);

    $output = stream_get_contents($pipes[1]);
    fclose($pipes[1]);

    $stderr = stream_get_contents($pipes[2]);
    fclose($pipes[2]);

    $return_value = proc_close($process);

    if ($return_value === 124) {
      $output = "Execution timed out!\n";
    }
}

header('Content-type:application/json; charset=utf8');

echo json_encode(array($output));
