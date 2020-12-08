<?php
set_time_limit(10);             // 時間制限の設定

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
$cmd = "$dir/../src/./rplpp";

$json_string = file_get_contents("php://input");
$post = json_decode($json_string, true);

// 引数
$invert = $post['invert'];
if ($invert) { $cmd .= " -inverse"; }
// TO-DO: CLIからのライブラリ使用方法に合わせる
// つまり下記のように実現する
//$library = $post['library'];
//if ($library) { $cmd .= " -library"; }

// プログラムを保存
$prog_text = $post['prog'];

// TO-DO: CLIからのライブラリ使用方法に合わせる
// 現在ここでライブラリの内の文字列をプログラムに結合している
$library = $post['library'];
if ($library) {
    $library_text = (string)file_get_contents("$dir/../library/Library.rplpp");
    $prog_text = $library_text.$prog_text;
}

$prog_hash = substr(sha1($prog_text), 0, 8);
$res = file_put_contents("$dir/programs/$prog_hash.rplpp", $prog_text);
if ($res === FALSE) {
    header("HTTP/1.1 500 Internal Server Error");
    exit;
}
$cmd .= " $dir/programs/$prog_hash.rplpp";
// echo $cmd;

$cwd = "/tmp";
$descriptorspec = array(
    0 => array("pipe", "r"),
    1 => array("pipe", "w")
);
$env = array();

$process = proc_open($cmd, $descriptorspec, $pipes, $cwd, $env);

if (is_resource($process)) {

    fwrite($pipes[0], $prog_text);
    fclose($pipes[0]);

    $output = stream_get_contents($pipes[1]);
    fclose($pipes[1]);

    $return_value = proc_close($process);

    // echo $return_value . "\n";

    if ($return_value === 124) {
      //echo "Execution timed out!\n";
      $output = 'Execution timed out!\n';
    }
}

header('Content-type:application/json; charset=utf8');

echo json_encode(array($output));