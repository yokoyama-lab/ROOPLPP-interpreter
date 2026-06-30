<?php
// 共有ヘルパ。各エンドポイントが require して使う。
// JSON 応答・エラー・リクエストボディ読み取りを一箇所に集約し、
// ステータスコードや Content-Type、入力検証の重複と漏れを防ぐ。

/** データを JSON で出力する（ステータス省略時は 200）。 */
function json_out($data, int $status = 200): void
{
    http_response_code($status);
    header('Content-type: application/json; charset=utf-8');
    echo json_encode($data);
}

/** エラー JSON を返して終了する。 */
function json_error(int $status, string $message): void
{
    json_out(["error" => $message], $status);
    exit;
}

/**
 * リクエストボディを JSON として読み、連想配列を返す。
 * 不正な JSON や上限超過なら 400 で終了する。
 */
function read_json_body(int $max_size = 1048576): array
{
    $raw = file_get_contents("php://input", false, null, 0, $max_size);
    $data = json_decode((string)$raw, true);
    if (!is_array($data)) {
        json_error(400, "Invalid request");
    }
    return $data;
}
