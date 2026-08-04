(** 利用者向けの診断メッセージ（PyJanus の [jana_py/errors.py] を範にとる）。

    実装 (diagnostics.ml) は 37 個の定義を持つが、外から使うのはここに挙げた
    9 個だけである。残り（生メッセージの分解・ソース行の照合・キャレットの
    組み立て・修正ヒントの判定など）は整形の内部事情で、外から触ると
    メッセージの形式に依存した結合が生まれる。

    メッセージの形式は eval.ml と diagnostics.ml の**両側の取り決め**なので、
    片方だけ変えてはいけない（test/diagnostics_test.ml が形式を固定している）。 *)

(** {1 実行時エラーと構文エラーの整形} *)

val format_runtime_error : ?src:string -> ?file:string -> string -> string
(** eval.ml が投げた生メッセージを、原因・場所・変数の値・修正ヒントを備えた
    構造化テキストにする。[src] があればソースの抜粋とキャレットを添える。 *)

val format_parse_error :
  ?src:string -> ?file:string -> Lexing.position -> Lexing.position -> string
(** 構文解析器が落ちた位置（開始・終了）を同じ体裁で整形する。列は 0 起点。 *)

val fix_hints : string -> string list
(** メッセージの内容から、対応する修正ヒントを選ぶ。 *)

(** {1 実行後のゼロクリア検査} *)

val garbage_report :
  ?limit:int ->
  (string * Value.value) list -> (Value.locs * Value.value) list -> string
(** 主クラスの変数のうち 0 / nil に戻っていないものを一覧する。可逆プログラムは
    終了時にすべてゼロへ戻るのが「クリーン」で、残っているものは意図した出力か
    ガーベジのどちらかである。未解放のオブジェクトや配列は中身まで展開する。 *)

val is_zero : Value.value -> bool
(** ゼロクリア済みか（[IntVal 0] のみ真。nil も [IntVal 0] で表される）。 *)

(** {1 eval.ml が文を包むときに使う部品} *)

val where_line : Syntax.stm -> (Syntax.id * Value.locs) list ->
  (Value.locs * Value.value) list -> string
(** その文に現れる識別子の値を並べた [WHERE:] 行。 *)

val has_where : string -> bool
(** 生メッセージに既に [WHERE:] 行が付いているか（二重に積まないための判定）。 *)

val at_line : int -> string
(** 行番号だけの位置マーカ [AT:]。 *)

val at_span : int * int * int * int -> string
(** (行, 列, 終了行, 終了列) の位置マーカ。列は 0 起点。 *)

(** {1 テスト用}

    ここから下は test/diagnostics_test.ml が直接呼ぶもの。メッセージの形式は
    eval.ml との取り決めなので、その形式を固定するテストが要る。 *)

val contains : needle:string -> string -> bool
(** 部分文字列の判定。メッセージ検査を「完全一致」ではなく「本文を含むこと」で
    書けるようにするために公開している。 *)

val contains_at_boundary : needle:string -> string -> bool
(** 語の境界に接した部分文字列の判定（識別子の照合に使う）。 *)

val split_runtime_message : string -> string list * string * string list
(** 生メッセージを (トレース, 本体, 変数の値) に分ける。この 3 分割が
    eval.ml と diagnostics.ml の境界そのもの。 *)

val strip_context : string -> string
(** 「… in this statement」のような文脈の接尾辞を落とす。 *)

val ids_of_stm : Syntax.stm -> Syntax.id list
(** 文に現れる識別子（[where_line] が値を並べる対象）。 *)

val candidate_lines : string -> string -> int list
(** pretty 表示した文をソース本文と照合して、その文がある行の候補を返す
    （空白と括弧の違いは無視する）。位置を持たない文の行推定に使う。 *)
