(**診断メッセージの整形。

   実行時エラー・構文エラー・終了時のゼロクリア検査を、人間にも LLM にも
   読める形へ整形する。姉妹プロジェクト PyJanus の [jana_py/errors.py]
   （構造化エラー＋修正ヒント）と [runtime.py] の非ゼロ警告に相当する。

   eval.ml は [failwith] に「pretty 表示した文や式を外側から順に並べた行 +
   "ERROR:..." の行」という文字列を積んで投げる。ここではその文字列を
   本体メッセージとトレースに分解し、ソース抜粋・修正ヒントを添える。 *)

open Syntax
open Value

(**エラー本体の後ろに付ける「そのとき変数がどうだったか」の行の目印*)
let where_marker = "WHERE:"
(* 構文解析器が付けた行番号。eval.ml が文のラッパで積む *)
let at_marker = "AT:"
let at_line n = "\n" ^ at_marker ^ string_of_int n

(**式の範囲つきの位置マーカ（行:列:終了行:終了列）*)
let at_span (l, c, el, ec) =
  Printf.sprintf "\n%s%d:%d:%d:%d" at_marker l c el ec

(* ------------------------------------------------------------------ *)
(* 文字列ユーティリティ                                                 *)
(* ------------------------------------------------------------------ *)

let split_lines s = String.split_on_char '\n' s

let is_blank s = String.trim s = ""

(**接尾辞 [suf] を持つなら取り除く*)
let strip_suffix suf s =
  let ls = String.length s and lf = String.length suf in
  if ls >= lf && String.sub s (ls - lf) lf = suf
  then Some (String.sub s 0 (ls - lf))
  else None

(**"... in this statement in this expression" のような文脈接尾辞を落とす*)
let rec strip_context s =
  match strip_suffix " in this statement" s with
  | Some s' -> strip_context s'
  | None ->
     match strip_suffix " in this expression" s with
     | Some s' -> strip_context s'
     | None -> String.trim s

(**部分文字列を含むか*)
let contains ~needle s =
  let ln = String.length needle and ls = String.length s in
  if ln = 0 then true
  else if ln > ls then false
  else
    let rec go i = i + ln <= ls && (String.sub s i ln = needle || go (i + 1)) in
    go 0

(**[needle] が最初に現れる位置*)
let index_of ~needle s =
  let ln = String.length needle and ls = String.length s in
  let rec go i =
    if i + ln > ls then None
    else if String.sub s i ln = needle then Some i
    else go (i + 1)
  in
  if ln = 0 then Some 0 else go 0

(**行の同一性をゆるく見るための正規化。pretty 表示は括弧や空白を
   足すことがあるので、それらを落として比較する。 *)
let normalize s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | ' ' | '\t' | '\r' | '(' | ')' -> ()
      | c -> Buffer.add_char buf (Char.lowercase_ascii c))
    s;
  Buffer.contents buf

(* ------------------------------------------------------------------ *)
(* 実行時エラーメッセージの分解                                         *)
(* ------------------------------------------------------------------ *)

(*eval.ml が投げた生メッセージを (トレース, 本体メッセージ, 変数の値) に分ける。
   トレースは外側の文・式から内側の式へ向かう順に並ぶ。
   変数の値は WHERE: 行（eval.ml の文ラッパが付ける）から取り出す。 *)
(**生メッセージから、構文解析器由来の位置を取り出す。
   "AT:<行>"（文）と "AT:<行>:<列>:<終了行>:<終了列>"（式）の両方を読む。 *)
let exact_pos raw =
  List.fold_left
    (fun acc l ->
      let l = String.trim l in
      match index_of ~needle:at_marker l with
      | Some 0 ->
         let body =
           String.sub l (String.length at_marker)
             (String.length l - String.length at_marker) |> String.trim
         in
         (match List.map int_of_string_opt (String.split_on_char ':' body) with
          | [ Some n ] -> Some (n, None)
          | [ Some n; Some c; Some el; Some ec ] -> Some (n, Some (c, el, ec))
          | _ -> acc)
      | _ -> acc)
    None (split_lines raw)

(**行番号だけが要るとき*)
let exact_line raw = Option.map fst (exact_pos raw)

let split_runtime_message raw =
  let ls = split_lines raw in
  (* 位置マーカは本文にもトレースにも出さない *)
  let ls = List.filter (fun l -> index_of ~needle:at_marker (String.trim l) <> Some 0) ls in
  let where =
    List.filter_map
      (fun l ->
        let l = String.trim l in
        match index_of ~needle:where_marker l with
        | Some 0 -> Some (String.sub l (String.length where_marker)
                            (String.length l - String.length where_marker) |> String.trim)
        | _ -> None)
      ls
    |> List.filter (fun w -> w <> "")
  in
  let ls = List.filter (fun l -> index_of ~needle:where_marker (String.trim l) <> Some 0) ls in
  (* "ERROR" を含む最後の行が本体 *)
  let err_index =
    List.fold_left
      (fun (i, found) l -> (i + 1, if contains ~needle:"ERROR" l then Some i else found))
      (0, None) ls
    |> snd
  in
  match err_index with
  | None ->
     (List.filter (fun l -> not (is_blank l)) ls |> List.map String.trim,
      String.trim (String.concat "\n" ls), where)
  | Some i ->
     let trace =
       List.filteri (fun j _ -> j < i) ls
       |> List.filter (fun l -> not (is_blank l))
       |> List.map String.trim
     in
     let err_line = List.nth ls i in
     let core =
       match index_of ~needle:"ERROR" err_line with
       | None -> err_line
       | Some p ->
          let rest = String.sub err_line (p + 5) (String.length err_line - p - 5) in
          (* "ERROR:" と "ERROR: " と "ERROR in ..." の揺れを吸収 *)
          let rest =
            if String.length rest > 0 && rest.[0] = ':'
            then String.sub rest 1 (String.length rest - 1)
            else rest
          in
          String.trim rest
     in
     (trace, strip_context core, where)

(* ------------------------------------------------------------------ *)
(* 修正ヒント                                                           *)
(* ------------------------------------------------------------------ *)

(**メッセージ本体からキーワードで修正ヒントを選ぶ（PyJanus の _fix_hints 相当）*)
let fix_hints message =
  let m = String.lowercase_ascii message in
  let has needle = contains ~needle m in
  let hints = ref [] in
  let add h = hints := h :: !hints in
  if has "array index" && has "out of bounds" then begin
    add "`new int[n] xs` allocates xs[0] .. xs[n-1] only; check the size given to `new` and the index expression.";
    add "`for i in (a..b)` is inclusive at both ends, and counts DOWN when a > b (so an empty range like (0..n-1) with n = 0 runs twice).";
    add "`&&` and `||` do not short-circuit: `i < n && xs[i] = 0` still evaluates xs[i]. Clamp the index (e.g. xs[i * (i < n)]) or allocate a sentinel element."
  end;
  if has "negative index" then
    add "The index expression evaluated to a negative number; ROOPL++ arrays are 0-based with no wrap-around.";
  if has "all array elements is not zero-cleared" then begin
    add "`delete int[n] xs` requires every element to be 0. Clear the array first, e.g. by `uncall`ing whatever filled it.";
    add "If the array holds the result you want to keep, make it a field of the main class instead of deleting it."
  end;
  if has "all instance field is not zero-cleared" then
    add "`delete C x` requires every field of the object to be 0 / nil first; uncall the methods that filled them.";
  if has "assertion should be true" then
    add "In `from e1 do s1 loop s2 until e2`, the entry assertion e1 must hold when the loop is entered. Check the initial value of the loop variable.";
  if has "assertion should be false" then
    add "In `from e1 do s1 loop s2 until e2`, e1 must be FALSE on every iteration after the first, so it must characterise the entry state only (e.g. `from i = 0` with `i += 1` in s2).";
  if has "assertion is incorrect" then
    add "The exit condition did not match the branch that was actually taken: `if e1 then s1 else s2 fi e2` requires e2 to be true after s1 and false after s2.";
  if has "but it should be" then begin
    add "`delocal t x = e` requires x to equal e at the end of the block; e has to recompute the variable's final value from what is still available.";
    add "If the final value cannot be recomputed, log it (e.g. into an array) and delocalise against the logged value."
  end;
  if has "formal argument and actual argument are not same value" then begin
    add "Arguments that are expressions -- including array elements such as a[i] -- are passed by value and must be unchanged when the method returns.";
    add "To let a method write into an array, pass the array and the index separately: `call m(a, i)` and update `a[i]` inside."
  end;
  if has "not nil" then
    add "`new`, `construct` and `copy` require their target to be nil; the variable still refers to an object. Delete or uncopy it first.";
  if has "both variable's reference is not same" then
    add "`uncopy t x y` requires x and y to refer to the same object; uncopy against the very reference that `copy` duplicated.";
  if has "division by zero" || has "modulo by zero" then
    add "Guard the divisor, or restructure so the divisor is a constant known to be non-zero.";
  if has "unbound variable" then begin
    add "The identifier is not a field of the main class, not a parameter, and not in scope of an enclosing `local` block.";
    add "Check the spelling, and remember that a `local` variable is only visible until its matching `delocal`."
  end;
  if has "method" && has "does not exist" then
    add "Check the method name and that the number of arguments matches the declaration; ROOPL++ resolves methods by name only.";
  if has "mismatched argument list lengths" then
    add "The call passes a different number of arguments than the method declares.";
  if has "must not change" && has "for statement" then
    add "The loop variable of `for i in (a..b)` is read-only inside the body; use a separate local variable if you need to modify it.";
  if has "integer value" || has "integer values expected" then
    add "An arithmetic or `+=`/`-=`/`^=` operand was an object or array reference instead of an int.";
  if has "expected object value" || has "expected location value" then
    add "The variable is nil (or not an object) at this point; construct it with `new`/`construct` before calling into it.";
  if has "no matching case" || has "switch" then
    add "Every executed `switch` needs a matching `case`, and the closing `hctiws` value must select the same branch that was taken.";
  if has "class" && (has "not valid" || has "not found" || has "not exist") then
    add "The class name is unknown; check the spelling, or load the standard library with `-library`.";
  if !hints = [] then
    add "Check the statement shown above against the reversibility condition it has to satisfy (entry/exit assertions, zero-cleared targets, unchanged value arguments).";
  List.rev !hints

(* ------------------------------------------------------------------ *)
(* ソース抜粋                                                           *)
(* ------------------------------------------------------------------ *)

(**1 行分を "  > 12 | ..." の形にする*)
let excerpt_line ~width ~marked n text =
  Printf.sprintf "  %s %*d | %s" (if marked then ">" else " ") width n text

(**[line] を中心に前後 1 行を抜き出す（1 始まり）*)
let source_excerpt src line =
  let ls = Array.of_list (split_lines src) in
  if line <= 0 || line > Array.length ls then []
  else
    let start = max 1 (line - 1) in
    let stop = min (Array.length ls) (line + 1) in
    let width = String.length (string_of_int stop) in
    let acc = ref [] in
    for n = stop downto start do
      acc := excerpt_line ~width ~marked:(n = line) n ls.(n - 1) :: !acc
    done;
    !acc

(**キャレット付きの抜粋（位置情報があるとき）。[len] はキャレットの幅。 *)
let source_excerpt_caret ?(len = 1) src line col =
  let ls = Array.of_list (split_lines src) in
  if line <= 0 || line > Array.length ls then []
  else
    let width = String.length (string_of_int (min (Array.length ls) (line + 1))) in
    let before = if line > 1 then [ excerpt_line ~width ~marked:false (line - 1) ls.(line - 2) ] else [] in
    let here = excerpt_line ~width ~marked:true line ls.(line - 1) in
    let caret =
      Printf.sprintf "  %s %*s | %s%s" " " width ""
        (String.make (max 0 col) ' ') (String.make (max 1 len) '^')
    in
    let after =
      if line < Array.length ls
      then [ excerpt_line ~width ~marked:false (line + 1) ls.(line) ]
      else []
    in
    before @ [ here; caret ] @ after

(**識別子の途中で始まる一致を除いた包含判定。
   "local int t = 0" が "delocal int t = 0" に一致してしまうのを防ぐ。 *)
let contains_at_boundary ~needle s =
  let is_word c = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c = '_' in
  let ln = String.length needle and ls = String.length s in
  if ln = 0 then true
  else
    let rec go i =
      if i + ln > ls then false
      else if String.sub s i ln = needle && (i = 0 || not (is_word s.[i - 1])) then true
      else go (i + 1)
    in
    go 0

(**トレースの 1 行が出てくるソース行番号の候補を探す（空白と括弧を無視した
   包含判定なので厳密ではない。見つかった行は「候補」として報告する）*)
let candidate_lines src needle =
  let key = normalize needle in
  if String.length key < 4 then []
  else
    split_lines src
    |> List.mapi (fun i l -> (i + 1, normalize l))
    |> List.filter (fun (_, l) -> contains_at_boundary ~needle:key l)
    |> List.map fst

(**pretty 表示だけに現れる末尾キーワードを落とした変種も試す。
   例: 文 "from i >= 0 do" はソースでは "from i >= 0 loop" と書かれている。 *)
let needle_variants t =
  let dropped =
    List.filter_map (fun kw -> strip_suffix kw t) [ " do"; " then"; " loop" ]
  in
  t :: dropped

(**トレース全体から、もっとも確からしい行番号（候補）を決める*)
let locate src trace =
  let candidates_of t =
    List.fold_left
      (fun acc v -> match acc with [] -> candidate_lines src v | _ -> acc)
      [] (needle_variants t)
  in
  let rec exact = function
    | [] -> None
    | t :: tl -> (match candidates_of t with [ n ] -> Some n | _ -> exact tl)
  in
  match exact trace with
  | Some n -> `Exact n
  | None ->
     match trace with
     | t :: _ ->
        (match candidates_of t with
         | [] -> `Unknown
         | ns when List.length ns <= 5 -> `Candidates ns
         | _ -> `Unknown)
     | [] -> `Unknown

(* ------------------------------------------------------------------ *)
(* エラー整形                                                           *)
(* ------------------------------------------------------------------ *)

let bullet_lines label items =
  match items with
  | [] -> []
  | _ -> ("" :: (label ^ ":") :: List.map (fun h -> "  - " ^ h) items)

(**実行時エラーを構造化して整形する。
   [src] にソース本文を渡すと、該当行の推定と抜粋を付ける。 *)
let format_runtime_error ?src ?file raw =
  let trace, message, where = split_runtime_message raw in
  let head = [ "ROOPL++ execution error"; "  message: " ^ message ] in
  let file_line = match file with Some f -> [ "  file: " ^ f ] | None -> [] in
  let loc, excerpt =
    match exact_pos raw with
    (* 構文解析器が付けた位置があればそれを使う（推定は要らない） *)
    | Some (n, span) ->
       let loc =
         match span with
         | Some (c, el, ec) when el = n ->
            [ Printf.sprintf "  line: %d, columns %d-%d" n c ec ]
         | _ -> [ Printf.sprintf "  line: %d" n ]
       in
       (loc,
        (match src with
         | None -> []
         | Some src ->
            let e =
              match span with
              (* 同じ行に収まる式なら、その範囲にキャレットを引く *)
              | Some (c, el, ec) when el = n ->
                 source_excerpt_caret ~len:(max 1 (ec - c)) src n c
              | _ -> source_excerpt src n
            in
            (match e with [] -> [] | e -> "" :: "Source:" :: e)))
    | None ->
    match src with
    | None -> ([], [])
    | Some src ->
       (match locate src trace with
        | `Exact n ->
           ([ Printf.sprintf "  line: %d (best-effort match on the statement text)" n ],
            (match source_excerpt src n with [] -> [] | e -> "" :: "Source:" :: e))
        | `Candidates ns ->
           ([ "  line: " ^ String.concat ", " (List.map string_of_int ns)
              ^ " (candidates; the statement text occurs on several lines)" ], [])
        | `Unknown -> ([], []))
  in
  let trace_block =
    match trace with
    | [] -> []
    | _ ->
       "" :: "Trace (outermost first):"
       :: List.mapi (fun i t -> Printf.sprintf "  %d: %s" (i + 1) t) trace
  in
  let where_block =
    match where with
    | [] -> []
    (* 値は「その文に入った時点」のもの。単純文ならエラー時点と同じ。 *)
    | _ -> "" :: "Values on entry to this statement:" :: List.map (fun w -> "  " ^ w) where
  in
  let hints = bullet_lines "Fix hints" (fix_hints message) in
  String.concat "\n"
    (head @ file_line @ loc @ excerpt @ trace_block @ where_block @ hints)

(**構文エラーを整形する（位置情報があるので抜粋は正確）*)
let format_parse_error ?src ?file (start_pos : Lexing.position) (end_pos : Lexing.position) =
  let line = start_pos.pos_lnum in
  let col = start_pos.pos_cnum - start_pos.pos_bol in
  let head =
    [ "ROOPL++ parse error";
      "  message: unexpected token (the parser could not continue here)" ]
  in
  let file_line = match file with Some f -> [ "  file: " ^ f ] | None -> [] in
  let loc =
    [ Printf.sprintf "  location: line %d, column %d - line %d, column %d"
        line col end_pos.pos_lnum (end_pos.pos_cnum - end_pos.pos_bol) ]
  in
  let excerpt =
    match src with
    | None -> []
    | Some src ->
       (match source_excerpt_caret src line col with [] -> [] | e -> "" :: "Source:" :: e)
  in
  let hints =
    bullet_lines "Fix hints"
      [ "Statements are not separated by any symbol; a stray `;` or `,` is a parse error.";
        "Every block has to be closed by its keyword: `if e then s else s fi e`, `from e do s loop s until e`, `for x in (a..b) do s end`, `local t x = e s delocal t x = e`, `construct C x s destruct x`.";
        "Only `+=`, `-=`, `^=` and `<=>` update a variable; there is no `=`, `*=` or `/=` assignment.";
        "A method body may not be empty -- use `skip`." ]
  in
  String.concat "\n" (head @ file_line @ loc @ excerpt @ hints)

(* ------------------------------------------------------------------ *)
(* 終了時のゼロクリア検査                                               *)
(* ------------------------------------------------------------------ *)

(**値がゼロクリアされているか。nil も 0（eval.ml で Nil = IntVal 0）。*)
let is_zero = function IntVal 0 -> true | _ -> false

(**ストアからロケーションを引く（見つからなければ None）*)
let lookup st l = List.assoc_opt l st

(**参照の中身を 1 行で説明する。循環（DoublyLinkedList など）に備えて
   訪問済みロケーションを持ち回り、深さも制限する。 *)
let rec describe ?(depth = 2) ~visited st v =
  match v with
  | IntVal n -> string_of_int n
  | LocsVal l when List.mem l !visited -> Printf.sprintf "<object @%d> (already shown)" l
  | LocsVal l ->
     visited := l :: !visited;
     (match lookup st l with
      | Some (ObjVal (cid, env)) ->
         if depth <= 0 then Printf.sprintf "<%s @%d> {...}" cid l
         else
           let fields =
             env
             |> List.filter (fun (f, _) -> f <> "this")
             |> List.filter_map (fun (f, loc) ->
                    match lookup st loc with
                    | Some v when not (is_zero v) ->
                       Some (f ^ " = " ^ describe ~depth:(depth - 1) ~visited st v)
                    | _ -> None)
           in
           if fields = []
           then Printf.sprintf "<%s @%d> (allocated, all fields zero)" cid l
           else Printf.sprintf "<%s @%d> { %s }" cid l (String.concat "; " fields)
      | Some v -> describe ~depth:(depth - 1) ~visited st v
      | None -> Printf.sprintf "<location %d>" l)
  | LocsVec vec ->
     let cells =
       List.mapi (fun i loc -> (i, lookup st loc)) vec
       |> List.filter_map (function
              | (i, Some v) when not (is_zero v) ->
                 Some (Printf.sprintf "[%d] = %s" i (describe ~depth:(depth - 1) ~visited st v))
              | _ -> None)
     in
     let n = List.length cells in
     let cells, more =
       if n > 8 then (List.filteri (fun i _ -> i < 8) cells, n - 8) else (cells, 0)
     in
     let cells =
       if more > 0 then cells @ [ Printf.sprintf "... %d more" more ] else cells
     in
     if cells = []
     then Printf.sprintf "<int[%d]> (allocated, all elements zero)" (List.length vec)
     else Printf.sprintf "<int[%d]> { %s }" (List.length vec) (String.concat "; " cells)
  | ObjVal (cid, _) -> Printf.sprintf "<%s object>" cid

(* ------------------------------------------------------------------ *)
(* エラーが起きた文に現れる変数の値                                     *)
(* ------------------------------------------------------------------ *)

(**式に現れる識別子*)
let rec ids_of_exp = function
  | EPos (_, e) -> ids_of_exp e
  | Const _ | Nil -> []
  | Var x -> [ x ]
  | ArrayElement (x, e) -> x :: ids_of_exp e
  | Binary (_, e1, e2) -> ids_of_exp e1 @ ids_of_exp e2
  | Dot (e1, e2) -> ids_of_exp e1 @ ids_of_exp e2

(**l 値に現れる識別子*)
let rec ids_of_obj = function
  | VarArray (x, None) -> [ x ]
  | VarArray (x, Some e) -> x :: ids_of_exp e
  | InstVar (o1, o2) -> ids_of_obj o1 @ ids_of_obj o2

let ids_of_arg = function Id x -> [ x ] | Exp e -> ids_of_exp e

(**文に直接現れる識別子。入れ子の文の中までは見ない（エラーはいちばん内側の
   文で報告されるので、その文のガードと被演算子だけあれば足りる）。 *)
let rec ids_of_stm = function
  (* 位置情報の殻は素通し *)
  | Positioned (_, s) -> ids_of_stm s
  | Skip | Print _ -> []
  | Assign (o, _, e) -> ids_of_obj o @ ids_of_exp e
  | Swap (o1, o2) -> ids_of_obj o1 @ ids_of_obj o2
  | Conditional (e1, _, _, e2) -> ids_of_exp e1 @ ids_of_exp e2
  | Loop (e1, _, _, e2) -> ids_of_exp e1 @ ids_of_exp e2
  | For (x, e1, e2, _) -> x :: (ids_of_exp e1 @ ids_of_exp e2)
  | Switch (o1, _, _, o2) -> ids_of_obj o1 @ ids_of_obj o2
  | ObjectBlock (_, x, _) -> [ x ]
  | LocalBlock (_, x, e1, _, e2) -> x :: (ids_of_exp e1 @ ids_of_exp e2)
  | LocalCall (_, args) | LocalUncall (_, args) -> List.concat_map ids_of_arg args
  | ObjectCall (o, _, args) | ObjectUncall (o, _, args) ->
     ids_of_obj o @ List.concat_map ids_of_arg args
  | ObjectConstruction (_, o) | ObjectDestruction (_, o) -> ids_of_obj o
  | CopyReference (_, o1, o2) | UncopyReference (_, o1, o2) -> ids_of_obj o1 @ ids_of_obj o2
  | ArrayConstruction ((_, e), o) | ArrayDestruction ((_, e), o) -> ids_of_exp e @ ids_of_obj o
  | Show e -> ids_of_exp e

(**重複を除く（出現順は保つ）*)
let dedup l =
  List.rev (List.fold_left (fun acc x -> if List.mem x acc then acc else x :: acc) [] l)

(**エラーになった文に現れる変数の、その時点の値を "WHERE:" 行にする。
   eval.ml の文ラッパから呼ばれ、diagnostics 側で読み戻して表示する。 *)
let where_line stm env st =
  let visited = ref [] in
  let ids = dedup (ids_of_stm stm) |> List.filter (fun x -> x <> "this") in
  let shown =
    List.filter_map
      (fun x ->
        match List.assoc_opt x env with
        | None -> None
        | Some locs ->
           (match lookup st locs with
            | None -> None
            | Some v -> Some (x ^ " = " ^ describe ~depth:1 ~visited st v)))
      ids
  in
  (* 変数が無い文でも目印だけは残す。外側の文ラッパが自分の変数を
     上書きで付け足さないようにするため（has_where で判定する）。 *)
  "\n" ^ where_marker ^ String.concat "; " shown

(**メッセージに既に WHERE 行が付いているか*)
let has_where raw =
  List.exists
    (fun l -> index_of ~needle:where_marker (String.trim l) = Some 0)
    (split_lines raw)

(**終了時にゼロクリアされていない変数の一覧を作る。
   [result] は eval_prog が返す (識別子, 値) のリスト、[st] は最終ストア。 *)
let garbage_lines ?(limit = 40) result st =
  let visited = ref [] in
  let dirty = List.filter (fun (_, v) -> not (is_zero v)) result in
  let shown = List.filteri (fun i _ -> i < limit) dirty in
  let lines =
    List.map (fun (id, v) -> "  " ^ id ^ " = " ^ describe ~visited st v) shown
  in
  let omitted = List.length dirty - List.length shown in
  if omitted > 0
  then lines @ [ Printf.sprintf "  ... and %d more" omitted ]
  else lines

(**ゼロクリア検査の報告（PyJanus の "non-zero values remain" 警告に相当）*)
let garbage_report ?(limit = 40) result st =
  let total = List.length result in
  let dirty = List.filter (fun (_, v) -> not (is_zero v)) result in
  if dirty = [] then
    Printf.sprintf "ROOPL++ zero-clear check: all %d value(s) are zero-cleared (no garbage left)." total
  else
    String.concat "\n"
      ((Printf.sprintf "ROOPL++ zero-clear check: %d of %d value(s) are NOT zero-cleared:"
          (List.length dirty) total)
       :: garbage_lines ~limit result st
       @ [ "  note: a reversible program ends clean when every variable of the main class is";
           "        back to 0 / nil. Whatever is listed here is either the intended output or";
           "        leftover garbage -- objects and arrays above were allocated and never deleted." ])
