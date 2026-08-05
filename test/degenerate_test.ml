open OUnit2

(* 退化した入力の掃引。

   2026-08-05 の監査で見つけた 8 件のバグは、どれも「変な入力を実際に流して
   出力を見る」ことで見つかった。手法そのものをテストにして固定する。

   `test/degenerate/*.rplpp` を全部走らせて、次の 2 つを要求する。

   1. **落ちるとしても行儀よく落ちること。** 素の例外（Stack_overflow、
      Not_found など）で死んではいけない。`new int[-1]` が
      `Fatal error: exception Stack_overflow` で終了コード 2 を返していたのは
      これで捕まる。

   2. **実装の内部事情がメッセージに漏れないこと。** 利用者に
      `index out of bounds in lookup_vec` や `ERROR in isTrue` を見せない。
      内部の語彙（下の [internal_vocabulary]）が出たら失敗させる。

   3. **最後まで走るなら可逆であること。** `本体 ; その逆` で初期状態へ戻る。
      可逆性のバグ（`call m(x,x)` の別名、`uncopy x x`、`x += x`）はどれも
      「落ちずに黙って受理される」形なので、1 と 2 では捕まらない。ここが
      効く。

   新しい構文や検査を足したときは、退化させた入力をここへ追加する。 *)

let dir =
  (* dune test は _build/default/test を、dune exec はリポジトリ直下を CWD に
     するので、両方から見つかる候補を並べる *)
  let candidates =
    [ "degenerate"; "test/degenerate"; "../test/degenerate"; "../../test/degenerate" ] in
  try List.find (fun d -> Sys.file_exists (Filename.concat d "div_by_zero.rplpp"))
        candidates
  with Not_found -> assert_failure "test/degenerate/ が見つからない"

let programs =
  Sys.readdir dir |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f ".rplpp")
  |> List.sort compare

let read_file path =
  let ch = open_in_bin path in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch; s

(* 実装の内部でしか意味がない語。利用者向けメッセージに出てはいけない *)
let internal_vocabulary =
  [ "lookup_vec"; "isTrue"; "gen_st"; "gen_locsvec"; "lval_val";
    "LocsVal"; "LocsVec"; "IntVal"; "ObjVal"; "l-value";
    "not implemented"; "empty environment"; "unbound locations" ]

let parse src = Parser.main Lexer.token (Lexing.from_string src)

let run_source src = ignore (Eval.eval_prog (parse src))

(* main の本体を「本体 ; その逆」に差し替える（example_test と同じ手口） *)
let round_trip_prog (Syntax.Prog cl) =
  Syntax.Prog
    (List.map
       (fun (Syntax.CDecl (tid, inh, fields, methods)) ->
         Syntax.CDecl (tid, inh, fields,
                List.map
                  (fun (Syntax.MDecl (mid, para, stml)) ->
                    if mid = "main"
                    then Syntax.MDecl (mid, para, stml @ Invert.invert stml)
                    else Syntax.MDecl (mid, para, stml))
                  methods))
       cl)

let check name =
  let src = read_file (Filename.concat dir name) in
  let outcome =
    try run_source src; `Ok with
    | Util.Runtime_error m | Failure m -> `Rejected m
    | Util.Parse_error _ -> `Rejected "(parse error)"
    (* ここに来るのは素の例外で死んだということ。Stack_overflow や Not_found
       が利用者に見えてはいけない *)
    | e -> `Crashed (Printexc.to_string e)
  in
  match outcome with
  | `Crashed e ->
     assert_failure
       (Printf.sprintf "%s: 行儀よく落ちていない（素の例外が出た）: %s" name e)
  | `Ok ->
     (* 走るなら可逆でなければならない *)
     (match (try Some (Eval.eval_prog (round_trip_prog (parse src))) with
             | Util.Runtime_error _ | Failure _ -> None) with
      | None ->
         assert_failure
           (Printf.sprintf
              "%s: 順方向は通るのに「本体 ; その逆」が落ちる（可逆でない）" name)
      | Some result ->
         (match List.filter (fun (_, v) -> not (Diagnostics.is_zero v)) result with
          | [] -> ()
          | dirty ->
             assert_failure
               (Printf.sprintf
                  "%s: 順方向は通るのに往復で初期状態へ戻らない（%s が残った）"
                  name (String.concat ", " (List.map fst dirty)))))
  | `Rejected m ->
     List.iter
       (fun w ->
         if Diagnostics.contains ~needle:w m then
           assert_failure
             (Printf.sprintf
                "%s: 内部の語 %S が利用者向けメッセージに出ている:\n%s" name w m))
       internal_vocabulary

(* 対照: ok_ で始まるものは最後まで走らなければならない *)
let check_ok name =
  let src = read_file (Filename.concat dir name) in
  try run_source src with
  | Util.Runtime_error m | Failure m ->
     assert_failure (Printf.sprintf "%s: 通るはずが落ちた:\n%s" name m)

let suite = "test suite for degenerate inputs" >::: [
      "the corpus is not empty" >:: (fun _ ->
        assert_bool "退化入力が置かれている" (List.length programs >= 20));

      "no degenerate input crashes or leaks internals"
      >::: List.map (fun n -> n >:: (fun _ -> check n)) programs;

      "the controls still run to completion"
      >::: List.map (fun n -> n >:: (fun _ -> check_ok n))
             (List.filter (fun n -> String.length n > 3 && String.sub n 0 3 = "ok_")
                programs);
    ]

let _ = run_test_tt_main suite
