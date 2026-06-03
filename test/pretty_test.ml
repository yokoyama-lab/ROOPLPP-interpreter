open OUnit2
open Pretty

(* Regression tests for the string-literal escaping used by -inverse output.
   Bug (reversibility audit, 2026-06-03): pretty.ml printed Print(str) with
   OCaml String.escaped, which renders bytes >= 128 (UTF-8 / Japanese text)
   as decimal escapes that lexer.mll cannot re-lex, so the inverse output
   could not be parsed back. escape_string_literal must emit only the escapes
   the lexer accepts (quote, backslash, newline, tab) and pass every other
   byte through verbatim. *)

let tests = "test suite for pretty.ml string escaping" >::: [
      (* The key regression: non-ASCII (UTF-8) bytes must pass through raw,
         not become decimal escapes. Six UTF-8 bytes here. *)
      "non-ASCII bytes are verbatim" >:: (fun _ ->
        let s = "\xe8\xb5\xb0\xe6\x9f\xbb" in
        assert_equal ~printer:(fun x -> x) s (escape_string_literal s));

      "double quote is escaped" >:: (fun _ ->
        assert_equal ~printer:(fun x -> x) "a\\\"b" (escape_string_literal "a\"b"));

      "backslash is escaped" >:: (fun _ ->
        assert_equal ~printer:(fun x -> x) "a\\\\b" (escape_string_literal "a\\b"));

      "newline becomes backslash-n" >:: (fun _ ->
        assert_equal ~printer:(fun x -> x) "a\\nb" (escape_string_literal "a\nb"));

      "tab becomes backslash-t" >:: (fun _ ->
        assert_equal ~printer:(fun x -> x) "a\\tb" (escape_string_literal "a\tb"));

      "plain ASCII is unchanged" >:: (fun _ ->
        assert_equal ~printer:(fun x -> x) "hello" (escape_string_literal "hello"));
    ]

let _ = run_test_tt_main tests
