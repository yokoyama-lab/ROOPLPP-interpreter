open OUnit2
open Syntax
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

      (* Control / non-printable bytes must NOT be emitted raw (a raw ESC would
         inject an ANSI sequence into -inverse output, a raw NUL would corrupt
         regenerated source). They are escaped as \DDD, which lexer.mll now
         re-lexes. UTF-8 bytes (>= 0x80) still pass through verbatim (above). *)
      "ESC (0x1b) becomes \\027" >:: (fun _ ->
        assert_equal ~printer:(fun x -> x) "\\027" (escape_string_literal "\x1b"));

      "NUL (0x00) becomes \\000" >:: (fun _ ->
        assert_equal ~printer:(fun x -> x) "\\000" (escape_string_literal "\x00"));

      "DEL (0x7f) becomes \\127" >:: (fun _ ->
        assert_equal ~printer:(fun x -> x) "\\127" (escape_string_literal "\x7f"));
    ]

(* pretty_exp must parenthesize a binary operand that is itself a binary
   expression, otherwise non-default-precedence grouping is lost when the
   -inverse output is re-parsed. Regression for the josephus example, whose
   `i * ((a + k) / i)` previously printed as `i * a + k / i`. *)
let exp_tests = "test suite for pretty.ml expression parenthesizing" >::: [
      "no spurious parens for a flat expression" >:: (fun _ ->
        assert_equal ~printer:(fun x -> x) "1 + 2"
          (pretty_exp (Binary(Add, Const 1, Const 2))));

      "grouping that changes the value is preserved" >:: (fun _ ->
        (* i * ((hist[i - 2] + k) / i) *)
        let e = Binary(Mul, Var "i",
                       Binary(Div,
                              Binary(Add, ArrayElement("hist", Binary(Sub, Var "i", Const 2)), Var "k"),
                              Var "i")) in
        assert_equal ~printer:(fun x -> x)
          "i * ((hist[i - 2] + k) / i)" (pretty_exp e));

      "left-nested same-precedence keeps grouping" >:: (fun _ ->
        (* (a - b) - c, which differs from a - (b - c) *)
        let e = Binary(Sub, Binary(Sub, Var "a", Var "b"), Var "c") in
        assert_equal ~printer:(fun x -> x) "(a - b) - c" (pretty_exp e));

      "array index is delimited by brackets, not parens" >:: (fun _ ->
        assert_equal ~printer:(fun x -> x) "xs[i + 1]"
          (pretty_exp (ArrayElement("xs", Binary(Add, Var "i", Const 1)))));
    ]

let _ =
  run_test_tt_main tests;
  run_test_tt_main exp_tests
