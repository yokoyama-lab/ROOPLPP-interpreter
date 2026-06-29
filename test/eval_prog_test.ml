open OUnit2
open Syntax
open Value
open Eval

(* End-to-end tests for the whole pipeline: lexer -> parser -> eval_prog.
   The other suites exercise eval_state/eval_exp on hand-built ASTs; this
   one starts from ROOPL++ source text, so it is the only coverage of
   lexer.mll / parser.mly and of eval_prog (main-method lookup, field
   environment/store construction, final result projection).

   It also checks the two reversibility properties that must hold for every
   parsed program:
     - double inversion is the identity:      invert (invert p) = p
     - pretty output re-parses to the same AST: parse (pretty p) = p
   The second guards the -inverse feature, whose output is fed back to the
   parser (see the pretty.ml string-escaping regression, 2026-06-03). *)

let parse src = Parser.main Lexer.token (Lexing.from_string src)
let run src = eval_prog (parse src)
let src_of (Prog cl) = Pretty.pretty_cl cl

(* ---- example programs with known results ---------------------------- *)

(* Fibonacci, mirroring example/fib.rplpp: fib(4) leaves xs[1] = 8 in
   result; n is restored to 4 by the symmetric uncall. *)
let fib_src = {|
class Fib
  int[] xs
  method init()
    new int[2] xs
  method fib(int n)
    if n = 0 then
      xs[0] ^= 1
      xs[1] ^= 1
    else
      n -= 1
      call fib(n)
      xs[0] += xs[1]
      xs[0] <=> xs[1]
    fi xs[0] = xs[1]
  method get(int out)
    out ^= xs[1]
class Program
  int result
  int n
  method main()
    n ^= 4
    local Fib f = nil
    new Fib f
    call f::init()
    call f::fib(n)
    call f::get(result)
    uncall f::fib(n)
    uncall f::init()
    delete Fib f
    delocal Fib f = nil
|}

(* sum_{i=1}^{10} i = 55, via a for loop inside a method called by reference. *)
let forsum_src = {|
class C
  method add(int n)
    for i in (1..10) do
      n += i
    end
class Program
  int result
  method main()
    local C c = nil
    new C c
    call c::add(result)
    delete C c
    delocal C c = nil
|}

(* if/fi with matching entry and exit assertions. *)
let cond_src = {|
class Program
  int x
  method main()
    x ^= 5
    if x = 5 then
      x += 10
    else
      x -= 10
    fi x = 15
|}

(* local/delocal block with arithmetic; result ends at 8, n is delocal'd away. *)
let local_src = {|
class Program
  int result
  method main()
    local int n = 5
    n += 3
    result ^= n
    delocal int n = 8
|}

let eval_tests = "eval_prog end-to-end" >::: [
    "fib(4) -> result=8, n restored to 4" >:: (fun _ ->
      assert_equal [("result", IntVal 8); ("n", IntVal 4)] (run fib_src));

    "for-loop sum 1..10 = 55" >:: (fun _ ->
      assert_equal [("result", IntVal 55)] (run forsum_src));

    "if/fi taken branch" >:: (fun _ ->
      assert_equal [("x", IntVal 15)] (run cond_src));

    "local block arithmetic" >:: (fun _ ->
      assert_equal [("result", IntVal 8)] (run local_src));
  ]

(* Exercises the two pretty-printer hazards that the reparse property must
   catch, neither of which the four programs above contain:
     - arithmetic whose grouping is lost without parentheses
       (result + n * ((k + n) / n) would re-parse differently if the inner
        parens were dropped — the josephus -inverse bug), and
     - a Print literal with non-ASCII (UTF-8) text and a control byte (ESC),
       which must survive escape_string_literal -> lexer round-tripping. *)
let roundtrip_src = {|
class Program
  int result
  int n
  int k
  method main()
    n ^= 6
    k ^= 1
    print("\027[1m走査\027[0m\n")
    local int t = result + n * ((k + n) / n)
    t += t
    delocal int t = (result + n * ((k + n) / n)) + t
|}

(* ---- reversibility properties over parsed programs ------------------ *)

let named_progs =
  [ "fib", fib_src; "forsum", forsum_src; "cond", cond_src; "local", local_src;
    "roundtrip", roundtrip_src ]

let double_invert_tests =
  "invert_prog is an involution" >::: List.map
    (fun (name, src) ->
      name >:: (fun _ ->
        let p = parse src in
        assert_equal p (Invert.invert_prog (Invert.invert_prog p))))
    named_progs

let reparse_tests =
  "pretty output re-parses to the same AST" >::: List.map
    (fun (name, src) ->
      name >:: (fun _ ->
        let p = parse src in
        assert_equal p (parse (src_of p))))
    named_progs

let _ =
  run_test_tt_main eval_tests;
  run_test_tt_main double_invert_tests;
  run_test_tt_main reparse_tests
