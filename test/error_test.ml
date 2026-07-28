open OUnit2

(* エラー経路のテスト。

   可逆言語では「表明が正しく落ちること」が意味論そのもの（ループの入口/出口
   表明、条件分岐の出口表明、delocal の一致、delete 前のゼロクリア、値引数の
   不変性）。それまで eval.ml のエラー送出 76 行のうち 71 行が一度も実行されて
   おらず、表明を素通しする回帰を検出できなかった。

   メッセージには文の pretty 表示が積まれるので、完全一致ではなく
   「ERROR 本文を含むこと」で検査する。 *)

let parse src = Parser.main Lexer.token (Lexing.from_string src)
let run src = ignore (Eval.eval_prog (parse src))

(* src を実行して、needle を含むエラーが起きることを確認する *)
let assert_error needle src =
  let got =
    try run src; None with
    | Util.Runtime_error e -> Some e
    | Failure e -> Some e
  in
  match got with
  | None -> assert_failure ("expected an error containing: " ^ needle)
  | Some e ->
     assert_bool
       (Printf.sprintf "expected %S in the error message, got:\n%s" needle e)
       (Diagnostics.contains ~needle e)

(* src が最後まで走ることを確認する（エラーを期待しない対照） *)
let assert_ok src = try run src with
  | Util.Runtime_error e | Failure e -> assert_failure ("unexpected error:\n" ^ e)

let prog body = "class Program\n int x\n int y\n int[] a\n method main()\n" ^ body

let case name needle src = name >:: (fun _ -> assert_error needle src)

let suite = "test suite for runtime error paths" >::: [

  (* ---- 配列 -------------------------------------------------------- *)
  case "array read out of bounds" "is out of bounds"
    (prog "  new int[2] a\n  x += a[2]\n");

  case "array write out of bounds" "is out of bounds"
    (prog "  new int[2] a\n  a[5] += 1\n");

  case "array index negative" "out of bounds"
    (prog "  new int[2] a\n  x -= 1\n  a[x] += 1\n");

  (* ---- 算術 -------------------------------------------------------- *)
  case "division by zero" "division by zero"
    (prog "  x += 1 / y\n");

  case "modulo by zero" "modulo by zero"
    (prog "  x += 1 % y\n");

  (* ---- 条件分岐の出口表明 ------------------------------------------ *)
  case "conditional exit assertion fails on the then branch" "Assertion should be true"
    (prog "  if x = 0 then\n   x += 1\n  else\n   skip\n  fi x = 0\n");

  case "conditional exit assertion fails on the else branch" "Assertion should be false"
    (prog "  if x != 0 then\n   skip\n  else\n   x += 1\n  fi x != 0\n");

  (* ---- ループの表明 ------------------------------------------------ *)
  case "loop entry assertion false on entry" "Assertion should be true"
    (prog "  x += 5\n  from x = 0 loop\n   x -= 1\n  until x = 0\n");

  case "loop entry assertion still true after an iteration" "Assertion should be false"
    (prog "  local int i = 0\n  from i >= 0 loop\n   i += 1\n  until i = 3\n  delocal int i = 3\n");

  (* ---- local / delocal --------------------------------------------- *)
  case "delocal value does not match" "But it should be"
    (prog "  local int t = 0\n  t += 3\n  x += t\n  delocal int t = 0\n");

  (* ---- delete のゼロクリア ------------------------------------------ *)
  case "delete array with a non-zero element" "All array elements is not zero-cleared"
    (prog "  new int[2] a\n  a[0] += 1\n  delete int[2] a\n");

  case "delete object with a non-zero field" "All instance field is not zero-cleared"
    ("class Cell\n int v\n method set(int n)\n  v += n\n\n"
     ^ prog "  local Cell c = nil\n  new Cell c\n  x += 1\n  call c::set(x)\n  delete Cell c\n  delocal Cell c = nil\n");

  (* ---- new / copy / uncopy ------------------------------------------ *)
  case "new on a variable that is not nil" "not nil"
    ("class Cell\n int v\n method noop()\n  skip\n\n"
     ^ prog "  local Cell c = nil\n  new Cell c\n  new Cell c\n  delocal Cell c = nil\n");

  case "copy target is not nil" "not nil"
    ("class Cell\n int v\n method noop()\n  skip\n\n"
     ^ prog "  local Cell c = nil\n  new Cell c\n  local Cell d = nil\n  new Cell d\n"
       ^ "  copy Cell c d\n  delocal Cell d = nil\n  delocal Cell c = nil\n");

  case "uncopy against a different reference" "both variable's reference is not same"
    ("class Cell\n int v\n method noop()\n  skip\n\n"
     ^ prog "  local Cell c = nil\n  new Cell c\n  local Cell d = nil\n  new Cell d\n"
       ^ "  uncopy Cell c d\n  delocal Cell d = nil\n  delocal Cell c = nil\n");

  (* ---- メソッド呼出し ---------------------------------------------- *)
  case "value argument is modified by the callee"
    "formal argument and actual argument are not same value"
    ("class T\n method bump(int n)\n  n += 1\n\n"
     ^ prog "  local T t = nil\n  new T t\n  call t::bump(3)\n  delete T t\n  delocal T t = nil\n");

  case "method does not exist" "does not exist"
    ("class T\n method here()\n  skip\n\n"
     ^ prog "  local T t = nil\n  new T t\n  call t::missing()\n  delete T t\n  delocal T t = nil\n");

  (* 引数の個数違いはメソッド探索の時点で落ちる（eval.ml の
     "mismatched argument list lengths" までは到達しない） *)
  case "argument list lengths do not match" "wrong number of arguments"
    ("class T\n method two(int m, int n)\n  m += n\n\n"
     ^ prog "  local T t = nil\n  new T t\n  call t::two(x)\n  delete T t\n  delocal T t = nil\n");

  case "method call on a nil object" "expected location value for object call"
    ("class T\n method noop()\n  skip\n\n"
     ^ prog "  local T t = nil\n  call t::noop()\n  delocal T t = nil\n");

  (* ---- 名前解決 ----------------------------------------------------- *)
  case "unbound variable" "unbound variable"
    (prog "  x += z\n");

  case "no main method" "was not found"
    "class C\n int v\n method notMain()\n  v += 1\n";

  case "unknown class in new" "is not valid"
    (prog "  local Missing m = nil\n  new Missing m\n  delocal Missing m = nil\n");

  (* ---- for --------------------------------------------------------- *)
  case "for loop variable is modified in the body" "must not change"
    (prog "  for i in (0..2) do\n   i += 1\n  end\n");

  (* ---- switch ------------------------------------------------------ *)
  (* break で閉じていない case へ分岐すると、break を探して case 列を
     走り抜けてしまう *)
  case "switch case without break runs off the case list" "no matching case"
    (prog "  x += 1\n  switch x\n   case 1 skip\n   default skip break\n  hctiws x\n");

  case "switch exit value selects another branch" "assertion is incorrect"
    (prog "  x += 1\n  switch x\n   case 1 x += 1 esac 5 break\n   default skip break\n  hctiws x\n");

  (* ---- 型の食い違い -------------------------------------------------- *)
  case "object used as an integer operand" "Integer value expected"
    ("class T\n method noop()\n  skip\n\n"
     ^ prog "  local T t = nil\n  new T t\n  x += t\n  delete T t\n  delocal T t = nil\n");

  case "array used where an integer is expected" "expected array value"
    (prog "  x += a[0]\n");

  case "for range is not an integer" "for range must be integer"
    ("class T\n method noop()\n  skip\n\n"
     ^ prog "  local T t = nil\n  new T t\n  for i in (t..1) do\n   skip\n  end\n"
       ^ "  delete T t\n  delocal T t = nil\n");

  (* ---- 対照: 正常に走るもの ------------------------------------------ *)
  "control: a clean program raises nothing" >:: (fun _ ->
    assert_ok (prog "  local int t = 0\n  t += 3\n  x += t\n  t -= 3\n  delocal int t = 0\n"));

  "control: a well-formed loop raises nothing" >:: (fun _ ->
    assert_ok (prog "  local int i = 0\n  from i = 0 loop\n   i += 1\n  until i = 3\n  delocal int i = 3\n  x += 1\n"));

  "control: delete after zero-clearing raises nothing" >:: (fun _ ->
    assert_ok (prog "  new int[2] a\n  a[0] += 1\n  a[0] -= 1\n  delete int[2] a\n"));
]

let _ = run_test_tt_main suite
