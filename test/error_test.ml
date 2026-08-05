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

  (* ---- 代入の副条件（可逆性そのもの） --------------------------------
     coq/roopl.v の E_assign は構文的な x ∉ fv(e) を、E_fassign / E_aassign /
     E_aswap は意味的な「書き込みが右辺（と添字）の値を変えない」を要求する。
     これが無いと x += x のように逆向きに戻せない文が通ってしまう。 *)
  case "assignment whose target occurs on the right" "must not occur on both sides"
    (prog "  x += 3\n  x += x\n");

  case "subtraction whose target occurs on the right" "must not occur on both sides"
    (prog "  x += 3\n  x -= x\n");

  case "assignment whose right-hand side reads the cell it writes"
    "must not be changed by the assignment itself"
    (prog "  new int[2] a\n  a[0] += 5\n  a[0] += a[0]\n");

  case "field assignment whose right-hand side reads the field it writes"
    "must not be changed by the assignment itself"
    ("class T\n int f\n method noop()\n  skip\n\n"
     ^ "class Program\n T t\n method main()\n"
     ^ "  new T t\n  t.f += 5\n  t.f += t.f\n");

  case "array swap that moves its own index"
    "must not be changed by the statement itself"
    (prog "  new int[2] a\n  a[0] += 1\n  a[a[0]] <=> a[0]\n");

  (* 局所ブロックの出口表明が自分自身を参照すると恒真になり、逆向きの実行が
     値を復元できない（coq/roopl.v の E_local の x ∉ fv(e2)） *)
  case "delocal expression mentions its own variable" "must not occur in its own"
    (prog "  local int t = 0\n  t += 3\n  x += t\n  delocal int t = t\n");

  case "local expression mentions its own variable" "must not occur in its own"
    (prog "  local int t = t\n  x += 1\n  delocal int t = 0\n");

  (* for は局所ブロック＋二重ガードのループの糖衣（coq/roopl.v の for_up）
     なので、E_local の条件がそのまま範囲式にかかる。範囲が自分自身を指すと
     出口の表明が恒真になる *)
  case "for range mentions the loop variable (upper end)"
    "must not mention the loop variable"
    (prog "  local int i = 3\n  for i in (0..i) do\n   x += 1\n  end\n"
     ^ "  delocal int i = 3\n");

  case "for range mentions the loop variable (lower end)"
    "must not mention the loop variable"
    (prog "  local int i = 0\n  for i in (i..3) do\n   x += 1\n  end\n"
     ^ "  delocal int i = 0\n");

  (* ---- 配列の要素数と条件式（内部メッセージが漏れていた経路） ----------
     new int[-1] は gen_locsvec が n <> 0 で再帰していたため Stack_overflow で
     クラッシュしていた（終了コード 2）。new int[0] は lookup_vec の内部
     メッセージが出ていた *)
  case "array size is negative" "Array size must be at least 1"
    (prog "  new int[-1] a\n");

  case "array size is zero" "Array size must be at least 1"
    (prog "  new int[0] a\n");

  case "delete with a non-positive size" "Array size must be at least 1"
    (prog "  new int[2] a\n  delete int[0] a\n");

  (* 条件式に整数でない値（配列やオブジェクト）を置くと isTrue の内部
     メッセージが出ていた *)
  case "a condition that is not an integer" "Integer value expected in the condition"
    (prog "  new int[2] a\n  if a then\n   skip\n  fi a\n");

  case "a loop condition that is not an integer" "Integer value expected in the condition"
    (prog "  new int[2] a\n  from a loop\n   skip\n  until a\n");

  (* ---- ドットの右側の添字はどのスコープか ----------------------------
     o.xs[k] の添字 k は**呼び出し側**の k であって、オブジェクトのフィールド k
     ではない。以前は l 値の解決がフィールド側の環境で右辺全体を評価していて、
     内側の k を拾っていた *)
  case "an out-of-bounds index through a field" "Array index xs[5] is out of bounds"
    ("class Box\n int[] xs\n int k\n method init()\n  new int[3] xs\n  k += 2\n  xs[0] += 100\n  xs[1] += 200\n  xs[2] += 300\n\nclass Program\n int r\n int k\n Box b\n method main()\n  new Box b\n  call b::init()\n  k += 1\n  r += b.xs[5]\n");

  (* ---- 内部的なメッセージが漏れていた経路（利用者向けに書き直した） ---- *)
  case "field access on an integer" "Field access needs an object on the left of the dot"
    ("class Box\n int f\n method noop()\n  skip\n\nclass Program\n int x\n method main()\n  x += x.f\n");

  case "field access on a nil object" "Field access needs an object on the left of the dot"
    ("class Box\n int f\n method noop()\n  skip\n\nclass Program\n Box b\n int x\n method main()\n  x += b.f\n");

  case "delete on a nil object" "delete needs an allocated object"
    ("class Box\n int f\n method noop()\n  skip\n\nclass Program\n Box b\n method main()\n  delete Box b\n");

  case "delete on an array variable" "delete needs an allocated object"
    ("class Box\n int f\n method noop()\n  skip\n\nclass Program\n int[] a\n method main()\n  new int[2] a\n  delete Box a\n");

  (* ---- copy / uncopy の自己別名（E_copy / E_uncopy の x ≠ y） ----------
     uncopy int x x は x の値を消してしまい、逆向きに走らせても戻らない。
     オブジェクトだと参照が消えて、確保済みのオブジェクトが回収不能になる
     （しかもゼロクリア検査は「garbage なし」と報告してしまう） *)
  case "uncopy of an integer variable with itself" "two different variables"
    (prog "  x += 1\n  uncopy int x x\n");

  case "copy of a variable with itself" "two different variables"
    ("class Box\n int f\n method noop()\n  skip\n\n"
     ^ "class Program\n Box b\n method main()\n  new Box b\n  copy Box b b\n");

  case "uncopy of an object with itself" "two different variables"
    ("class Box\n int f\n method noop()\n  skip\n\n"
     ^ "class Program\n Box b\n method main()\n  new Box b\n  uncopy Box b b\n");

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

  case "method call on a nil object" "The receiver of this call is nil or not an object"
    ("class T\n method noop()\n  skip\n\n"
     ^ prog "  local T t = nil\n  call t::noop()\n  delocal T t = nil\n");

  (* ---- 名前解決 ----------------------------------------------------- *)
  case "unbound variable" "unbound variable"
    (prog "  x += z\n");

  case "no main method" "was not found"
    "class C\n int v\n method notMain()\n  v += 1\n";

  case "unknown class in new" "is not declared in this program"
    (prog "  local Missing m = nil\n  new Missing m\n  delocal Missing m = nil\n");

  (* ---- for --------------------------------------------------------- *)
  case "for loop variable is modified in the body" "must not change"
    (prog "  for i in (0..2) do\n   i += 1\n  end\n");

  (* ループ変数の不変性は最初の 1 周だけでなく毎周検査する。ここを見逃すと
     次の周回で書き換えを上書きしてしまい、逆向きの実行が同じ道をたどらない *)
  case "for loop variable is modified from the second iteration on"
    "must not change"
    (prog "  for i in (1..3) do\n   if i = 2 then\n    i += 5\n   else\n    skip\n   fi i = 7\n  end\n");

  (* 範囲式も体で変わってはならない（逆は for i in (e2..e1)） *)
  case "for range is modified in the body" "range of this for statement"
    (prog "  x += 3\n  for i in (1..x) do\n   x += 1\n  end\n");

  (* ---- switch ------------------------------------------------------ *)
  (* break で閉じていない case へ分岐すると、break を探して case 列を
     走り抜けてしまう *)
  case "switch case without break runs off the case list" "no matching case"
    (prog "  x += 1\n  switch x\n   case 1 skip\n   default skip break\n  hctiws x\n");

  case "switch exit value selects another branch" "assertion is incorrect"
    (prog "  x += 1\n  switch x\n   case 1 x += 1 esac 5 break\n   default skip break\n  hctiws x\n");

  (* 出口の値が枝を識別できなければ、逆向きの実行が枝を選び直せない *)
  case "switch exit values do not distinguish the branches" "was not taken"
    (prog "  x += 2\n  switch x\n   case 1 y += 10 esac 10 break\n   case 2 y += 10 esac 10 break\n   default skip break\n  hctiws y\n");

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

  (* 別のセルを読む代入・添字に使う変数への代入は可逆なので通る *)
  "control: an assignment reading another cell raises nothing" >:: (fun _ ->
    assert_ok (prog ("  new int[2] a\n  a[1] += 5\n  a[0] += a[1]\n"
                     ^ "  a[0] -= 5\n  a[1] -= 5\n  delete int[2] a\n")));

  "control: an assignment whose index mentions a variable raises nothing" >:: (fun _ ->
    assert_ok (prog ("  new int[2] a\n  x += 1\n  a[x] += x\n"
                     ^ "  a[x] -= 1\n  x -= 1\n  delete int[2] a\n")));
]

let _ = run_test_tt_main suite
