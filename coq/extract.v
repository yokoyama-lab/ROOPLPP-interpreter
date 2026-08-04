(** OCaml 抽出：検証済みインタプリタ [run] を取り出す。

    生成物 extracted/rooplRun.ml はリポジトリにコミットしてある（OCaml 側の
    ビルドに Rocq を要求しないため）。再生成は `make extract`。 *)
Require Import ROOPL.roopl.
Require Import Extraction.

Extraction Language OCaml.
Set Extraction Output Directory "extracted".

Extract Inductive bool => "bool" [ "true" "false" ].
Extract Inductive option => "option" [ "Some" "None" ].
Extract Inductive list => "list" [ "[]" "(::)" ].
Extract Inductive prod => "(*)" [ "(,)" ].
Extract Inductive sumbool => "bool" [ "true" "false" ].

(* Z と nat は Coq の表現のまま出す（int へ写すと桁溢れで健全性が壊れる）。 *)
(* for / switch の糖衣も取り出す。差分テスト (test/extracted_test.ml) が
   「実装の for/switch」と「形式化での糖衣」を突き合わせるのに使う。 *)
Extraction "rooplRun.ml" run invert for_up for_down rev_switch swap_case.
