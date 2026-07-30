
type nat =
| O
| S of nat

val length : 'a1 list -> nat

val app : 'a1 list -> 'a1 list -> 'a1 list

type comparison =
| Eq
| Lt
| Gt

val compOpp : comparison -> comparison

val add : nat -> nat -> nat

type positive =
| XI of positive
| XO of positive
| XH

type n =
| N0
| Npos of positive

type z =
| Z0
| Zpos of positive
| Zneg of positive

module Nat :
 sig
  val eqb : nat -> nat -> bool

  val leb : nat -> nat -> bool

  val ltb : nat -> nat -> bool

  val eq_dec : nat -> nat -> bool
 end

module Pos :
 sig
  val succ : positive -> positive

  val add : positive -> positive -> positive

  val add_carry : positive -> positive -> positive

  val pred_double : positive -> positive

  val pred_N : positive -> n

  val mul : positive -> positive -> positive

  val compare_cont : comparison -> positive -> positive -> comparison

  val compare : positive -> positive -> comparison

  val eqb : positive -> positive -> bool

  val coq_Nsucc_double : n -> n

  val coq_Ndouble : n -> n

  val coq_lxor : positive -> positive -> n

  val iter_op : ('a1 -> 'a1 -> 'a1) -> positive -> 'a1 -> 'a1

  val to_nat : positive -> nat
 end

module N :
 sig
  val succ_pos : n -> positive

  val coq_lxor : n -> n -> n
 end

module Z :
 sig
  val double : z -> z

  val succ_double : z -> z

  val pred_double : z -> z

  val pos_sub : positive -> positive -> z

  val add : z -> z -> z

  val opp : z -> z

  val sub : z -> z -> z

  val mul : z -> z -> z

  val compare : z -> z -> comparison

  val ltb : z -> z -> bool

  val eqb : z -> z -> bool

  val to_nat : z -> nat

  val of_N : n -> z

  val coq_lxor : z -> z -> z
 end

val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list

val in_dec : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> bool

type id = nat

type mid = nat

type loc = nat

type field = nat

type cid = nat

type state = { vs : (id -> z); os : (id -> loc option); hn : nat;
               hp : (loc -> field -> z); hc : (loc -> cid) }

val setv : state -> id -> z -> state

type binop =
| Oadd
| Osub
| Omul
| Oeq
| Olt

type exp =
| Cst of z
| Var of id
| Fld of id * field
| Idx of id * exp
| Bop of binop * exp * exp

val bval : bool -> z

val eval_binop : binop -> z -> z -> z

val rdf : state -> id -> field -> z

val eval : exp -> state -> z

val fv : exp -> id list

type modop =
| MAdd
| MSub
| MXor

val mapp : modop -> z -> z -> z

val minv : modop -> modop

type stm =
| Sskip
| Sassign of id * modop * exp
| Sfassign of id * field * modop * exp
| Saassign of id * exp * modop * exp
| Sswap of id * id
| Saswap of id * exp * id * exp
| Soswap of id * id
| Scopy of id * id
| Suncopy of id * id
| Sseq of stm * stm
| Sif of exp * stm * stm * exp
| Sloop of exp * stm * stm * exp
| Slocal of id * exp * stm * exp
| Sshow of exp
| Sobj of cid * id * stm
| Scall of mid * id list
| Suncall of mid * id list
| Socall of id * mid * id list
| Souncall of id * mid * id list

val invert : stm -> stm

val mk_ren : id list -> id list -> id -> id

val rename_exp : (id -> id) -> exp -> exp

val rename : (id -> id) -> stm -> stm

type mdecl =
| MDecl of id list * stm

type cdecl =
| CDecl of cid option * (mid -> mdecl option)

type ctable = cid -> cdecl option

type menv = { procs : (mid -> mdecl option); classes : ctable }

val run : nat -> menv -> stm -> state -> state option

val run_loop :
  nat -> menv -> exp -> stm -> stm -> exp -> state -> state option

val for_up : id -> exp -> exp -> stm -> stm

val for_down : id -> exp -> exp -> stm -> stm

val rev_switch : id -> ((z*stm)*z) list -> stm -> id -> stm

val swap_case : ((z*stm)*z) -> (z*stm)*z
