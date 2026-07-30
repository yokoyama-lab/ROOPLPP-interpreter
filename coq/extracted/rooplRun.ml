
type nat =
| O
| S of nat

(** val length : 'a1 list -> nat **)

let rec length = function
| [] -> O
| _::l' -> S (length l')

(** val app : 'a1 list -> 'a1 list -> 'a1 list **)

let rec app l m =
  match l with
  | [] -> m
  | a::l1 -> a::(app l1 m)

type comparison =
| Eq
| Lt
| Gt

(** val compOpp : comparison -> comparison **)

let compOpp = function
| Eq -> Eq
| Lt -> Gt
| Gt -> Lt

module Coq__1 = struct
 (** val add : nat -> nat -> nat **)

 let rec add n0 m =
   match n0 with
   | O -> m
   | S p -> S (add p m)
end
include Coq__1

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

module Nat =
 struct
  (** val eqb : nat -> nat -> bool **)

  let rec eqb n0 m =
    match n0 with
    | O -> (match m with
            | O -> true
            | S _ -> false)
    | S n' -> (match m with
               | O -> false
               | S m' -> eqb n' m')

  (** val leb : nat -> nat -> bool **)

  let rec leb n0 m =
    match n0 with
    | O -> true
    | S n' -> (match m with
               | O -> false
               | S m' -> leb n' m')

  (** val ltb : nat -> nat -> bool **)

  let ltb n0 m =
    leb (S n0) m

  (** val eq_dec : nat -> nat -> bool **)

  let rec eq_dec n0 m =
    match n0 with
    | O -> (match m with
            | O -> true
            | S _ -> false)
    | S n1 -> (match m with
               | O -> false
               | S n2 -> eq_dec n1 n2)
 end

module Pos =
 struct
  (** val succ : positive -> positive **)

  let rec succ = function
  | XI p -> XO (succ p)
  | XO p -> XI p
  | XH -> XO XH

  (** val add : positive -> positive -> positive **)

  let rec add x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> XO (add_carry p q)
       | XO q -> XI (add p q)
       | XH -> XO (succ p))
    | XO p ->
      (match y with
       | XI q -> XI (add p q)
       | XO q -> XO (add p q)
       | XH -> XI p)
    | XH -> (match y with
             | XI q -> XO (succ q)
             | XO q -> XI q
             | XH -> XO XH)

  (** val add_carry : positive -> positive -> positive **)

  and add_carry x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> XI (add_carry p q)
       | XO q -> XO (add_carry p q)
       | XH -> XI (succ p))
    | XO p ->
      (match y with
       | XI q -> XO (add_carry p q)
       | XO q -> XI (add p q)
       | XH -> XO (succ p))
    | XH ->
      (match y with
       | XI q -> XI (succ q)
       | XO q -> XO (succ q)
       | XH -> XI XH)

  (** val pred_double : positive -> positive **)

  let rec pred_double = function
  | XI p -> XI (XO p)
  | XO p -> XI (pred_double p)
  | XH -> XH

  (** val pred_N : positive -> n **)

  let pred_N = function
  | XI p -> Npos (XO p)
  | XO p -> Npos (pred_double p)
  | XH -> N0

  (** val mul : positive -> positive -> positive **)

  let rec mul x y =
    match x with
    | XI p -> add y (XO (mul p y))
    | XO p -> XO (mul p y)
    | XH -> y

  (** val compare_cont : comparison -> positive -> positive -> comparison **)

  let rec compare_cont r x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> compare_cont r p q
       | XO q -> compare_cont Gt p q
       | XH -> Gt)
    | XO p ->
      (match y with
       | XI q -> compare_cont Lt p q
       | XO q -> compare_cont r p q
       | XH -> Gt)
    | XH -> (match y with
             | XH -> r
             | _ -> Lt)

  (** val compare : positive -> positive -> comparison **)

  let compare =
    compare_cont Eq

  (** val eqb : positive -> positive -> bool **)

  let rec eqb p q =
    match p with
    | XI p0 -> (match q with
                | XI q0 -> eqb p0 q0
                | _ -> false)
    | XO p0 -> (match q with
                | XO q0 -> eqb p0 q0
                | _ -> false)
    | XH -> (match q with
             | XH -> true
             | _ -> false)

  (** val coq_Nsucc_double : n -> n **)

  let coq_Nsucc_double = function
  | N0 -> Npos XH
  | Npos p -> Npos (XI p)

  (** val coq_Ndouble : n -> n **)

  let coq_Ndouble = function
  | N0 -> N0
  | Npos p -> Npos (XO p)

  (** val coq_lxor : positive -> positive -> n **)

  let rec coq_lxor p q =
    match p with
    | XI p0 ->
      (match q with
       | XI q0 -> coq_Ndouble (coq_lxor p0 q0)
       | XO q0 -> coq_Nsucc_double (coq_lxor p0 q0)
       | XH -> Npos (XO p0))
    | XO p0 ->
      (match q with
       | XI q0 -> coq_Nsucc_double (coq_lxor p0 q0)
       | XO q0 -> coq_Ndouble (coq_lxor p0 q0)
       | XH -> Npos (XI p0))
    | XH ->
      (match q with
       | XI q0 -> Npos (XO q0)
       | XO q0 -> Npos (XI q0)
       | XH -> N0)

  (** val iter_op : ('a1 -> 'a1 -> 'a1) -> positive -> 'a1 -> 'a1 **)

  let rec iter_op op p a =
    match p with
    | XI p0 -> op a (iter_op op p0 (op a a))
    | XO p0 -> iter_op op p0 (op a a)
    | XH -> a

  (** val to_nat : positive -> nat **)

  let to_nat x =
    iter_op Coq__1.add x (S O)
 end

module N =
 struct
  (** val succ_pos : n -> positive **)

  let succ_pos = function
  | N0 -> XH
  | Npos p -> Pos.succ p

  (** val coq_lxor : n -> n -> n **)

  let coq_lxor n0 m =
    match n0 with
    | N0 -> m
    | Npos p -> (match m with
                 | N0 -> n0
                 | Npos q -> Pos.coq_lxor p q)
 end

module Z =
 struct
  (** val double : z -> z **)

  let double = function
  | Z0 -> Z0
  | Zpos p -> Zpos (XO p)
  | Zneg p -> Zneg (XO p)

  (** val succ_double : z -> z **)

  let succ_double = function
  | Z0 -> Zpos XH
  | Zpos p -> Zpos (XI p)
  | Zneg p -> Zneg (Pos.pred_double p)

  (** val pred_double : z -> z **)

  let pred_double = function
  | Z0 -> Zneg XH
  | Zpos p -> Zpos (Pos.pred_double p)
  | Zneg p -> Zneg (XI p)

  (** val pos_sub : positive -> positive -> z **)

  let rec pos_sub x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> double (pos_sub p q)
       | XO q -> succ_double (pos_sub p q)
       | XH -> Zpos (XO p))
    | XO p ->
      (match y with
       | XI q -> pred_double (pos_sub p q)
       | XO q -> double (pos_sub p q)
       | XH -> Zpos (Pos.pred_double p))
    | XH ->
      (match y with
       | XI q -> Zneg (XO q)
       | XO q -> Zneg (Pos.pred_double q)
       | XH -> Z0)

  (** val add : z -> z -> z **)

  let add x y =
    match x with
    | Z0 -> y
    | Zpos x' ->
      (match y with
       | Z0 -> x
       | Zpos y' -> Zpos (Pos.add x' y')
       | Zneg y' -> pos_sub x' y')
    | Zneg x' ->
      (match y with
       | Z0 -> x
       | Zpos y' -> pos_sub y' x'
       | Zneg y' -> Zneg (Pos.add x' y'))

  (** val opp : z -> z **)

  let opp = function
  | Z0 -> Z0
  | Zpos x0 -> Zneg x0
  | Zneg x0 -> Zpos x0

  (** val sub : z -> z -> z **)

  let sub m n0 =
    add m (opp n0)

  (** val mul : z -> z -> z **)

  let mul x y =
    match x with
    | Z0 -> Z0
    | Zpos x' ->
      (match y with
       | Z0 -> Z0
       | Zpos y' -> Zpos (Pos.mul x' y')
       | Zneg y' -> Zneg (Pos.mul x' y'))
    | Zneg x' ->
      (match y with
       | Z0 -> Z0
       | Zpos y' -> Zneg (Pos.mul x' y')
       | Zneg y' -> Zpos (Pos.mul x' y'))

  (** val compare : z -> z -> comparison **)

  let compare x y =
    match x with
    | Z0 -> (match y with
             | Z0 -> Eq
             | Zpos _ -> Lt
             | Zneg _ -> Gt)
    | Zpos x' -> (match y with
                  | Zpos y' -> Pos.compare x' y'
                  | _ -> Gt)
    | Zneg x' ->
      (match y with
       | Zneg y' -> compOpp (Pos.compare x' y')
       | _ -> Lt)

  (** val ltb : z -> z -> bool **)

  let ltb x y =
    match compare x y with
    | Lt -> true
    | _ -> false

  (** val eqb : z -> z -> bool **)

  let eqb x y =
    match x with
    | Z0 -> (match y with
             | Z0 -> true
             | _ -> false)
    | Zpos p -> (match y with
                 | Zpos q -> Pos.eqb p q
                 | _ -> false)
    | Zneg p -> (match y with
                 | Zneg q -> Pos.eqb p q
                 | _ -> false)

  (** val to_nat : z -> nat **)

  let to_nat = function
  | Zpos p -> Pos.to_nat p
  | _ -> O

  (** val of_N : n -> z **)

  let of_N = function
  | N0 -> Z0
  | Npos p -> Zpos p

  (** val coq_lxor : z -> z -> z **)

  let coq_lxor a b =
    match a with
    | Z0 -> b
    | Zpos a0 ->
      (match b with
       | Z0 -> a
       | Zpos b0 -> of_N (Pos.coq_lxor a0 b0)
       | Zneg b0 -> Zneg (N.succ_pos (N.coq_lxor (Npos a0) (Pos.pred_N b0))))
    | Zneg a0 ->
      (match b with
       | Z0 -> a
       | Zpos b0 -> Zneg (N.succ_pos (N.coq_lxor (Pos.pred_N a0) (Npos b0)))
       | Zneg b0 -> of_N (N.coq_lxor (Pos.pred_N a0) (Pos.pred_N b0)))
 end

(** val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list **)

let rec map f = function
| [] -> []
| a::l0 -> (f a)::(map f l0)

(** val in_dec : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> bool **)

let rec in_dec h a = function
| [] -> false
| y::l0 -> let s = h y a in if s then true else in_dec h a l0

type id = nat

type mid = nat

type loc = nat

type field = nat

type cid = nat

type state = { vs : (id -> z); os : (id -> loc option); hn : nat;
               hp : (loc -> field -> z); hc : (loc -> cid) }

(** val setv : state -> id -> z -> state **)

let setv a x v =
  { vs = (fun y -> if Nat.eqb x y then v else a.vs y); os = a.os; hn = a.hn;
    hp = a.hp; hc = a.hc }

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

(** val bval : bool -> z **)

let bval = function
| true -> Zpos XH
| false -> Z0

(** val eval_binop : binop -> z -> z -> z **)

let eval_binop o a b =
  match o with
  | Oadd -> Z.add a b
  | Osub -> Z.sub a b
  | Omul -> Z.mul a b
  | Oeq -> bval (Z.eqb a b)
  | Olt -> bval (Z.ltb a b)

(** val rdf : state -> id -> field -> z **)

let rdf a x f =
  match a.os x with
  | Some l -> if Nat.ltb l a.hn then a.hp l f else Z0
  | None -> Z0

(** val eval : exp -> state -> z **)

let rec eval e a =
  match e with
  | Cst z0 -> z0
  | Var x -> a.vs x
  | Fld (x, f) -> rdf a x f
  | Idx (x, e0) -> rdf a x (Z.to_nat (eval e0 a))
  | Bop (o, e1, e2) -> eval_binop o (eval e1 a) (eval e2 a)

(** val fv : exp -> id list **)

let rec fv = function
| Var x -> x::[]
| Idx (_, e0) -> fv e0
| Bop (_, e1, e2) -> app (fv e1) (fv e2)
| _ -> []

type modop =
| MAdd
| MSub
| MXor

(** val mapp : modop -> z -> z -> z **)

let mapp o a b =
  match o with
  | MAdd -> Z.add a b
  | MSub -> Z.sub a b
  | MXor -> Z.coq_lxor a b

(** val minv : modop -> modop **)

let minv = function
| MAdd -> MSub
| MSub -> MAdd
| MXor -> MXor

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

(** val invert : stm -> stm **)

let rec invert = function
| Sassign (x, o, e) -> Sassign (x, (minv o), e)
| Sfassign (x, f, o, e) -> Sfassign (x, f, (minv o), e)
| Saassign (x, ei, o, e) -> Saassign (x, ei, (minv o), e)
| Scopy (x, y) -> Suncopy (x, y)
| Suncopy (x, y) -> Scopy (x, y)
| Sseq (s1, s2) -> Sseq ((invert s2), (invert s1))
| Sif (e1, s1, s2, e2) -> Sif (e2, (invert s1), (invert s2), e1)
| Sloop (e1, s1, s2, e2) -> Sloop (e2, (invert s1), (invert s2), e1)
| Slocal (x, e1, s0, e2) -> Slocal (x, e2, (invert s0), e1)
| Sobj (c, x, s0) -> Sobj (c, x, (invert s0))
| Scall (m, args) -> Suncall (m, args)
| Suncall (m, args) -> Scall (m, args)
| Socall (x, m, args) -> Souncall (x, m, args)
| Souncall (x, m, args) -> Socall (x, m, args)
| x -> x

(** val mk_ren : id list -> id list -> id -> id **)

let rec mk_ren ps args x =
  match ps with
  | [] -> x
  | p::ps' ->
    (match args with
     | [] -> x
     | a::args' -> if Nat.eqb p x then a else mk_ren ps' args' x)

(** val rename_exp : (id -> id) -> exp -> exp **)

let rec rename_exp r = function
| Cst z0 -> Cst z0
| Var x -> Var (r x)
| Fld (x, f) -> Fld ((r x), f)
| Idx (x, e0) -> Idx ((r x), (rename_exp r e0))
| Bop (o, e1, e2) -> Bop (o, (rename_exp r e1), (rename_exp r e2))

(** val rename : (id -> id) -> stm -> stm **)

let rec rename r = function
| Sskip -> Sskip
| Sassign (x, o, e) -> Sassign ((r x), o, (rename_exp r e))
| Sfassign (x, f, o, e) -> Sfassign ((r x), f, o, (rename_exp r e))
| Saassign (x, ei, o, e) ->
  Saassign ((r x), (rename_exp r ei), o, (rename_exp r e))
| Sswap (x, y) -> Sswap ((r x), (r y))
| Saswap (x, e1, y, e2) ->
  Saswap ((r x), (rename_exp r e1), (r y), (rename_exp r e2))
| Soswap (x, y) -> Soswap ((r x), (r y))
| Scopy (x, y) -> Scopy ((r x), (r y))
| Suncopy (x, y) -> Suncopy ((r x), (r y))
| Sseq (s1, s2) -> Sseq ((rename r s1), (rename r s2))
| Sif (e1, s1, s2, e2) ->
  Sif ((rename_exp r e1), (rename r s1), (rename r s2), (rename_exp r e2))
| Sloop (e1, s1, s2, e2) ->
  Sloop ((rename_exp r e1), (rename r s1), (rename r s2), (rename_exp r e2))
| Slocal (x, e1, s', e2) ->
  Slocal ((r x), (rename_exp r e1), (rename r s'), (rename_exp r e2))
| Sshow e -> Sshow (rename_exp r e)
| Sobj (c, x, s') -> Sobj (c, (r x), (rename r s'))
| Scall (m, args) -> Scall (m, (map r args))
| Suncall (m, args) -> Suncall (m, (map r args))
| Socall (x, m, args) -> Socall ((r x), m, (map r args))
| Souncall (x, m, args) -> Souncall ((r x), m, (map r args))

type mdecl =
| MDecl of id list * stm

type cdecl =
| CDecl of cid option * (mid -> mdecl option)

type ctable = cid -> cdecl option

type menv = { procs : (mid -> mdecl option); classes : ctable }

(** val run : nat -> menv -> stm -> state -> state option **)

let rec run fuel g s a =
  match fuel with
  | O -> None
  | S k ->
    (match s with
     | Sskip -> Some a
     | Sassign (x, o, e) ->
       if in_dec Nat.eq_dec x (fv e)
       then None
       else Some (setv a x (mapp o (a.vs x) (eval e a)))
     | Sswap (x, y) -> Some (setv (setv a x (a.vs y)) y (a.vs x))
     | Sseq (s1, s2) ->
       (match run k g s1 a with
        | Some b -> run k g s2 b
        | None -> None)
     | Sif (e1, s1, s2, e2) ->
       if Z.eqb (eval e1 a) Z0
       then (match run k g s2 a with
             | Some b -> if Z.eqb (eval e2 b) Z0 then Some b else None
             | None -> None)
       else (match run k g s1 a with
             | Some b -> if Z.eqb (eval e2 b) Z0 then None else Some b
             | None -> None)
     | Sloop (e1, s1, s2, e2) ->
       if Z.eqb (eval e1 a) Z0
       then None
       else (match run k g s1 a with
             | Some b -> run_loop k g e1 s1 s2 e2 b
             | None -> None)
     | Slocal (x, e1, s', e2) ->
       if in_dec Nat.eq_dec x (fv e1)
       then None
       else if in_dec Nat.eq_dec x (fv e2)
            then None
            else (match run k g s' (setv a x (eval e1 a)) with
                  | Some b ->
                    if Z.eqb (b.vs x) (eval e2 b)
                    then Some (setv b x (a.vs x))
                    else None
                  | None -> None)
     | Sshow _ -> Some a
     | Scall (m, args) ->
       (match g.procs m with
        | Some m0 ->
          let MDecl (ps, body) = m0 in
          if Nat.eqb (length ps) (length args)
          then run k g (rename (mk_ren ps args) body) a
          else None
        | None -> None)
     | Suncall (m, args) ->
       (match g.procs m with
        | Some m0 ->
          let MDecl (ps, body) = m0 in
          if Nat.eqb (length ps) (length args)
          then run k g (invert (rename (mk_ren ps args) body)) a
          else None
        | None -> None)
     | _ -> None)

(** val run_loop :
    nat -> menv -> exp -> stm -> stm -> exp -> state -> state option **)

and run_loop fuel g e1 s1 s2 e2 a =
  match fuel with
  | O -> None
  | S k ->
    if Z.eqb (eval e2 a) Z0
    then (match run k g s2 a with
          | Some b ->
            if Z.eqb (eval e1 b) Z0
            then (match run k g s1 b with
                  | Some c -> run_loop k g e1 s1 s2 e2 c
                  | None -> None)
            else None
          | None -> None)
    else Some a

(** val for_up : id -> exp -> exp -> stm -> stm **)

let for_up x e1 e2 s =
  Slocal (x, e1, (Sloop ((Bop (Oeq, (Var x), e1)), s, (Sassign (x, MAdd, (Cst
    (Zpos XH)))), (Bop (Oeq, (Var x), e2)))), e2)

(** val for_down : id -> exp -> exp -> stm -> stm **)

let for_down x e1 e2 s =
  Slocal (x, e1, (Sloop ((Bop (Oeq, (Var x), e1)), s, (Sassign (x, MSub, (Cst
    (Zpos XH)))), (Bop (Oeq, (Var x), e2)))), e2)

(** val rev_switch : id -> ((z*stm)*z) list -> stm -> id -> stm **)

let rec rev_switch x cs d y =
  match cs with
  | [] -> d
  | p::tl ->
    let p0,w = p in
    let v,s = p0 in
    Sif ((Bop (Oeq, (Var x), (Cst v))), s, (rev_switch x tl d y), (Bop (Oeq,
    (Var y), (Cst w))))

(** val swap_case : ((z*stm)*z) -> (z*stm)*z **)

let swap_case = function
| p,w -> let v,s = p in (w,(invert s)),v
