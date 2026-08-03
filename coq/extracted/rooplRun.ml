
(** val negb : bool -> bool **)

let negb = function
| true -> false
| false -> true

type nat =
| O
| S of nat

(** val fst : ('a1*'a2) -> 'a1 **)

let fst = function
| x,_ -> x

(** val snd : ('a1*'a2) -> 'a2 **)

let snd = function
| _,y -> y

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

(** val pred : nat -> nat **)

let pred n0 = match n0 with
| O -> n0
| S u -> u

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

  (** val max : nat -> nat -> nat **)

  let rec max n0 m =
    match n0 with
    | O -> m
    | S n' -> (match m with
               | O -> n0
               | S m' -> S (max n' m'))

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

  type mask =
  | IsNul
  | IsPos of positive
  | IsNeg

  (** val succ_double_mask : mask -> mask **)

  let succ_double_mask = function
  | IsNul -> IsPos XH
  | IsPos p -> IsPos (XI p)
  | IsNeg -> IsNeg

  (** val double_mask : mask -> mask **)

  let double_mask = function
  | IsPos p -> IsPos (XO p)
  | x0 -> x0

  (** val double_pred_mask : positive -> mask **)

  let double_pred_mask = function
  | XI p -> IsPos (XO (XO p))
  | XO p -> IsPos (XO (pred_double p))
  | XH -> IsNul

  (** val sub_mask : positive -> positive -> mask **)

  let rec sub_mask x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> double_mask (sub_mask p q)
       | XO q -> succ_double_mask (sub_mask p q)
       | XH -> IsPos (XO p))
    | XO p ->
      (match y with
       | XI q -> succ_double_mask (sub_mask_carry p q)
       | XO q -> double_mask (sub_mask p q)
       | XH -> IsPos (pred_double p))
    | XH -> (match y with
             | XH -> IsNul
             | _ -> IsNeg)

  (** val sub_mask_carry : positive -> positive -> mask **)

  and sub_mask_carry x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> succ_double_mask (sub_mask_carry p q)
       | XO q -> double_mask (sub_mask p q)
       | XH -> IsPos (pred_double p))
    | XO p ->
      (match y with
       | XI q -> double_mask (sub_mask_carry p q)
       | XO q -> succ_double_mask (sub_mask_carry p q)
       | XH -> double_pred_mask p)
    | XH -> IsNeg

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

  (** val coq_lor : positive -> positive -> positive **)

  let rec coq_lor p q =
    match p with
    | XI p0 ->
      (match q with
       | XI q0 -> XI (coq_lor p0 q0)
       | XO q0 -> XI (coq_lor p0 q0)
       | XH -> p)
    | XO p0 ->
      (match q with
       | XI q0 -> XI (coq_lor p0 q0)
       | XO q0 -> XO (coq_lor p0 q0)
       | XH -> XI p0)
    | XH -> (match q with
             | XO q0 -> XI q0
             | _ -> q)

  (** val coq_land : positive -> positive -> n **)

  let rec coq_land p q =
    match p with
    | XI p0 ->
      (match q with
       | XI q0 -> coq_Nsucc_double (coq_land p0 q0)
       | XO q0 -> coq_Ndouble (coq_land p0 q0)
       | XH -> Npos XH)
    | XO p0 ->
      (match q with
       | XI q0 -> coq_Ndouble (coq_land p0 q0)
       | XO q0 -> coq_Ndouble (coq_land p0 q0)
       | XH -> N0)
    | XH -> (match q with
             | XO _ -> N0
             | _ -> Npos XH)

  (** val ldiff : positive -> positive -> n **)

  let rec ldiff p q =
    match p with
    | XI p0 ->
      (match q with
       | XI q0 -> coq_Ndouble (ldiff p0 q0)
       | XO q0 -> coq_Nsucc_double (ldiff p0 q0)
       | XH -> Npos (XO p0))
    | XO p0 ->
      (match q with
       | XI q0 -> coq_Ndouble (ldiff p0 q0)
       | XO q0 -> coq_Ndouble (ldiff p0 q0)
       | XH -> Npos p)
    | XH -> (match q with
             | XO _ -> Npos XH
             | _ -> N0)

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
  (** val succ_double : n -> n **)

  let succ_double = function
  | N0 -> Npos XH
  | Npos p -> Npos (XI p)

  (** val double : n -> n **)

  let double = function
  | N0 -> N0
  | Npos p -> Npos (XO p)

  (** val succ_pos : n -> positive **)

  let succ_pos = function
  | N0 -> XH
  | Npos p -> Pos.succ p

  (** val sub : n -> n -> n **)

  let sub n0 m =
    match n0 with
    | N0 -> N0
    | Npos n' ->
      (match m with
       | N0 -> n0
       | Npos m' ->
         (match Pos.sub_mask n' m' with
          | Pos.IsPos p -> Npos p
          | _ -> N0))

  (** val compare : n -> n -> comparison **)

  let compare n0 m =
    match n0 with
    | N0 -> (match m with
             | N0 -> Eq
             | Npos _ -> Lt)
    | Npos n' -> (match m with
                  | N0 -> Gt
                  | Npos m' -> Pos.compare n' m')

  (** val leb : n -> n -> bool **)

  let leb x y =
    match compare x y with
    | Gt -> false
    | _ -> true

  (** val pos_div_eucl : positive -> n -> n*n **)

  let rec pos_div_eucl a b =
    match a with
    | XI a' ->
      let q,r = pos_div_eucl a' b in
      let r' = succ_double r in
      if leb b r' then (succ_double q),(sub r' b) else (double q),r'
    | XO a' ->
      let q,r = pos_div_eucl a' b in
      let r' = double r in
      if leb b r' then (succ_double q),(sub r' b) else (double q),r'
    | XH ->
      (match b with
       | N0 -> N0,(Npos XH)
       | Npos p -> (match p with
                    | XH -> (Npos XH),N0
                    | _ -> N0,(Npos XH)))

  (** val coq_lor : n -> n -> n **)

  let coq_lor n0 m =
    match n0 with
    | N0 -> m
    | Npos p -> (match m with
                 | N0 -> n0
                 | Npos q -> Npos (Pos.coq_lor p q))

  (** val coq_land : n -> n -> n **)

  let coq_land n0 m =
    match n0 with
    | N0 -> N0
    | Npos p -> (match m with
                 | N0 -> N0
                 | Npos q -> Pos.coq_land p q)

  (** val ldiff : n -> n -> n **)

  let ldiff n0 m =
    match n0 with
    | N0 -> N0
    | Npos p -> (match m with
                 | N0 -> n0
                 | Npos q -> Pos.ldiff p q)

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

  (** val leb : z -> z -> bool **)

  let leb x y =
    match compare x y with
    | Gt -> false
    | _ -> true

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

  (** val quotrem : z -> z -> z*z **)

  let quotrem a b =
    match a with
    | Z0 -> Z0,Z0
    | Zpos a0 ->
      (match b with
       | Z0 -> Z0,a
       | Zpos b0 -> let q,r = N.pos_div_eucl a0 (Npos b0) in (of_N q),(of_N r)
       | Zneg b0 ->
         let q,r = N.pos_div_eucl a0 (Npos b0) in (opp (of_N q)),(of_N r))
    | Zneg a0 ->
      (match b with
       | Z0 -> Z0,a
       | Zpos b0 ->
         let q,r = N.pos_div_eucl a0 (Npos b0) in
         (opp (of_N q)),(opp (of_N r))
       | Zneg b0 ->
         let q,r = N.pos_div_eucl a0 (Npos b0) in (of_N q),(opp (of_N r)))

  (** val quot : z -> z -> z **)

  let quot a b =
    fst (quotrem a b)

  (** val rem : z -> z -> z **)

  let rem a b =
    snd (quotrem a b)

  (** val coq_lor : z -> z -> z **)

  let coq_lor a b =
    match a with
    | Z0 -> b
    | Zpos a0 ->
      (match b with
       | Z0 -> a
       | Zpos b0 -> Zpos (Pos.coq_lor a0 b0)
       | Zneg b0 -> Zneg (N.succ_pos (N.ldiff (Pos.pred_N b0) (Npos a0))))
    | Zneg a0 ->
      (match b with
       | Z0 -> a
       | Zpos b0 -> Zneg (N.succ_pos (N.ldiff (Pos.pred_N a0) (Npos b0)))
       | Zneg b0 ->
         Zneg (N.succ_pos (N.coq_land (Pos.pred_N a0) (Pos.pred_N b0))))

  (** val coq_land : z -> z -> z **)

  let coq_land a b =
    match a with
    | Z0 -> Z0
    | Zpos a0 ->
      (match b with
       | Z0 -> Z0
       | Zpos b0 -> of_N (Pos.coq_land a0 b0)
       | Zneg b0 -> of_N (N.ldiff (Npos a0) (Pos.pred_N b0)))
    | Zneg a0 ->
      (match b with
       | Z0 -> Z0
       | Zpos b0 -> of_N (N.ldiff (Npos b0) (Pos.pred_N a0))
       | Zneg b0 ->
         Zneg (N.succ_pos (N.coq_lor (Pos.pred_N a0) (Pos.pred_N b0))))

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

(** val seq : nat -> nat -> nat list **)

let rec seq start = function
| O -> []
| S len0 -> start::(seq (S start) len0)

(** val in_dec : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> bool **)

let rec in_dec h a = function
| [] -> false
| y::l0 -> let s = h y a in if s then true else in_dec h a l0

(** val forallb : ('a1 -> bool) -> 'a1 list -> bool **)

let rec forallb f = function
| [] -> true
| a::l0 -> if f a then forallb f l0 else false

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

(** val seto : state -> id -> loc option -> state **)

let seto a x r =
  { vs = a.vs; os = (fun y -> if Nat.eqb x y then r else a.os y); hn = a.hn;
    hp = a.hp; hc = a.hc }

(** val setf : state -> loc -> field -> z -> state **)

let setf a l f v =
  { vs = a.vs; os = a.os; hn = a.hn; hp = (fun l' f' ->
    if if Nat.eqb l l' then Nat.eqb f f' else false then v else a.hp l' f');
    hc = a.hc }

(** val alloc : state -> cid -> id -> state **)

let alloc a c x =
  { vs = a.vs; os = (fun y -> if Nat.eqb x y then Some a.hn else a.os y);
    hn = (S a.hn); hp = (fun l f -> if Nat.eqb l a.hn then Z0 else a.hp l f);
    hc = (fun l -> if Nat.eqb l a.hn then c else a.hc l) }

(** val dealloc : state -> id -> state **)

let dealloc a x =
  { vs = a.vs; os = (fun y -> if Nat.eqb x y then None else a.os y); hn =
    (pred a.hn); hp = a.hp; hc = a.hc }

type binop =
| Oadd
| Osub
| Oxor
| Omul
| Odiv
| Omod
| Oband
| Obor
| Oand
| Oor
| Olt
| Ogt
| Oeq
| One
| Ole
| Oge

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

(** val ztrue : z -> bool **)

let ztrue z0 =
  negb (Z.eqb z0 Z0)

(** val eval_binop : binop -> z -> z -> z **)

let eval_binop o a b =
  match o with
  | Oadd -> Z.add a b
  | Osub -> Z.sub a b
  | Oxor -> Z.coq_lxor a b
  | Omul -> Z.mul a b
  | Odiv -> Z.quot a b
  | Omod -> Z.rem a b
  | Oband -> Z.coq_land a b
  | Obor -> Z.coq_lor a b
  | Oand -> bval (if ztrue a then ztrue b else false)
  | Oor -> bval (if ztrue a then true else ztrue b)
  | Olt -> bval (Z.ltb a b)
  | Ogt -> bval (Z.ltb b a)
  | Oeq -> bval (Z.eqb a b)
  | One -> bval (negb (Z.eqb a b))
  | Ole -> bval (Z.leb a b)
  | Oge -> bval (Z.leb b a)

(** val divb : binop -> z -> bool **)

let divb o b =
  match o with
  | Odiv -> ztrue b
  | Omod -> ztrue b
  | _ -> true

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

type arg =
| Aref of id
| Aval of exp

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
| Snew of cid * id
| Sdelete of cid * id
| Scall of mid * arg list
| Suncall of mid * arg list
| Socall of id * mid * arg list
| Souncall of id * mid * arg list

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
| Snew (c, x) -> Sdelete (c, x)
| Sdelete (c, x) -> Snew (c, x)
| Scall (m, args) -> Suncall (m, args)
| Suncall (m, args) -> Scall (m, args)
| Socall (x, m, args) -> Souncall (x, m, args)
| Souncall (x, m, args) -> Socall (x, m, args)
| x -> x

(** val ren_args : id list -> arg list -> id -> id **)

let rec ren_args ps args x =
  match ps with
  | [] -> x
  | p::ps' ->
    (match args with
     | [] -> x
     | a::as' ->
       (match a with
        | Aref y -> if Nat.eqb p x then y else ren_args ps' as' x
        | Aval _ -> ren_args ps' as' x))

(** val rename_exp : (id -> id) -> exp -> exp **)

let rec rename_exp r = function
| Cst z0 -> Cst z0
| Var x -> Var (r x)
| Fld (x, f) -> Fld ((r x), f)
| Idx (x, e0) -> Idx ((r x), (rename_exp r e0))
| Bop (o, e1, e2) -> Bop (o, (rename_exp r e1), (rename_exp r e2))

(** val rename_arg : (id -> id) -> arg -> arg **)

let rename_arg r = function
| Aref x -> Aref (r x)
| Aval e -> Aval (rename_exp r e)

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
| Snew (c, x) -> Snew (c, (r x))
| Sdelete (c, x) -> Sdelete (c, (r x))
| Scall (m, args) -> Scall (m, (map (rename_arg r) args))
| Suncall (m, args) -> Suncall (m, (map (rename_arg r) args))
| Socall (x, m, args) -> Socall ((r x), m, (map (rename_arg r) args))
| Souncall (x, m, args) -> Souncall ((r x), m, (map (rename_arg r) args))

(** val wrap_vals : id list -> arg list -> stm -> stm **)

let rec wrap_vals ps args s =
  match ps with
  | [] -> s
  | p::ps' ->
    (match args with
     | [] -> s
     | a::as' ->
       (match a with
        | Aref _ -> wrap_vals ps' as' s
        | Aval e -> Slocal (p, e, (wrap_vals ps' as' s), e)))

(** val bind_args : id list -> arg list -> stm -> stm **)

let bind_args ps args body =
  wrap_vals ps args (rename (ren_args ps args) body)

type mdecl =
| MDecl of id list * stm

type cdecl =
| CDecl of cid option * (mid -> mdecl option)

type ctable = cid -> cdecl option

(** val call_body : mdecl -> id -> arg list -> stm **)

let call_body d x args =
  let MDecl (ps, body) = d in bind_args ps ((Aref x)::args) body

type menv = { procs : (mid -> mdecl option); classes : ctable;
              cells : (cid -> nat) }

(** val dispatch_fn : nat -> ctable -> cid -> mid -> mdecl option **)

let rec dispatch_fn fuel t c m =
  match fuel with
  | O -> None
  | S k ->
    (match t c with
     | Some c0 ->
       let CDecl (p, ms) = c0 in
       (match ms m with
        | Some d -> Some d
        | None -> (match p with
                   | Some q -> dispatch_fn k t q m
                   | None -> None))
     | None -> None)

(** val oloc_eqb : loc option -> loc option -> bool **)

let oloc_eqb r1 r2 =
  match r1 with
  | Some l1 -> (match r2 with
                | Some l2 -> Nat.eqb l1 l2
                | None -> false)
  | None -> (match r2 with
             | Some _ -> false
             | None -> true)

(** val inb : menv -> state -> exp -> bool **)

let rec inb g a = function
| Fld (x, f) ->
  (match a.os x with
   | Some l -> if Nat.ltb l a.hn then Nat.ltb f (g.cells (a.hc l)) else false
   | None -> false)
| Idx (x, e') ->
  if if inb g a e' then Z.leb Z0 (eval e' a) else false
  then (match a.os x with
        | Some l ->
          if Nat.ltb l a.hn
          then Nat.ltb (Z.to_nat (eval e' a)) (g.cells (a.hc l))
          else false
        | None -> false)
  else false
| Bop (o, e1, e2) ->
  if if inb g a e1 then inb g a e2 else false
  then divb o (eval e2 a)
  else false
| _ -> true

(** val inb2 : menv -> state -> exp -> exp -> bool **)

let inb2 g a e1 e2 =
  if inb g a e1 then inb g a e2 else false

(** val inbw : menv -> state -> exp -> exp -> bool **)

let inbw g a ei e =
  if inb2 g a ei e then Z.leb Z0 (eval ei a) else false

(** val inbw2 : menv -> state -> exp -> exp -> bool **)

let inbw2 g a e1 e2 =
  if inb2 g a e1 e2
  then if Z.leb Z0 (eval e1 a) then Z.leb Z0 (eval e2 a) else false
  else false

(** val run : nat -> menv -> stm -> state -> nat -> (state*nat) option **)

let rec run fuel g s a nf =
  match fuel with
  | O -> None
  | S k ->
    (match s with
     | Sassign (x, o, e) ->
       if in_dec Nat.eq_dec x (fv e)
       then None
       else if negb (inb g a e)
            then None
            else Some ((setv a x (mapp o (a.vs x) (eval e a))),nf)
     | Sfassign (x, f, o, e) ->
       if negb (inb g a e)
       then None
       else (match a.os x with
             | Some l ->
               if if Nat.ltb l a.hn
                  then Nat.ltb f (g.cells (a.hc l))
                  else false
               then let b = setf a l f (mapp o (a.hp l f) (eval e a)) in
                    if Z.eqb (eval e b) (eval e a)
                    then Some (b,(Nat.max (S f) nf))
                    else None
               else None
             | None -> None)
     | Saassign (x, ei, o, e) ->
       if negb (inbw g a ei e)
       then None
       else (match a.os x with
             | Some l ->
               let i = Z.to_nat (eval ei a) in
               if if Nat.ltb l a.hn
                  then Nat.ltb i (g.cells (a.hc l))
                  else false
               then let b = setf a l i (mapp o (a.hp l i) (eval e a)) in
                    if if Z.eqb (eval ei b) (eval ei a)
                       then Z.eqb (eval e b) (eval e a)
                       else false
                    then Some (b,(Nat.max (S i) nf))
                    else None
               else None
             | None -> None)
     | Sswap (x, y) -> Some ((setv (setv a x (a.vs y)) y (a.vs x)),nf)
     | Saswap (x, e1, y, e2) ->
       if negb (inbw2 g a e1 e2)
       then None
       else (match a.os x with
             | Some l1 ->
               (match a.os y with
                | Some l2 ->
                  let i1 = Z.to_nat (eval e1 a) in
                  let i2 = Z.to_nat (eval e2 a) in
                  if if if Nat.ltb l1 a.hn
                        then Nat.ltb i1 (g.cells (a.hc l1))
                        else false
                     then if Nat.ltb l2 a.hn
                          then Nat.ltb i2 (g.cells (a.hc l2))
                          else false
                     else false
                  then let b =
                         setf (setf a l1 i1 (a.hp l2 i2)) l2 i2 (a.hp l1 i1)
                       in
                       if if Z.eqb (eval e1 b) (eval e1 a)
                          then Z.eqb (eval e2 b) (eval e2 a)
                          else false
                       then Some (b,(Nat.max (S i2) (Nat.max (S i1) nf)))
                       else None
                  else None
                | None -> None)
             | None -> None)
     | Soswap (x, y) -> Some ((seto (seto a x (a.os y)) y (a.os x)),nf)
     | Scopy (x, y) ->
       if Nat.eqb x y
       then None
       else (match a.os y with
             | Some _ -> None
             | None -> Some ((seto a y (a.os x)),nf))
     | Suncopy (x, y) ->
       if Nat.eqb x y
       then None
       else if oloc_eqb (a.os x) (a.os y)
            then Some ((seto a y None),nf)
            else None
     | Sseq (s1, s2) ->
       (match run k g s1 a nf with
        | Some p -> let b,nf1 = p in run k g s2 b nf1
        | None -> None)
     | Sif (e1, s1, s2, e2) ->
       if negb (inb g a e1)
       then None
       else if Z.eqb (eval e1 a) Z0
            then (match run k g s2 a nf with
                  | Some p ->
                    let b,nf1 = p in
                    if negb (inb g b e2)
                    then None
                    else if Z.eqb (eval e2 b) Z0 then Some (b,nf1) else None
                  | None -> None)
            else (match run k g s1 a nf with
                  | Some p ->
                    let b,nf1 = p in
                    if negb (inb g b e2)
                    then None
                    else if Z.eqb (eval e2 b) Z0 then None else Some (b,nf1)
                  | None -> None)
     | Sloop (e1, s1, s2, e2) ->
       if negb (inb g a e1)
       then None
       else if Z.eqb (eval e1 a) Z0
            then None
            else (match run k g s1 a nf with
                  | Some p -> let b,nf1 = p in run_loop k g e1 s1 s2 e2 b nf1
                  | None -> None)
     | Slocal (x, e1, s', e2) ->
       if in_dec Nat.eq_dec x (fv e1)
       then None
       else if in_dec Nat.eq_dec x (fv e2)
            then None
            else if negb (inb g a e1)
                 then None
                 else (match run k g s' (setv a x (eval e1 a)) nf with
                       | Some p ->
                         let b,nf1 = p in
                         if negb (inb g b e2)
                         then None
                         else if Z.eqb (b.vs x) (eval e2 b)
                              then Some ((setv b x (a.vs x)),nf1)
                              else None
                       | None -> None)
     | Sobj (cl, x, s') ->
       (match a.os x with
        | Some _ -> None
        | None ->
          (match run k g s' (alloc a cl x) nf with
           | Some p ->
             let b,nf1 = p in
             if if oloc_eqb (b.os x) (Some a.hn)
                then if Nat.eqb b.hn (S a.hn)
                     then if Nat.eqb (b.hc a.hn) cl
                          then forallb (fun f -> Z.eqb (b.hp a.hn f) Z0)
                                 (seq O nf1)
                          else false
                     else false
                else false
             then Some ((dealloc b x),nf1)
             else None
           | None -> None))
     | Snew (cl, x) ->
       (match a.os x with
        | Some _ -> None
        | None -> Some ((alloc a cl x),nf))
     | Sdelete (cl, x) ->
       (match a.os x with
        | Some l ->
          if if Nat.ltb O a.hn
             then if Nat.eqb l (pred a.hn)
                  then if Nat.eqb (a.hc (pred a.hn)) cl
                       then forallb (fun f -> Z.eqb (a.hp (pred a.hn) f) Z0)
                              (seq O nf)
                       else false
                  else false
             else false
          then Some ((dealloc a x),nf)
          else None
        | None -> None)
     | Scall (m, args) ->
       (match g.procs m with
        | Some m0 ->
          let MDecl (ps, body) = m0 in
          if Nat.eqb (length ps) (length args)
          then run k g (bind_args ps args body) a nf
          else None
        | None -> None)
     | Suncall (m, args) ->
       (match g.procs m with
        | Some m0 ->
          let MDecl (ps, body) = m0 in
          if Nat.eqb (length ps) (length args)
          then run k g (invert (bind_args ps args body)) a nf
          else None
        | None -> None)
     | Socall (x, m, args) ->
       (match a.os x with
        | Some l ->
          if Nat.ltb l a.hn
          then (match dispatch_fn k g.classes (a.hc l) m with
                | Some d ->
                  (match run k g (call_body d x args) a nf with
                   | Some p ->
                     let b,nf1 = p in
                     if if oloc_eqb (b.os x) (Some l)
                        then if Nat.eqb (b.hc l) (a.hc l)
                             then Nat.eqb b.hn a.hn
                             else false
                        else false
                     then Some (b,nf1)
                     else None
                   | None -> None)
                | None -> None)
          else None
        | None -> None)
     | Souncall (x, m, args) ->
       (match a.os x with
        | Some l ->
          if Nat.ltb l a.hn
          then (match dispatch_fn k g.classes (a.hc l) m with
                | Some d ->
                  (match run k g (invert (call_body d x args)) a nf with
                   | Some p ->
                     let b,nf1 = p in
                     if if oloc_eqb (b.os x) (Some l)
                        then if Nat.eqb (b.hc l) (a.hc l)
                             then Nat.eqb b.hn a.hn
                             else false
                        else false
                     then Some (b,nf1)
                     else None
                   | None -> None)
                | None -> None)
          else None
        | None -> None)
     | _ -> Some (a,nf))

(** val run_loop :
    nat -> menv -> exp -> stm -> stm -> exp -> state -> nat -> (state*nat)
    option **)

and run_loop fuel g e1 s1 s2 e2 a nf =
  match fuel with
  | O -> None
  | S k ->
    if negb (inb g a e2)
    then None
    else if Z.eqb (eval e2 a) Z0
         then (match run k g s2 a nf with
               | Some p ->
                 let b,nf1 = p in
                 if negb (inb g b e1)
                 then None
                 else if Z.eqb (eval e1 b) Z0
                      then (match run k g s1 b nf1 with
                            | Some p0 ->
                              let c,nf2 = p0 in run_loop k g e1 s1 s2 e2 c nf2
                            | None -> None)
                      else None
               | None -> None)
         else Some (a,nf)

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
