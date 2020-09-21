open Syntax

let rec invert_stm = function
  | Skip -> Skip
  | Assign(obj, modOp, e) ->
     let invert_op = function
       | ModAdd -> ModSub
       | ModSub -> ModAdd
       | ModXor -> ModXor
     in Assign(obj, invert_op modOp, e)
  | Swap(obj1, obj2) -> Swap(obj1, obj2)
  | Conditional(e1, stml1, stml2, e2) ->
     Conditional(e2, invert stml1, invert stml2, e1)
  | Loop(e1, stml1, stml2, e2) ->
     Loop(e2, invert stml1, invert stml2, e1)
  (*追加部分for*)
  | FOR(x, NFOR(n1, n2), stml) ->
     FOR(x, NFOR(n2, n1), invert stml)
  | FOR(x1, AFOR(rev, x2), stml) ->
     let flag = if rev = true
                then false
                else true in
     FOR(x1, AFOR(flag, x2), stml)
  (*追加部分switch*)
  | Switch(rev, obj1, case_list, obj2) ->
     let rev_case = function
       | (e1, stml, e2, break) -> (e2, invert stml, e1, break)
     in
     let flag = if rev = true
                then false
                else true in
     let case_list2 = List.map rev_case case_list in
     Switch(flag, obj2, case_list2, obj1)
  | ObjectBlock(tid, id, stml) ->
     ObjectBlock(tid, id, invert stml)
  | LocalBlock(dt, id, e1, stml, e2) ->
     LocalBlock(dt, id, e2, invert stml, e1)
  | LocalCall(mid, objl) -> LocalUncall(mid, objl)
  | LocalUncall(mid, objl) -> LocalCall(mid, objl)
  | ObjectCall(obj, mid, objl) -> ObjectUncall(obj, mid, objl)
  | ObjectUncall(obj, mid, objl) -> ObjectCall(obj, mid, objl)
  | ObjectConstruction(tid, obj) -> ObjectDestruction(tid, obj)
  | ObjectDestruction(tid, obj) -> ObjectConstruction(tid, obj)
  | CopyReference(dt, obj1, obj2) -> UncopyReference(dt, obj1, obj2)
  | UncopyReference(dt, obj1, obj2) -> CopyReference(dt, obj1, obj2)
  | ArrayConstruction(c, id) -> ArrayDestruction(c, id)
  | ArrayDestruction(c, id) -> ArrayConstruction(c, id)
  | Show e -> Show e
  | Print str -> Print str
and invert stml = List.rev (List.map invert_stm stml)
