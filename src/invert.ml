open Syntax
open Value

let rec invert = function
  | [] -> []
  | stm :: tl ->
     let stm2 =
       begin
         match stm with
         | Skip -> Skip
         | Assign(obj, modOp, e) ->
            let f =
              if modOp = ModAdd then ModSub
              else if modOp = ModSub then ModAdd
              else ModXor in
            Assign(obj, f, e)
         | Swap(obj1, obj2) -> Swap(obj1, obj2)
         | Conditional(e1, stml1, stml2, e2) ->
            Conditional(e2, invert stml1, invert stml2, e1)
         | Loop(e1, stml1, stml2, e2) ->
            Loop(e2, invert stml1, invert stml2, e1)
         | ObjectBlock(tid, id, stml) ->
            ObjectBlock(tid, id, invert stml)
         | LocalBlock(dt, id, e1, stml, e2) ->
            LocalBlock(dt, id, e1, invert stml, e2)
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
       end
     in
     (invert tl) @ [stm2]
