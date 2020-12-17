(**逆変換器：プログラムの文を逆変換する*)
open Syntax
(**文を逆変換する関数*)
let rec invert_stm stm = match stm with
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
  | For(x, e1, e2, stml) ->
     For(x, e2, e1, invert stml)
  (*追加部分switch*)
  | Switch(obj1, cases, stml, obj2) ->
     let rec invert_cases cases =
       let rec append_k cases =
         match cases with
         | ((c1, e1), s, (c2, e2, b)) :: tl ->
            if b = Break then [((c1, e1), s, (c2, e2, b))]
            else
              ((c1, e1), s, (c2, e2, b)) :: append_k tl
         | _ -> failwith "not implemented"
       in
       let rec search_kn cases =
         match cases with
         | ((_, _), _, (_, _, Break)) :: tl -> tl
         | _ :: tl -> search_kn tl
         | _ -> failwith "not implemented"
       in
       let invert_cases2 cases =
         let cases1 = List.map (fun ((c1, e1), s, (c2, e2, b)) ->
                          if c1 = Case && c2 = Esac then ((c1, e1), s, (c2, e2, b))
                          else if c1 = Case && c2 = NoEsac then ((NoCase, e1), s, (Esac, e2, b))
                          else ((Case, e1), s, (NoEsac, e2, b))
                        ) cases in
         let cases2 =
           match cases1 with
           | ((c1, e1), s, (c2, e2, b)) :: tl ->
              List.rev(((c1, e1), s, (c2, e2, Break)) :: tl)
           | _ -> failwith "not implemented"
         in
         match cases2 with
         | ((c1, e1), s, (c2, e2, b)) :: tl -> ((c1, e1), s, (c2, e2, NoBreak)) :: tl
         | _ -> failwith "not implemented"
       in
       match cases with
       | [] -> []
       | [x] -> [x]
       | ((c1, e1), s, (c2, e2, b)) :: tl ->
          if b = Break then ((c1, e1), s, (c2, e2, b)) :: invert_cases tl
          else
            let cases_k, cases_kn = append_k cases, search_kn cases in
            invert_cases2 cases_k @ invert_cases cases_kn
     in
     let cases2 =
       List.map (fun ((c1, e1), s, (c2, e2, b)) -> ((c1, e2), invert s, (c2, e1, b))) cases
     in
     Switch(obj2, invert_cases cases2, invert stml, obj1)
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
  | Show _ | Print _ -> stm
and invert stml = List.rev (List.map invert_stm stml)

let invert_method (MDecl(mid, para, stml)) = MDecl(mid, para, invert stml)
let invert_prog2 (CDecl(tid, inher, fields, methods)) =
  CDecl(tid, inher, fields, List.map invert_method methods)
let invert_prog (Prog(cl)) = Prog(List.map invert_prog2 cl)
