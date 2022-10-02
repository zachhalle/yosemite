type kind = Syntax.kind
type constructor = Syntax.constructor
type term = Syntax.term

open Syntax

let rec substKindMain m s n l k =
  match k with
  | Ktype -> k
  | Ksing c -> Ksing (substConMain m s n l c)
  | Kpi (k1, k2) -> Kpi (substKindMain m s n l k1, substKindMain (m+1) s n l k2)
  | Ksigma (k1, k2) -> Ksigma (substKindMain m s n l k1, substKindMain (m+1) s n l k2)
  | Kunit -> k

and substConMain m s n l c =
  match c with
  | Cvar (i, None) ->
    if i < m then
      c
    else if i < m+n then
      substConMain 0 [] 0 m (List.nth s (i-m))
    else
      Cvar (i-n+l, None)
  | Cvar (i, Some j) ->
    if i < m then
      Cvar (i, Some (j-n+l))
    else if i < m+n then
      substConMain 0 [] 0 m (List.nth s (i-m))
    else
      Cvar (i-n+l, Some (j-n+l))
  | Clam (k, c) ->
    Clam (substKindMain m s n l k, substConMain (m+1) s n l c)
  | Capp (c1, c2) ->
    Capp (substConMain m s n l c1, substConMain m s n l c2)
  | Cpair (c1, c2) ->
    Cpair (substConMain m s n l c1, substConMain m s n l c2)
  | Cpi1 c ->
    Cpi1 (substConMain m s n l c)
  | Cpi2 c ->
    Cpi2 (substConMain m s n l c)
  | Cunit -> c
  | Carrow (c1, c2) ->
    Carrow (substConMain m s n l c1, substConMain m s n l c2)
  | Cforall (k, c) ->
    Cforall (substKindMain m s n l k, substConMain (m+1) s n l c)
  | Cexists (k, c) ->
    Cexists (substKindMain m s n l k, substConMain (m+1) s n l c)
  | Cprod cl ->
    Cprod (List.map (substConMain m s n l) cl)
  | Csum cl ->
    Csum (List.map (substConMain m s n l) cl)
  | Crec c ->
    Crec (substConMain (m+1) s n l c)
  | Ctag c ->
    Ctag (substConMain m s n l c)
  | Cref c ->
    Cref (substConMain m s n l c)
  | Cexn | Cbool | Cint | Cchar | Cstring -> c

let rec substTermMain m s n l e =
  match e with
  | Tvar _ -> e
  | Tlam (v, c, e') ->
    Tlam (v, substConMain m s n l c, substTermMain m s n l e')
  | Tapp (e1, e2) ->
    Tapp (substTermMain m s n l e1, substTermMain m s n l e2)
  | Tplam (k, e') ->
    Tplam (substKindMain m s n l k, substTermMain (m+1) s n l e')
  | Tpapp (e', c) ->
    Tpapp (substTermMain m s n l e', substConMain m s n l c)
  | Tpack (c1, e', c2) ->
    Tpack (substConMain m s n l c1, substTermMain m s n l e', substConMain m s n l c2)
  | Tunpack (v, e1, e2) ->
    Tunpack (v, substTermMain m s n l e1, substTermMain (m+1) s n l e2)
  | Ttuple el ->
    Ttuple (List.map (substTermMain m s n l) el)
  | Tproj (e', i) ->
    Tproj (substTermMain m s n l e', i)
  | Tinj (e', i, c) ->
    Tinj (substTermMain m s n l e', i, substConMain m s n l c)
  | Tcase (e', arms) ->
    Tcase (substTermMain m s n l e', List.map (fun (vi, ei) -> (vi, substTermMain m s n l ei)) arms)
  | Troll (e', c) ->
    Troll (substTermMain m s n l e', substConMain m s n l c)
  | Tunroll e' ->
    Tunroll (substTermMain m s n l e')
  | Ttag (e1, e2) ->
    Ttag (substTermMain m s n l e1, substTermMain m s n l e2)
  | Tiftag (e1, e2, v, e3, e4) ->
    Tiftag (
      substTermMain m s n l e1,
      substTermMain m s n l e2,
      v,
      substTermMain m s n l e3,
      substTermMain m s n l e4
    )
  | Tnewtag c ->
    Tnewtag (substConMain m s n l c)
  | Traise (e', c) ->
    Traise (substTermMain m s n l e', substConMain m s n l c)
  | Thandle (e1, v, e2) ->
    Thandle (substTermMain m s n l e1, v, substTermMain m s n l e2)
  | Tref e' ->
    Tref (substTermMain m s n l e')
  | Tderef e' ->
    Tderef (substTermMain m s n l e')
  | Tassign (e1, e2) ->
    Tassign (substTermMain m s n l e1, substTermMain m s n l e2)
  | Tif (e1, e2, e3) ->
    Tif (substTermMain m s n l e1, substTermMain m s n l e2, substTermMain m s n l e3)
  | Tbool _ | Tint _ | Tchar _ | Tstring _ -> e
  | Tlet (v, e1, e2) ->
    Tlet (v, substTermMain m s n l e1, substTermMain m s n l e2)
  | Tprim (prim, el) ->
    Tprim (prim, List.map (substTermMain m s n l) el)
 

let substKindGen m s l e =
  let n = List.length s in
  if n = 0 && l = 0 then
    e
  else
    substKindMain m s n l e

let substConGen m s l e =
  let n = List.length s in
  if n = 0 && l = 0 then
    e
  else
    substConMain m s n l e

let substTermGen m s l e = 
  let n = List.length s in
  if n = 0 && l = 0 then
    e
  else
    substTermMain m s n l e

let liftKind l e =
  if l = 0 then
    e
  else
    substKindMain 0 [] 0 l e

let liftConstructor l e =
  if l = 0 then
    e
  else
    substConMain 0 [] 0 l e

let liftTerm l e =
  if l = 0 then
    e
  else
    substTermMain 0 [] 0 l e

let substKind s e = substKindMain 0 [s] 1 0 e
let substConstructor s e = substConMain 0 [s] 1 0 e
let substTerm s e = substTermMain 0 [s] 1 0 e
