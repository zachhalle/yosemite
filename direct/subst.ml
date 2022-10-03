type kind = Syntax.kind
type constructor = Syntax.constructor
type term = Syntax.term

open Syntax

let rec subst_kind_main m s n l k =
  match k with
  | Ktype -> k
  | Ksing c -> Ksing (subst_con_main m s n l c)
  | Kpi (k1, k2) -> Kpi (subst_kind_main m s n l k1, subst_kind_main (m+1) s n l k2)
  | Ksigma (k1, k2) -> Ksigma (subst_kind_main m s n l k1, subst_kind_main (m+1) s n l k2)
  | Kunit -> k

and subst_con_main m s n l c =
  match c with
  | Cvar (i, None) ->
    if i < m then
      c
    else if i < m+n then
      subst_con_main 0 [] 0 m (List.nth s (i-m))
    else
      Cvar (i-n+l, None)
  | Cvar (i, Some j) ->
    if i < m then
      Cvar (i, Some (j-n+l))
    else if i < m+n then
      subst_con_main 0 [] 0 m (List.nth s (i-m))
    else
      Cvar (i-n+l, Some (j-n+l))
  | Clam (k, c) ->
    Clam (subst_kind_main m s n l k, subst_con_main (m+1) s n l c)
  | Capp (c1, c2) ->
    Capp (subst_con_main m s n l c1, subst_con_main m s n l c2)
  | Cpair (c1, c2) ->
    Cpair (subst_con_main m s n l c1, subst_con_main m s n l c2)
  | Cpi1 c ->
    Cpi1 (subst_con_main m s n l c)
  | Cpi2 c ->
    Cpi2 (subst_con_main m s n l c)
  | Cunit -> c
  | Carrow (c1, c2) ->
    Carrow (subst_con_main m s n l c1, subst_con_main m s n l c2)
  | Cforall (k, c) ->
    Cforall (subst_kind_main m s n l k, subst_con_main (m+1) s n l c)
  | Cexists (k, c) ->
    Cexists (subst_kind_main m s n l k, subst_con_main (m+1) s n l c)
  | Cprod cl ->
    Cprod (List.map (subst_con_main m s n l) cl)
  | Csum cl ->
    Csum (List.map (subst_con_main m s n l) cl)
  | Crec c ->
    Crec (subst_con_main (m+1) s n l c)
  | Ctag c ->
    Ctag (subst_con_main m s n l c)
  | Cref c ->
    Cref (subst_con_main m s n l c)
  | Cexn | Cbool | Cint | Cchar | Cstring -> c

let rec subst_term_main m s n l e =
  match e with
  | Tvar _ -> e
  | Tlam (v, c, e') ->
    Tlam (v, subst_con_main m s n l c, subst_term_main m s n l e')
  | Tapp (e1, e2) ->
    Tapp (subst_term_main m s n l e1, subst_term_main m s n l e2)
  | Tplam (k, e') ->
    Tplam (subst_kind_main m s n l k, subst_term_main (m+1) s n l e')
  | Tpapp (e', c) ->
    Tpapp (subst_term_main m s n l e', subst_con_main m s n l c)
  | Tpack (c1, e', c2) ->
    Tpack (subst_con_main m s n l c1, subst_term_main m s n l e', subst_con_main m s n l c2)
  | Tunpack (v, e1, e2) ->
    Tunpack (v, subst_term_main m s n l e1, subst_term_main (m+1) s n l e2)
  | Ttuple el ->
    Ttuple (List.map (subst_term_main m s n l) el)
  | Tproj (e', i) ->
    Tproj (subst_term_main m s n l e', i)
  | Tinj (e', i, c) ->
    Tinj (subst_term_main m s n l e', i, subst_con_main m s n l c)
  | Tcase (e', arms) ->
    Tcase (subst_term_main m s n l e', List.map (fun (vi, ei) -> (vi, subst_term_main m s n l ei)) arms)
  | Troll (e', c) ->
    Troll (subst_term_main m s n l e', subst_con_main m s n l c)
  | Tunroll e' ->
    Tunroll (subst_term_main m s n l e')
  | Ttag (e1, e2) ->
    Ttag (subst_term_main m s n l e1, subst_term_main m s n l e2)
  | Tiftag (e1, e2, v, e3, e4) ->
    Tiftag (
      subst_term_main m s n l e1,
      subst_term_main m s n l e2,
      v,
      subst_term_main m s n l e3,
      subst_term_main m s n l e4
    )
  | Tnewtag c ->
    Tnewtag (subst_con_main m s n l c)
  | Traise (e', c) ->
    Traise (subst_term_main m s n l e', subst_con_main m s n l c)
  | Thandle (e1, v, e2) ->
    Thandle (subst_term_main m s n l e1, v, subst_term_main m s n l e2)
  | Tref e' ->
    Tref (subst_term_main m s n l e')
  | Tderef e' ->
    Tderef (subst_term_main m s n l e')
  | Tassign (e1, e2) ->
    Tassign (subst_term_main m s n l e1, subst_term_main m s n l e2)
  | Tif (e1, e2, e3) ->
    Tif (subst_term_main m s n l e1, subst_term_main m s n l e2, subst_term_main m s n l e3)
  | Tbool _ | Tint _ | Tchar _ | Tstring _ -> e
  | Tlet (v, e1, e2) ->
    Tlet (v, subst_term_main m s n l e1, subst_term_main m s n l e2)
  | Tprim (prim, el) ->
    Tprim (prim, List.map (subst_term_main m s n l) el)
 

let subst_kind_gen m s l e =
  let n = List.length s in
  if n = 0 && l = 0 then
    e
  else
    subst_kind_main m s n l e

let subst_con_gen m s l e =
  let n = List.length s in
  if n = 0 && l = 0 then
    e
  else
    subst_con_main m s n l e

let subst_term_gen m s l e = 
  let n = List.length s in
  if n = 0 && l = 0 then
    e
  else
    subst_term_main m s n l e

let lift_kind l e =
  if l = 0 then
    e
  else
    subst_kind_main 0 [] 0 l e

let lift_constructor l e =
  if l = 0 then
    e
  else
    subst_con_main 0 [] 0 l e

let lift_term l e =
  if l = 0 then
    e
  else
    subst_term_main 0 [] 0 l e

let subst_kind s e = subst_kind_main 0 [s] 1 0 e
let subst_constructor s e = subst_con_main 0 [s] 1 0 e
let subst_term s e = subst_term_main 0 [s] 1 0 e
