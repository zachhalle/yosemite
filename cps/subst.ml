type kind = Syntax.kind
type constructor = Syntax.constructor
type expr = Syntax.expr
type value = Syntax.value

open Syntax

let rec subst_kind_main m s n l k =
  match k with
  | Ktype -> k
  | Ksing c -> Ksing (subst_constructor_main m s n l c)
  | Kpi (k1, k2) -> Kpi (subst_kind_main m s n l k1, subst_kind_main (m+1) s n l k2)
  | Ksigma (k1, k2) -> Ksigma (subst_kind_main m s n l k1, subst_kind_main (m+1) s n l k2)
  | Kunit -> k

and subst_constructor_main m s n l c =
  match c with
  | Cvar (i, None) ->
    if i < m then
      c
    else if i < m+n then
      subst_constructor_main 0 [] 0 m (List.nth s (i-m))
    else
      Cvar (i-n+l, None)
  | Cvar (i, Some j) ->
    if i < m then
      Cvar (i, Some (j-n+l))
    else if i < m+n then
      subst_constructor_main 0 [] 0 m (List.nth s (i-m))
    else
      Cvar (i-n+l, Some (j-n+l))
  | Clam (k, c) -> Clam (subst_kind_main m s n l k, subst_constructor_main (m+1) s n l c)
  | Capp (c1, c2) -> Capp (subst_constructor_main m s n l c1, subst_constructor_main m s n l c2)
  | Cpair (c1, c2) -> Cpair (subst_constructor_main m s n l c1, subst_constructor_main m s n l c2)
  | Cpi1 c -> Cpi1 (subst_constructor_main m s n l c)
  | Cpi2 c -> Cpi2 (subst_constructor_main m s n l c)
  | Cunit -> c
  | Cnot c -> Cnot (subst_constructor_main m s n l c)
  | Cexists (k, c) -> Cexists (subst_kind_main m s n l k, subst_constructor_main (m+1) s n l c)
  | Cprod cl -> Cprod (List.map (subst_constructor_main m s n l) cl)
  | Csum cl -> Csum (List.map (subst_constructor_main m s n l) cl)
  | Crec c -> Crec (subst_constructor_main (m+1) s n l c)
  | Ctag c -> Ctag (subst_constructor_main m s n l c)
  | Cref c -> Cref (subst_constructor_main m s n l c)
  | Cexn | Cbool | Cint | Cchar | Cstring -> c

let rec subst_expr_main m s n l e =
  match e with
  | Eapp (v1, v2) -> Eapp (subst_value_main m s n l v1, subst_value_main m s n l v2)
  | Eunpack (x, v, e) -> Eunpack (x, subst_value_main m s n l v, subst_expr_main (m+1) s n l e)
  | Eproj (x, v, i, e) -> Eproj (x, subst_value_main m s n l v, i, subst_expr_main m s n l e)
  | Ecase (v, arms) ->
    Ecase (subst_value_main m s n l v, List.map (fun (xi, ei) -> (xi, subst_expr_main m s n l ei)) arms)
  | Eiftag (v1, v2, x, e1, e2) ->
    Eiftag (
      subst_value_main m s n l v1, subst_value_main m s n l v2,
      x, 
      subst_expr_main m s n l e1, subst_expr_main m s n l e2)
  | Enewtag (x, t, e) -> Enewtag (x, subst_constructor_main m s n l t, subst_expr_main m s n l e)
  | Eref (x, v, e) -> Eref (x, subst_value_main m s n l v, subst_expr_main m s n l e)
  | Ederef (x, v, e) -> Ederef (x, subst_value_main m s n l v, subst_expr_main m s n l e)
  | Eassign (v1, v2, e) ->
    Eassign (subst_value_main m s n l v1, subst_value_main m s n l v2, subst_expr_main m s n l e)
  | Eif (v, e1, e2) -> Eif (subst_value_main m s n l v, subst_expr_main m s n l e1, subst_expr_main m s n l e2)
  | Elet (x, v, e) -> Elet (x, subst_value_main m s n l v, subst_expr_main m s n l e)
  | Eprim (x, prim, vl, e) -> Eprim (x, prim, List.map (subst_value_main m s n l) vl, subst_expr_main m s n l e)
  | Ehalt -> e

and subst_value_main m s n l v =
  match v with
  | Vvar _ -> v
  | Vlam (x, c, e) -> Vlam (x, subst_constructor_main m s n l c, subst_expr_main m s n l e)
  | Vpack (c1, v', c2) ->
    Vpack (subst_constructor_main m s n l c1, subst_value_main m s n l v', subst_constructor_main m s n l c2)
  | Vtuple vl -> Vtuple (List.map (subst_value_main m s n l) vl)
  | Vinj (v', i, c) -> Vinj (subst_value_main m s n l v', i, subst_constructor_main m s n l c)
  | Vroll (v', c) -> Vroll (subst_value_main m s n l v', subst_constructor_main m s n l c)
  | Vunroll v' -> Vunroll (subst_value_main m s n l v')
  | Vtag (v1, v2) -> Vtag (subst_value_main m s n l v1, subst_value_main m s n l v2)
  | Vbool _ | Vint _ | Vchar _ | Vstring _ -> v

let subst_kind_gen m s l exp =
  let n = List.length s in
  if n == 0 && l == 0 then
    exp
  else
    subst_kind_main m s n l exp

let subst_constructor_gen m s l exp =
  let n = List.length s in
  if n == 0 && l == 0 then
    exp
  else
    subst_constructor_main m s n l exp

let subst_expr_gen m s l exp =
  let n = List.length s in
  if n == 0 && l == 0 then
    exp
  else
    subst_expr_main m s n l exp

let subst_value_gen m s l exp =
  let n = List.length s in
  if n == 0 && l == 0 then
    exp
  else
    subst_value_main m s n l exp

let lift_kind l exp =
  if l == 0 then
    exp
  else
    subst_kind_main 0 [] 0 l exp

let lift_constructor l exp =
  if l == 0 then
    exp
  else
    subst_constructor_main 0 [] 0 l exp

let lift_expr l exp =
  if l == 0 then
    exp
  else
    subst_expr_main 0 [] 0 l exp

let lift_value l exp =
  if l == 0 then
    exp
  else
    subst_value_main 0 [] 0 l exp

let subst_kind s exp = subst_kind_main 0 [s] 1 0 exp
let subst_constructor s exp = subst_constructor_main 0 [s] 1 0 exp
let subst_expr s exp = subst_expr_main 0 [s] 1 0 exp
let subst_value s exp = subst_value_main 0 [s] 1 0 exp

