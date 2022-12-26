type kind = Syntax.kind
type constructor = Syntax.constructor
type expr = Syntax.expr
type value = Syntax.value

open Syntax

exception IndexError of int list

exception IndexErrorKind of kind * int option * int list
exception IndexErrorConstructor of constructor * int option * int list
exception IndexErrorExpr of expr * int option * int list
exception IndexErrorValue of value * int option * int list

let bottom = Depth.bottom
let dec path d = Depth.dec (IndexError path) d
let join path d1 d2 = Depth.join (IndexError path) d1 d2

let rec depth_kind path k =
  match k with
  | Ktype -> bottom
  | Ksing c -> depth_con (0 :: path) c
  | Kpi (k1, k2) -> join path (depth_kind (0 :: path) k1) (dec path (depth_kind (1 :: path) k2))
  | Ksigma (k1, k2) -> join path (depth_kind (0 :: path) k1) (dec path (depth_kind (1 :: path) k2))
  | Kunit -> bottom

and depth_con path c =
  match c with
  | Cvar (i, None) ->
    Depth.AtLeast i
  | Cvar (i, Some j) ->
    if i < j then
      Depth.Exactly j
    else
      raise (IndexError (List.rev path))
  | Clam (k, c) ->
    join path (depth_kind (0 :: path) k) (dec path (depth_con (1 :: path) c))
  | Capp (c1, c2) ->
    join path (depth_con (0 :: path) c1) (depth_con (1 :: path) c2)
  | Cpair (c1, c2) ->
    join path (depth_con (0 :: path) c1) (depth_con (1 :: path) c2)
  | Cpi1 c ->
    depth_con (0 :: path) c
  | Cpi2 c ->
    depth_con (0 :: path) c
  | Cunit -> bottom
  | Cnot c -> depth_con (0 :: path) c
  | Cexists (k, c) -> join path (depth_kind (0 :: path) k) (depth_con (1 :: path) c)
  | Cprod cl ->
    fst (List.fold_left (fun (d, i) c -> (join path (depth_con (i :: 0 :: path) c) d, i+1)) (bottom, 0) cl)
  | Csum cl ->
    fst (List.fold_left (fun (d, i) c -> (join path (depth_con (i :: 0 :: path) c) d, i+1)) (bottom, 0) cl)
  | Crec c -> depth_con (0 :: path) c
  | Ctag c -> depth_con (0 :: path) c
  | Cref c -> depth_con (0 :: path) c
  | Cexn | Cbool | Cint | Cchar | Cstring -> bottom

let check_kind k =
  try ignore (depth_kind [] k) with
  | IndexError path -> raise (IndexErrorKind (k, None, path))

let check_constructor c =
  try ignore (depth_con [] c) with
  | IndexError path -> raise (IndexErrorConstructor (c, None, path))

let rec impose_kind_main path n k =
   match k with
   | Ktype -> k
   | Ksing c ->
        Ksing (impose_con_main (0 :: path) n c)
   | Kpi (k1, k2) ->
        Kpi (impose_kind_main (0 :: path) n k1, impose_kind_main (1 :: path) (n+1) k2)
   | Ksigma (k1, k2) ->
        Ksigma (impose_kind_main (0 :: path) n k1, impose_kind_main (1 :: path) (n+1) k2)
   | Kunit -> k

and impose_con_main path n c =
   match c with
   | Cvar (i, None) ->
        if i < n then
           Cvar (i, Some n)
        else
           raise (IndexError (List.rev path))
   | Cvar (i, Some j) ->
        if n = j && i < j then
           c
        else
           raise (IndexError (List.rev path))
   | Clam (k, c) ->
        Clam (impose_kind_main (0 :: path) n k, impose_con_main (1 :: path) (n+1) c)
   | Capp (c1, c2) ->
        Capp (impose_con_main (0 :: path) n c1, impose_con_main (1 :: path) n c2)
   | Cpair (c1, c2) ->
        Cpair (impose_con_main (0 :: path) n c1, impose_con_main (1 :: path) n c2)
   | Cpi1 c ->
        Cpi1 (impose_con_main (0 :: path) n c)
   | Cpi2 c ->
        Cpi2 (impose_con_main (0 :: path) n c)
   | Cunit -> c
   | Cnot c -> Cnot (impose_con_main (0 :: path) n c)
   | Cexists (k, c) ->
        Cexists (impose_kind_main (0 :: path) n k, impose_con_main (1 :: path) (n+1) c)
   | Cprod cl ->
        Cprod (List.mapi (fun i c -> impose_con_main (i :: 0 :: path) n c) cl)
   | Csum cl ->
        Csum (List.mapi (fun i c -> impose_con_main (i :: 0 :: path) n c) cl)
   | Crec c ->
        Crec (impose_con_main (0 :: path) (n+1) c)
   | Ctag c ->
        Ctag (impose_con_main (0 :: path) n c)
   | Cref c ->
        Cref (impose_con_main (0 :: path) n c)
   | Cexn | Cbool | Cint | Cchar | Cstring -> c

let rec impose_expr_main path n e =
  match e with
  | Eapp (v1, v2) -> Eapp (impose_value_main (0 :: path) n v1, impose_value_main (1 :: path) n v2)
  | Eunpack (x, v, e) -> Eunpack (x, impose_value_main (1 :: path) n v, impose_expr_main (2 :: path) (n+1) e)
  | Eproj (x, v, i, e) -> Eproj (x, impose_value_main (1 :: path) n v, i, impose_expr_main (3 :: path) n e)
  | Ecase (v, arms) ->
    Ecase (
      impose_value_main (0 :: path) n v,
      List.mapi (fun i (xi, ei) -> (xi, impose_expr_main (1 :: i :: 1 :: path) n ei)) arms)
  | Eiftag (v1, v2, x, e1, e2) ->
    Eiftag (
      impose_value_main (0 :: path) n v1,
      impose_value_main (1 :: path) n v2,
      x,
      impose_expr_main (3 :: path) n e1,
      impose_expr_main (4 :: path) n e2)
  | Enewtag (x, t, e) -> Enewtag (x, impose_con_main (1 :: path) n t, impose_expr_main (2 :: path) n e)
  | Eref (x, v, e) -> Eref (x, impose_value_main (1 :: path) n v, impose_expr_main (2 :: path) n e)
  | Ederef (x, v, e) -> Ederef (x, impose_value_main (1 :: path) n v, impose_expr_main (2 :: path) n e)
  | Eassign (v1, v2, e) ->
    Eassign (impose_value_main (0 :: path) n v1, impose_value_main (1 :: path) n v2, impose_expr_main (2 :: path) n e)
  | Eif (v, e1, e2) ->
    Eif (impose_value_main (0 :: path) n v, impose_expr_main (1 :: path) n e1, impose_expr_main (2 :: path) n e2)
  | Elet (x, v, e) ->
    Elet (x, impose_value_main (1 :: path) n v, impose_expr_main (2 :: path) n e)
  | Eprim (x, prim, vl, e) ->
    Eprim (
      x, prim, List.mapi (fun i e -> impose_value_main (i :: 2 :: path) n e) vl, impose_expr_main (3 :: path) n e)
  | Ehalt -> e

and impose_value_main path n v =
  match v with
  | Vvar _ -> v
  | Vlam (x, c, e) -> Vlam (x, impose_con_main (1 :: path) n c, impose_expr_main (2 :: path) n e)
  | Vpack (c1, v', c2) ->
    Vpack (
      impose_con_main (0 :: path) n c1,
      impose_value_main (1 :: path) n v',
      impose_con_main (2 :: path) n c2)
  | Vtuple vl -> Vtuple (List.mapi (fun i v -> impose_value_main (i :: 0 :: path) n v) vl)
  | Vinj (v', i, c) -> Vinj (impose_value_main (0 :: path) n v', i, impose_con_main (2 :: path) n c)
  | Vroll (v', c) -> Vroll (impose_value_main (0 :: path) n v', impose_con_main (1 :: path) n c)
  | Vunroll v' -> Vunroll (impose_value_main (0 :: path) n v')
  | Vtag (v1, v2) -> Vtag (impose_value_main (0 :: path) n v1, impose_value_main (1 :: path) n v2)
  | Vbool _ | Vint _ | Vchar _ | Vstring _ -> v

let impose_kind n k =
  try impose_kind_main [] n k with
  | IndexError path -> raise (IndexErrorKind (k, Some n, path))

let impose_constructor n c =
  try impose_con_main [] n c with
  | IndexError path -> raise (IndexErrorConstructor (c, Some n, path))

let impose_expr n e =
  try impose_expr_main [] n e with
  | IndexError path -> raise (IndexErrorExpr (e, Some n, path))

let impose_value n v =
  try impose_value_main [] n v with
  | IndexError path -> raise (IndexErrorValue (v, Some n, path))
