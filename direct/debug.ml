type kind = Syntax.kind
type constructor = Syntax.constructor
type term = Syntax.term

open Syntax

exception IndexError of int list

exception IndexErrorKind of kind * int option * int list
exception IndexErrorConstructor of constructor * int option * int list
exception IndexErrorTerm of term * int option * int list

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
  | Carrow (c1, c2) -> join path (depth_con (0 :: path) c1) (depth_con (1 :: path) c2)
  | Cforall (k, c) -> join path (depth_kind (0 :: path) k) (dec path (depth_con (1 :: path) c))
  | Cexists (k, c) -> join path (depth_kind (0 :: path) k) (dec path (depth_con (1 :: path) c))
  | Cprod cl ->
    fst (List.fold_left (fun (d, i) c -> (join path (depth_con (i :: 0 :: path) c) d, i+1)) (bottom, 0) cl)
  | Csum cl ->
    fst (List.fold_left (fun (d, i) c -> (join path (depth_con (i :: 0 :: path) c) d, i+1)) (bottom, 0) cl)
  | Crec c -> dec path (depth_con (0 :: path) c)
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
   | Carrow (c1, c2) ->
        Carrow (impose_con_main (0 :: path) n c1, impose_con_main (1 :: path) n c2)
   | Cforall (k, c) ->
        Cforall (impose_kind_main (0 :: path) n k, impose_con_main (1 :: path) (n+1) c)
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

let rec impose_term_main path n e =
   match e with
   | Tvar _ ->
        e
   | Tlam (v, c, e') ->
        Tlam (v, impose_con_main (1 :: path) n c, impose_term_main (2 :: path) n e')
   | Tapp (e1, e2) ->
        Tapp (impose_term_main (0 :: path) n e1, impose_term_main (1 :: path) n e2)
   | Tplam (k, e') ->
        Tplam (impose_kind_main (0 :: path) n k, impose_term_main (1 :: path) (n+1) e')
   | Tpapp (e', c) ->
        Tpapp (impose_term_main (0 :: path) n e', impose_con_main (1 :: path) n c)
   | Tpack (c1, e', c2) ->
        Tpack (impose_con_main (0 :: path) n c1, impose_term_main (1 :: path) n e', impose_con_main (2 :: path) n c2)
   | Tunpack (v, e1, e2) ->
        Tunpack (v, impose_term_main (1 :: path) n e1, impose_term_main (2 :: path) (n+1) e2)
   | Ttuple el ->
        Ttuple (List.mapi (fun i e -> impose_term_main (i :: 0 :: path) n e) el)
   | Tproj (e', i) ->
        Tproj (impose_term_main (0 :: path) n e', i)
   | Tinj (e', i, c) ->
        Tinj (impose_term_main (0 :: path) n e', i, impose_con_main (2 :: path) n c)
   | Tcase (e', arms) ->
        Tcase (impose_term_main (0 :: path) n e',
               List.mapi (fun i (v, e) -> (v, impose_term_main (1 :: i :: 1 :: path) n e)) arms)
   | Troll (e', c) ->
        Troll (impose_term_main (0 :: path) n e', impose_con_main (1 :: path) n c)
   | Tunroll e' ->
        Tunroll (impose_term_main (0 :: path) n e')
   | Ttag (e1, e2) ->
        Ttag (impose_term_main (0 :: path) n e1, impose_term_main (1 :: path) n e2)
   | Tiftag (e1, e2, v, e3, e4) ->
        Tiftag (impose_term_main (0 :: path) n e1, impose_term_main (1 :: path) n e2, v, impose_term_main (3 :: path) n e3, impose_term_main (4 :: path) n e4)
   | Tnewtag c ->
        Tnewtag (impose_con_main (0 :: path) n c)
   | Traise (e', c) ->
        Traise (impose_term_main (0 :: path) n e', impose_con_main (1 :: path) n c)
   | Thandle (e1, v, e2) ->
        Thandle (impose_term_main (0 :: path) n e1, v, impose_term_main (2 :: path) n e2)
   | Tref e' ->
        Tref (impose_term_main (0 :: path) n e')
   | Tderef e' ->
        Tderef (impose_term_main (0 :: path) n e')
   | Tassign (e1, e2) ->
        Tassign (impose_term_main (0 :: path) n e1, impose_term_main (1 :: path) n e2)
   | Tbool _ -> e
   | Tif (e1, e2, e3) ->
        Tif (impose_term_main (0 :: path) n e1, impose_term_main (1 :: path) n e2, impose_term_main (2 :: path) n e3)
   | Tint _ -> e
   | Tchar _ -> e
   | Tstring _ -> e
   | Tlet (v, e1, e2) ->
        Tlet (v, impose_term_main (1 :: path) n e1, impose_term_main (2 :: path) n e2)
   | Tprim (prim, el) ->
        Tprim (prim, List.mapi (fun i e -> impose_term_main (i :: 1 :: path) n e) el)

let impose_kind n k =
  try impose_kind_main [] n k with
  | IndexError path -> raise (IndexErrorKind (k, Some n, path))

let impose_constructor n c =
  try impose_con_main [] n c with
  | IndexError path -> raise (IndexErrorConstructor (c, Some n, path))

let impose_term n e =
  try impose_term_main [] n e with
  | IndexError path -> raise (IndexErrorTerm (e, Some n, path))
