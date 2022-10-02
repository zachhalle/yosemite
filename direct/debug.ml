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

let rec depthKind path k =
  match k with
  | Ktype -> bottom
  | Ksing c -> depthCon (0 :: path) c
  | Kpi (k1, k2) -> join path (depthKind (0 :: path) k1) (dec path (depthKind (1 :: path) k2))
  | Ksigma (k1, k2) -> join path (depthKind (0 :: path) k1) (dec path (depthKind (1 :: path) k2))
  | Kunit -> bottom

and depthCon path c =
  match c with
  | Cvar (i, None) ->
    Depth.AtLeast i
  | Cvar (i, Some j) ->
    if i < j then
      Depth.Exactly j
    else
      raise (IndexError (List.rev path))
  | Clam (k, c) ->
    join path (depthKind (0 :: path) k) (dec path (depthCon (1 :: path) c))
  | Capp (c1, c2) ->
    join path (depthCon (0 :: path) c1) (depthCon (1 :: path) c2)
  | Cpair (c1, c2) ->
    join path (depthCon (0 :: path) c1) (depthCon (1 :: path) c2)
  | Cpi1 c ->
    depthCon (0 :: path) c
  | Cpi2 c ->
    depthCon (0 :: path) c
  | Cunit -> bottom
  | Carrow (c1, c2) -> join path (depthCon (0 :: path) c1) (depthCon (1 :: path) c2)
  | Cforall (k, c) -> join path (depthKind (0 :: path) k) (depthCon (1 :: path) c)
  | Cexists (k, c) -> join path (depthKind (0 :: path) k) (depthCon (1 :: path) c)
  | Cprod cl ->
    fst (List.fold_left (fun (d, i) c -> (join path (depthCon (i :: 0 :: path) c) d, i+1)) (bottom, 0) cl)
  | Csum cl ->
    fst (List.fold_left (fun (d, i) c -> (join path (depthCon (i :: 0 :: path) c) d, i+1)) (bottom, 0) cl)
  | Crec c -> depthCon (0 :: path) c
  | Ctag c -> depthCon (0 :: path) c
  | Cref c -> depthCon (0 :: path) c
  | Cexn | Cbool | Cint | Cchar | Cstring -> bottom

let checkKind k =
  try ignore (depthKind [] k) with
  | IndexError path -> raise (IndexErrorKind (k, None, path))

let checkConstructor c =
  try ignore (depthCon [] c) with
  | IndexError path -> raise (IndexErrorConstructor (c, None, path))

let rec imposeKindMain path n k =
   match k with
   | Ktype -> k
   | Ksing c ->
        Ksing (imposeConMain (0 :: path) n c)
   | Kpi (k1, k2) ->
        Kpi (imposeKindMain (0 :: path) n k1, imposeKindMain (1 :: path) (n+1) k2)
   | Ksigma (k1, k2) ->
        Ksigma (imposeKindMain (0 :: path) n k1, imposeKindMain (1 :: path) (n+1) k2)
   | Kunit -> k

and imposeConMain path n c =
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
        Clam (imposeKindMain (0 :: path) n k, imposeConMain (1 :: path) (n+1) c)
   | Capp (c1, c2) ->
        Capp (imposeConMain (0 :: path) n c1, imposeConMain (1 :: path) n c2)
   | Cpair (c1, c2) ->
        Cpair (imposeConMain (0 :: path) n c1, imposeConMain (1 :: path) n c2)
   | Cpi1 c ->
        Cpi1 (imposeConMain (0 :: path) n c)
   | Cpi2 c ->
        Cpi2 (imposeConMain (0 :: path) n c)
   | Cunit -> c
   | Carrow (c1, c2) ->
        Carrow (imposeConMain (0 :: path) n c1, imposeConMain (1 :: path) n c2)
   | Cforall (k, c) ->
        Cforall (imposeKindMain (0 :: path) n k, imposeConMain (1 :: path) (n+1) c)
   | Cexists (k, c) ->
        Cexists (imposeKindMain (0 :: path) n k, imposeConMain (1 :: path) (n+1) c)
   | Cprod cl ->
        Cprod (List.mapi (fun i c -> imposeConMain (i :: 0 :: path) n c) cl)
   | Csum cl ->
        Csum (List.mapi (fun i c -> imposeConMain (i :: 0 :: path) n c) cl)
   | Crec c ->
        Crec (imposeConMain (0 :: path) (n+1) c)
   | Ctag c ->
        Ctag (imposeConMain (0 :: path) n c)
   | Cref c ->
        Cref (imposeConMain (0 :: path) n c)
   | Cexn | Cbool | Cint | Cchar | Cstring -> c

let rec imposeTermMain path n e =
   match e with
   | Tvar _ ->
        e
   | Tlam (v, c, e') ->
        Tlam (v, imposeConMain (1 :: path) n c, imposeTermMain (2 :: path) n e')
   | Tapp (e1, e2) ->
        Tapp (imposeTermMain (0 :: path) n e1, imposeTermMain (1 :: path) n e2)
   | Tplam (k, e') ->
        Tplam (imposeKindMain (0 :: path) n k, imposeTermMain (1 :: path) (n+1) e')
   | Tpapp (e', c) ->
        Tpapp (imposeTermMain (0 :: path) n e', imposeConMain (1 :: path) n c)
   | Tpack (c1, e', c2) ->
        Tpack (imposeConMain (0 :: path) n c1, imposeTermMain (1 :: path) n e', imposeConMain (2 :: path) n c2)
   | Tunpack (v, e1, e2) ->
        Tunpack (v, imposeTermMain (1 :: path) n e1, imposeTermMain (2 :: path) (n+1) e2)
   | Ttuple el ->
        Ttuple (List.mapi (fun i e -> imposeTermMain (i :: 0 :: path) n e) el)
   | Tproj (e', i) ->
        Tproj (imposeTermMain (0 :: path) n e', i)
   | Tinj (e', i, c) ->
        Tinj (imposeTermMain (0 :: path) n e', i, imposeConMain (2 :: path) n c)
   | Tcase (e', arms) ->
        Tcase (imposeTermMain (0 :: path) n e',
               List.mapi (fun i (v, e) -> (v, imposeTermMain (1 :: i :: 1 :: path) n e)) arms)
   | Troll (e', c) ->
        Troll (imposeTermMain (0 :: path) n e', imposeConMain (1 :: path) n c)
   | Tunroll e' ->
        Tunroll (imposeTermMain (0 :: path) n e')
   | Ttag (e1, e2) ->
        Ttag (imposeTermMain (0 :: path) n e1, imposeTermMain (1 :: path) n e2)
   | Tiftag (e1, e2, v, e3, e4) ->
        Tiftag (imposeTermMain (0 :: path) n e1, imposeTermMain (1 :: path) n e2, v, imposeTermMain (3 :: path) n e3, imposeTermMain (4 :: path) n e4)
   | Tnewtag c ->
        Tnewtag (imposeConMain (0 :: path) n c)
   | Traise (e', c) ->
        Traise (imposeTermMain (0 :: path) n e', imposeConMain (1 :: path) n c)
   | Thandle (e1, v, e2) ->
        Thandle (imposeTermMain (0 :: path) n e1, v, imposeTermMain (2 :: path) n e2)
   | Tref e' ->
        Tref (imposeTermMain (0 :: path) n e')
   | Tderef e' ->
        Tderef (imposeTermMain (0 :: path) n e')
   | Tassign (e1, e2) ->
        Tassign (imposeTermMain (0 :: path) n e1, imposeTermMain (1 :: path) n e2)
   | Tbool _ -> e
   | Tif (e1, e2, e3) ->
        Tif (imposeTermMain (0 :: path) n e1, imposeTermMain (1 :: path) n e2, imposeTermMain (2 :: path) n e3)
   | Tint _ -> e
   | Tchar _ -> e
   | Tstring _ -> e
   | Tlet (v, e1, e2) ->
        Tlet (v, imposeTermMain (1 :: path) n e1, imposeTermMain (2 :: path) n e2)
   | Tprim (prim, el) ->
        Tprim (prim, List.mapi (fun i e -> imposeTermMain (i :: 1 :: path) n e) el)

let imposeKind n k =
  try imposeKindMain [] n k with
  | IndexError path -> raise (IndexErrorKind (k, Some n, path))

let imposeConstructor n c =
  try imposeConMain [] n c with
  | IndexError path -> raise (IndexErrorConstructor (c, Some n, path))

let imposeTerm n e =
  try imposeTermMain [] n e with
  | IndexError path -> raise (IndexErrorTerm (e, Some n, path))
