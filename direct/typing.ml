type kind = Syntax.kind
type constructor = Syntax.constructor
type context = Context.context

open Syntax
open Subst
open Context
open Type_error

let rec natural_kind ctx c =
  match c with
  | Cvar (i, _) -> lookup_kind ctx i
  | Capp (c1, c2) ->
    begin match natural_kind ctx c1 with
    | Kpi (_, k2) -> subst_kind c2 k2
    | _ -> raise Type_error
    end
  | Cpi1 c' ->
    begin match natural_kind ctx c' with
    | Ksigma (k1, _) -> k1
    | _ -> raise Type_error
    end
  | Cpi2 c' ->
    begin match natural_kind ctx c' with
    | Ksigma (_, k2) -> k2
    | _ -> raise Type_error
    end
  | Carrow _ | Cforall _ | Cexists _ | Cprod _
  | Csum _ | Crec _ | Ctag _ | Cref _ | Cexn
  | Cbool | Cint | Cchar | Cstring -> Ktype
  | Cunit | Clam _ | Cpair _ -> raise Type_error

let rec whreduce c =
  match c with
  | Capp (c1, c2) ->
    begin match whreduce c1 with
    | Clam (_, c1') -> whreduce (subst_constructor c2 c1')
    | c1' -> Capp (c1', c2)
    end
  | Cpi1 c' ->
    begin match whreduce c' with
    | Cpair (c1, _) -> whreduce c1
    | c'' -> Cpi1 c''
    end
  | Cpi2 c' ->
    begin match whreduce c' with
    | Cpair (_, c2) -> whreduce c2
    | c'' -> Cpi2 c''
    end
  | _ -> c

let rec whnf ctx c =
  let c' = whreduce c in
  match natural_kind ctx c' with
  | Ksing c'' -> whnf ctx c''
  | _ -> c'

let rec equiv ctx c1 c2 k =
  match k with
  | Ktype -> ignore (equiv_path ctx (whnf ctx c1) (whnf ctx c2))
  | Ksing _ -> ()
  | Kpi (k1, k2) ->
    equiv
      (extend_kind ctx k1)
      (Capp (lift_constructor 1 c1, Cvar (0, None)))
      (Capp (lift_constructor 1 c2, Cvar (0, None)))
      k2
  | Ksigma (k1, k2) ->
    equiv ctx (Cpi1 c1) (Cpi1 c2) k1;
    equiv ctx (Cpi2 c1) (Cpi2 c2) (subst_kind (Cpi1 c1) k2)
  | Kunit -> ()

and equiv_path ctx c1 c2 =
  match c1, c2 with
  | Cvar (i1, _), Cvar (i2, _) ->
    if i1 = i2 then
      lookup_kind ctx i1
    else
      raise Type_error
  | Capp (c1a, c1b), Capp (c2a, c2b) ->
    begin match equiv_path ctx c1a c2a with
    | Kpi (k1, k2) -> equiv ctx c1b c2b k1; subst_kind c1b k2
    | _ -> raise Type_error
    end
  | Cpi1 c1', Cpi1 c2' ->
    begin match equiv_path ctx c1' c2' with
    | Ksigma (_, k2) -> subst_kind (Cpi1 c1') k2
    | _ -> raise Type_error
    end
  | Carrow (c1a, c1b), Carrow (c2a, c2b) ->
    equiv ctx c1a c2a Ktype;
    equiv ctx c1b c2b Ktype;
    Ktype
  | Cforall (k1, c1'), Cforall (k2, c2') ->
    samekind ctx k1 k2;
    equiv (extend_kind ctx k1) c1' c2' Ktype;
    Ktype
  | Cexists (k1, c1'), Cexists (k2, c2') ->
    samekind ctx k1 k2;
    equiv (extend_kind ctx k1) c1' c2' Ktype;
    Ktype
  | Cprod cl1, Cprod cl2 ->
    List.iter2 (fun c1 c2 -> equiv ctx c1 c2 Ktype) cl1 cl2;
    Ktype
  | Csum cl1, Csum cl2 ->
    List.iter2 (fun c1 c2 -> equiv ctx c1 c2 Ktype) cl1 cl2;
    Ktype
  | Crec c1', Crec c2' ->
    equiv (extend_kind ctx Ktype) c1' c2' Ktype;
    Ktype
  | Ctag c1', Ctag c2' ->
    equiv ctx c1' c2' Ktype;
    Ktype
  | Cref c1', Cref c2' ->
    equiv ctx c1' c2' Ktype;
    Ktype
  | Cexn, Cexn -> Ktype
  | Cbool, Cbool -> Ktype
  | Cint, Cint -> Ktype
  | Cchar, Cchar -> Ktype
  | Cstring, Cstring -> Ktype
  | _ -> raise Type_error

and samekind ctx k1 k2 =
  match k1, k2 with
  | Ktype, Ktype -> ()
  | Ksing c1, Ksing c2 -> equiv ctx c1 c2 Ktype
  | Kpi (k1a, k1b), Kpi (k2a, k2b) ->
    samekind ctx k1a k2a;
    samekind (extend_kind ctx k1a) k1b k2b
  | Ksigma (k1a, k1b), Ksigma (k2a, k2b) ->
    samekind ctx k1a k2a;
    samekind (extend_kind ctx k1a) k1b k2b
  | Kunit, Kunit -> ()
  | _ -> raise Type_error

let rec subkind ctx k1 k2 =
  match k1, k2 with
  | Ktype, Ktype -> ()
  | Ksing c1, Ksing c2 -> equiv ctx c1 c2 Ktype
  | Ksing _, Ktype -> ()
  | Kpi (k1a, k1b), Kpi (k2a, k2b) ->
    subkind ctx k2a k1a;
    subkind (extend_kind ctx k2a) k1b k2b;
  | Ksigma (k1a, k1b), Ksigma (k2a, k2b) ->
    subkind ctx k1a k2a;
    subkind (extend_kind ctx k1a) k1b k2b
  | Kunit, Kunit -> ()
  | _ -> raise Type_error

let rec selfify c k =
  match k with
  | Ktype -> Ksing c
  | Ksing _ -> k
  | Kpi (k1, k2) -> Kpi (k1, selfify (Capp (lift_constructor 1 c, Cvar (0, None))) k2)
  | Ksigma (k1, k2) -> Ksigma (selfify (Cpi1 c) k1, lift_kind 1 (selfify (Cpi2 c) (subst_kind (Cpi1 c) k2)))
  | Kunit -> k
 
let rec inhabitant k =
  match k with
  | Ktype -> Cprod [] (* unit *)
  | Ksing c -> c
  | Kpi (k1, k2) -> Clam (k1, inhabitant k2)
  | Ksigma (k1, k2) ->
    let c = inhabitant k1 in
    Cpair (c, inhabitant (subst_kind c k2))
  | Kunit -> Cunit

