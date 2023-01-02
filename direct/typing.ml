type kind = Syntax.kind
type constructor = Syntax.constructor
type term = Syntax.term
type context = Context.context

open Syntax
open Subst
open Context
open Type_error
open Prim_type

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
    | Ksigma (k1, _) -> k1
    | _ -> raise Type_error
    end
  | Cpi2 c1', Cpi2 c2' ->
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

let rec check_kind ctx k =
  match k with
  | Ktype | Kunit -> ()
  | Ksing c -> check_constructor ctx c Ktype
  | Kpi (k1, k2) | Ksigma (k1, k2) -> check_kind ctx k1; check_kind (extend_kind ctx k1) k2

and infer_constructor ctx c =
  match c with
  | Cvar (i, _) -> selfify c (lookup_kind ctx i)
  | Clam (k, c) -> check_kind ctx k; Kpi (k, infer_constructor (extend_kind ctx k) c)
  | Capp (c1, c2) ->
    begin match infer_constructor ctx c1 with
    | Kpi (dom, cod) -> check_constructor ctx c2 dom; subst_kind c2 cod
    | _ -> raise Type_error
    end
  | Cpair (c1, c2) -> Ksigma (infer_constructor ctx c1, lift_kind 1 (infer_constructor ctx c2))
  | Cpi1 c' ->
    begin match infer_constructor ctx c' with
    | Ksigma (k1, _) -> k1
    | _ -> raise Type_error
    end
  | Cpi2 c' ->
    begin match infer_constructor ctx c' with
    | Ksigma (_, k2) -> subst_kind (Cpi1 c') k2
    | _ -> raise Type_error
    end
  | Cunit -> Kunit
  | Carrow (c1, c2) -> check_constructor ctx c1 Ktype; check_constructor ctx c2 Ktype; Ksing c
  | Cforall (k, c') -> check_kind ctx k; check_constructor (extend_kind ctx k) c' Ktype; Ksing c
  | Cexists (k, c') -> check_kind ctx k; check_constructor (extend_kind ctx k) c' Ktype; Ksing c
  | Cprod cl -> List.iter (fun c' -> check_constructor ctx c' Ktype) cl; Ksing c
  | Csum cl -> List.iter (fun c' -> check_constructor ctx c' Ktype) cl; Ksing c
  | Crec c' -> check_constructor (extend_kind ctx Ktype) c' Ktype; Ksing c
  | Ctag c' -> check_constructor ctx c' Ktype; Ksing c
  | Cref c' -> check_constructor ctx c' Ktype; Ksing c
  | Cexn | Cbool | Cint | Cchar | Cstring -> Ksing c

and check_constructor ctx c k = subkind ctx (infer_constructor ctx c) k

let whnf_annot ctx t = check_constructor ctx t Ktype; whnf ctx t

module Prim_type = Prim_type_fun (
  struct
    type constructor = Syntax.constructor
    let unit_t   = Cprod []
    let bool_t   = Cbool
    let int_t    = Cint
    let char_t   = Cchar
    let string_t = Cstring
  end
)

let rec infer_term ctx e =
  match e with
  | Tvar v -> lookup_type ctx v
  | Tlam (v, dom, e') ->
    check_constructor ctx dom Ktype; Carrow (dom, infer_term (extend_type ctx v dom) e')
  | Tapp (e1, e2) ->
    begin match infer_term_whnf ctx e1 with
    | Carrow (dom, cod) -> check_term ctx e2 dom; cod
    | _ -> raise Type_error
    end
  | Tplam (k, e') -> check_kind ctx k; Cforall (k, infer_term (extend_kind ctx k) e')
  | Tpapp (e', c) ->
    begin match infer_term_whnf ctx e' with
    | Cforall (k, t) -> check_constructor ctx c k; subst_constructor c t
    | _ -> raise Type_error
    end
  | Tpack (c, e', annot) ->
    begin match whnf_annot ctx annot with
    | Cexists (k, t) -> check_constructor ctx c k; check_term ctx e' (subst_constructor c t); annot
    | _ -> raise Type_error
    end
  | Tunpack (v, e1, e2) ->
    begin match infer_term_whnf ctx e1 with
    | Cexists (k1, t1) ->
      let ctx' = extend_type (extend_kind ctx k1) v t1 in
      let t2 = infer_term ctx' e2 in
      let t2' = subst_constructor (inhabitant k1) t2 in
      equiv ctx' t2 (lift_constructor 1 t2') Ktype; t2'
    | _ -> raise Type_error
    end
  | Ttuple el -> Cprod (List.map (infer_term ctx) el)
  | Tproj (e', i) ->
    begin match infer_term_whnf ctx e' with
    | Cprod tl -> begin try List.nth tl i with _ -> raise Type_error end 
    | _ -> raise Type_error
    end
  | Tinj (e', i, annot) ->
    begin match whnf_annot ctx annot with
    | Csum tl ->
      let t = try List.nth tl i with _ -> raise Type_error in
      check_term ctx e' t; annot
    | _ -> raise Type_error
    end
  | Tcase (_, []) -> raise Type_error
  | Tcase (e', (v1, e1) :: rest) ->
    begin match infer_term_whnf ctx e' with
    | Csum [] -> raise Type_error
    | Csum (t1 :: trest) ->
      let t = infer_term (extend_type ctx v1 t1) e1 in
      begin try List.iter2 (fun (vi, ei) ti -> check_term (extend_type ctx vi ti) ei t) rest trest with
      | Invalid_argument _ -> raise Type_error
      end;
      t
    | _ -> raise Type_error
    end
  | Troll (e', annot) ->
    begin match whnf_annot ctx annot with
    | Crec t -> check_term ctx e' (subst_constructor annot t); annot
    | _ -> raise Type_error
    end
  | Tunroll e' ->
    begin match infer_term_whnf ctx e' with
    | Crec t' as t -> subst_constructor t t'
    | _ -> raise Type_error
    end
  | Ttag (e1, e2) ->
    begin match infer_term_whnf ctx e1 with
    | Ctag t -> check_term ctx e2 t; Cexn
    | _ -> raise Type_error
    end
  | Tiftag (e1, e2, v, e3, e4) ->
    begin match infer_term_whnf ctx e1 with
    | Ctag t ->
      check_term ctx e2 Cexn;
      let t' = infer_term (extend_type ctx v t) e3 in
      check_term ctx e4 t'; t'
    | _ -> raise Type_error
    end
  | Tnewtag t -> check_constructor ctx t Ktype; Ctag t
  | Traise (e', t) -> check_term ctx e' Cexn; check_constructor ctx t Ktype; t
  | Thandle (e1, v, e2) ->
    let t = infer_term ctx e1 in
    check_term (extend_type ctx v Cexn) e2 t; t
  | Tref e' -> Cref (infer_term ctx e')
  | Tderef e' ->
    begin match infer_term_whnf ctx e' with
    | Cref t -> t
    | _ -> raise Type_error
    end
  | Tassign (e1, e2) ->
    begin match infer_term_whnf ctx e1 with
    | Cref t -> check_term ctx e2 t; Cprod []
    | _ -> raise Type_error
    end
  | Tbool _ -> Cbool
  | Tif (e1, e2, e3) ->
    begin match infer_term_whnf ctx e1 with
    | Cbool ->
      let t = infer_term ctx e2 in
      check_term ctx e3 t; t
    | _ -> raise Type_error
    end
  | Tint _ -> Cint
  | Tchar _ -> Cchar
  | Tstring _ -> Cstring
  | Tlet (v, e1, e2) ->
    let t = infer_term ctx e1 in
    infer_term (extend_type ctx v t) e2
  | Tprim (prim, el) -> 
    let (tl, t) = Prim_type.prim_type prim in
    begin try List.iter2 (fun ei ti -> check_term ctx ei ti) el tl with
    | Invalid_argument _ -> raise Type_error
    end;
    t

and infer_term_whnf ctx e = whnf ctx (infer_term ctx e)

and check_term ctx e t = equiv ctx (infer_term ctx e) t Ktype
