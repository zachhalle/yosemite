type kind = Syntax.kind
type constructor = Syntax.constructor
type expr = Syntax.expr
type value = Syntax.value
type context = Context.context

open Context
open Prim_type
open Subst
open Syntax
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
  | Cnot _ | Cexists _ | Cprod _ | Csum _ 
  | Crec _ | Ctag _ | Cref _ | Cexn 
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
  | Cnot c1', Cnot c2' ->
    equiv ctx c1' c2' Ktype;
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
  | Cnot c' -> check_constructor ctx c' Ktype; Ksing c
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

let rec check_expr ctx e = 
  match e with
  | Eapp (v1, v2) ->
    begin match infer_value_whnf ctx v1 with
    | Cnot dom -> check_value ctx v2 dom
    | _ -> raise Type_error
    end
  | Eunpack (x, v, e) ->
    begin match infer_value_whnf ctx v with
    | Cexists (k, t) -> check_expr (extend_type (extend_kind ctx k) x t) e
    | _ -> raise Type_error
    end
  | Eproj (x, v, i, e) ->
    begin match infer_value_whnf ctx v with
    | Cprod tl ->
      let t = try List.nth tl i with Failure _ | Invalid_argument _ -> raise Type_error in
      check_expr (extend_type ctx x t) e
    | _ -> raise Type_error
    end
  | Ecase (v, arms) ->
    begin match infer_value_whnf ctx v with
    | Csum tl ->
      begin try List.iter2 (fun (xi, ei) ti -> check_expr (extend_type ctx xi ti) ei) arms tl with
      | Invalid_argument _ -> raise Type_error
      end
    | _ -> raise Type_error
    end
  | Eiftag (v1, v2, x, e1, e2) ->
    begin match infer_value_whnf ctx v1 with
    | Ctag t -> check_value ctx v2 Cexn; check_expr (extend_type ctx x t) e1; check_expr ctx e2
    | _ -> raise Type_error
    end
  | Enewtag (x, t, e) -> check_constructor ctx t Ktype; check_expr (extend_type ctx x (Ctag t)) e
  | Eref (x, v, e) ->
    let t = infer_value ctx v in 
    check_expr (extend_type ctx x (Cref t)) e
  | Ederef (x, v, e) ->
    begin match infer_value_whnf ctx v with
    | Cref t -> check_expr (extend_type ctx x t) e
    | _ -> raise Type_error
    end
  | Eassign (v1, v2, e) ->
    begin match infer_value_whnf ctx v1 with
    | Cref t -> check_value ctx v2 t; check_expr ctx e
    | _ -> raise Type_error
    end
  | Eif (v, e1, e2) -> check_value ctx v Cbool; check_expr ctx e1; check_expr ctx e2
  | Elet (x, v, e) ->
    let t = infer_value ctx v in
    check_expr (extend_type ctx x t) e
  | Eprim (x, prim, vl, e) ->
    let tl, t = Prim_type.prim_type prim in
    begin try List.iter2 (fun vi ti -> check_value ctx vi ti) vl tl with
    | Invalid_argument _ -> raise Type_error
    end;
    check_expr (extend_type ctx x t) e
  | Ehalt -> ()

and infer_value ctx v = 
  match v with
  | Vvar x -> lookup_type ctx x
  | Vlam (x, dom, e) -> check_constructor ctx dom Ktype; check_expr (extend_type ctx x dom) e; Cnot dom
  | Vpack (c, v, annot) -> 
    begin match whnf_annot ctx annot with
    | Cexists (k, t) -> check_constructor ctx c k; check_value ctx v (subst_constructor c t); annot
    | _ -> raise Type_error
    end
  | Vtuple vl -> Cprod (List.map (infer_value ctx) vl)
  | Vinj (v, i, annot) ->
    begin match whnf_annot ctx annot with
    | Csum tl ->
      let t = try List.nth tl i with Failure _ | Invalid_argument _ -> raise Type_error in
      check_value ctx v t; annot
    | _ -> raise Type_error
    end
  | Vroll (v, annot) ->
    begin match whnf_annot ctx annot with
    | Crec t -> check_value ctx v (subst_constructor annot t); annot
    | _ -> raise Type_error
    end
  | Vunroll v -> 
    begin match infer_value_whnf ctx v with
    | Crec t' as t -> subst_constructor t t'
    | _ -> raise Type_error
    end
  | Vtag (v1, v2) ->
    begin match infer_value_whnf ctx v1 with
    | Ctag t -> check_value ctx v2 t; Cexn
    | _ -> raise Type_error
    end
  | Vbool _ -> Cbool
  | Vint _ -> Cint
  | Vchar _ -> Cchar
  | Vstring _ -> Cstring

and infer_value_whnf ctx v = whnf ctx (infer_value ctx v)

and check_value ctx v t = equiv ctx (infer_value ctx v) t Ktype

let check_program e = check_expr empty e
