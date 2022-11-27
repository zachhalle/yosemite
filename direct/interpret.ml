module Result = struct
  type result = 
    | Rfn of (result -> result)
    | Rpfn of (unit -> result)
    | Rpack of result
    | Rtuple of result list
    | Rinj of result * int
    | Rroll of result
    | Rref of result ref
    | Rtag of unit ref
    | Rexn of unit ref * result
    | Rbool of bool
    | Rint of int
    | Rchar of char
    | Rstring of string
  
  exception Runtime_error
  exception Raise_exn of result
end

include Result

open Prim_eval

module Prim_eval = Prim_eval_fun (
  struct
    include Result

    let runit = Rtuple []
    let rbool b = Rbool b
    let rint i = Rint i
    let rchar c = Rchar c
    let rstring s = Rstring s

    let dest_rbool r =
      match r with
      | Rbool b -> Some b
      | _ -> None

    let dest_rint r =
      match r with
      | Rint i -> Some i
      | _ -> None

    let dest_rchar r =
      match r with
      | Rchar c -> Some c
      | _ -> None

    let dest_rstring r =
      match r with
      | Rstring s -> Some s
      | _ -> None
  end
)

module Dict = Map.Make(Var)

type env = result Dict.t

type term = Syntax.term

open Syntax 

let rec eval env e =
  match e with
  | Tvar v ->
    begin match Dict.find_opt v env with
    | Some r -> r
    | None -> raise Runtime_error
    end
  | Tlam (v, _, e') -> Rfn (fun r -> eval (Dict.add v r env) e')
  | Tapp (e1, e2) ->
    begin match eval env e1 with
    | Rfn f -> f (eval env e2)
    | _ -> raise Runtime_error
    end
  | Tplam (_, e') -> Rpfn (fun () -> eval env e')
  | Tpapp (e', _) ->
    begin match eval env e' with
    | Rpfn f -> f ()
    | _ -> raise Runtime_error
    end
  | Tpack (_, e', _) -> Rpack (eval env e')
  | Tunpack (v, e1, e2) ->
    begin match eval env e1 with
    | Rpack r -> eval (Dict.add v r env) e2
    | _ -> raise Runtime_error
    end
  | Ttuple el -> Rtuple (List.map (eval env) el)
  | Tproj (e', i) ->
    begin match eval env e' with
    | Rtuple rl -> begin try List.nth rl i with _ -> raise Runtime_error end
    | _ -> raise Runtime_error
    end
  | Tinj (e', i, _) -> Rinj (eval env e', i)
  | Tcase (e', arms) ->
    begin match eval env e' with
    | Rinj (r, i) ->
      let (vi, ei) = try List.nth arms i with _ -> raise Runtime_error in
      eval (Dict.add vi r env) ei
    | _ -> raise Runtime_error
    end
  | Troll (e', _) -> Rroll (eval env e')
  | Tunroll e' ->
    begin match eval env e' with
    | Rroll r -> r
    | _ -> raise Runtime_error
    end
  | Ttag (e1, e2) ->
    begin match eval env e1 with
    | Rtag tag -> Rexn (tag, eval env e2)
    | _ -> raise Runtime_error
    end
  | Tiftag (e1, e2, v, e3, e4) -> 
    begin match (eval env e1, eval env e2) with
    | (Rtag tag, Rexn (tag', r)) ->
      if tag == tag' then
        eval (Dict.add v r env) e3
      else
        eval env e4
    | _ -> raise Runtime_error
    end
  | Tnewtag _ -> Rtag (ref ())
  | Traise (e', _) -> raise (Raise_exn (eval env e'))
  | Thandle (e1, v, e2) -> begin try eval env e1 with Raise_exn r -> eval (Dict.add v r env) e2 end
  | Tref e' -> Rref (ref (eval env e'))
  | Tderef e' ->
    begin match eval env e' with
    | Rref r -> !r
    | _ -> raise Runtime_error
    end
  | Tassign (e1, e2) ->
    begin match eval env e1 with
    | Rref _ -> ignore (eval env e2); Rtuple []
    | _ -> raise Runtime_error
    end
  | Tbool b -> Rbool b
  | Tif (e1, e2, e3) ->
    begin match eval env e1 with
    | Rbool true -> eval env e2
    | Rbool false -> eval env e3
    | _ -> raise Runtime_error
    end
  | Tint i -> Rint i
  | Tchar c -> Rchar c
  | Tstring s -> Rstring s
  | Tlet (v, e1, e2) -> eval (Dict.add v (eval env e1) env) e2
  | Tprim (prim, el) -> Prim_eval.prim_eval prim (List.map (eval env) el)
