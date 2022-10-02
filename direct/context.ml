module ContextFun (
  Debug : sig
    val imposeKind : int -> Syntax.kind -> Syntax.kind
    val imposeConstructor : int -> Syntax.constructor -> Syntax.constructor
  end
) = struct

  module Dict = Map.Make(Var)
  open Debug

  type kind = Syntax.kind
  type constructor = Syntax.constructor
  type variable = Var.variable

  type context = { ksize : int ; kctx : kind list ; tctx : (int * constructor) Dict.t }

  let empty = { ksize = 0 ; kctx = [] ; tctx = Dict.empty }

  let lookupKind ({ kctx ; _ } : context) i =
    try Subst.liftKind (i+1) (List.nth kctx i) with
    | Failure _ -> failwith "Type error."

  let lookupType ({ ksize ; tctx ; _ } : context) v =
    let n, c = try Dict.find v tctx with Not_found -> failwith "Type error." in
    Subst.liftConstructor (ksize-n) c

  let extendKind { ksize ; kctx ; tctx } k =
    { ksize = ksize + 1 ; 
      kctx = imposeKind ksize k :: kctx ;
      tctx = tctx }

  let extendType { ksize ; kctx ; tctx } v c =
    { ksize = ksize ;
      kctx = kctx ;
      tctx = Dict.add v (ksize, imposeConstructor ksize c) tctx }

  let ksize ({ ksize ; _ } : context) = ksize
end

include ContextFun(
  struct
    (* replace these with identity functions for less error checking but more performance *)
    let imposeKind = Debug.imposeKind
    let imposeConstructor = Debug.imposeConstructor
  end
)
