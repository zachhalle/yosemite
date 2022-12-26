module ContextFun (
  Debug : sig
    val impose_kind : int -> Syntax.kind -> Syntax.kind
    val impose_constructor : int -> Syntax.constructor -> Syntax.constructor
  end
) = struct

  module Dict = Map.Make(Var)
  open Debug
  open Type_error

  type kind = Syntax.kind
  type constructor = Syntax.constructor
  type variable = Var.variable

  type context = { ksize : int ; kctx : kind list ; tctx : (int * constructor) Dict.t }

  let empty = { ksize = 0 ; kctx = [] ; tctx = Dict.empty }

  let lookup_kind ({ kctx ; _ } : context) i =
    try Subst.lift_kind (i+1) (List.nth kctx i) with
    | Failure _  | Invalid_argument _ -> raise Type_error

  let lookup_type ({ ksize ; tctx ; _ } : context) v =
    let n, c = try Dict.find v tctx with Not_found -> raise Type_error in
    Subst.lift_constructor (ksize-n) c

  let extend_kind { ksize ; kctx ; tctx } k =
    { ksize = ksize + 1 ; 
      kctx = impose_kind ksize k :: kctx ;
      tctx = tctx }

  let extend_type { ksize ; kctx ; tctx } v c =
    { ksize = ksize ;
      kctx = kctx ;
      tctx = Dict.add v (ksize, impose_constructor ksize c) tctx }

  let ksize ({ ksize ; _ } : context) = ksize
end

include ContextFun(
  struct
    (* replace these with identity functions for less error checking but more performance *)
    let impose_kind = Debug.impose_kind
    let impose_constructor = Debug.impose_constructor
  end
)
