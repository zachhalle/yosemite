module Result : sig 
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

type term = Syntax.term

type result = Result.result

module Dict : Map.S with type key = Var.t

type env = Result.result Dict.t

val eval : env -> term -> result
