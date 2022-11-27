module Prim_eval_fun (
  M : sig
    type result

    exception Runtime_error

    val runit : result
    val rbool : bool -> result
    val rint : int -> result
    val rchar : char -> result
    val rstring : string -> result

    val dest_rbool : result -> bool option
    val dest_rint : result -> int option
    val dest_rchar : result -> char option
    val dest_rstring : result -> string option
  end
) : sig
  type result = M.result

  val prim_eval : Prim.primitive -> result list -> result
end
