module Prim_type_fun (
  M : sig
    type constructor

    val unit_t   : constructor
    val bool_t   : constructor
    val int_t    : constructor
    val char_t   : constructor
    val string_t : constructor
  end
) : sig
  type constructor = M.constructor
  val prim_type : Prim.primitive -> constructor list * constructor
end = struct

  type constructor = M.constructor

  open M
  open Prim

  let prim_type p =
    match p with
    | Neg -> ([int_t], int_t)
    | Plus -> ([int_t; int_t], int_t)
    | Minus -> ([int_t; int_t], int_t)
    | Times -> ([int_t; int_t], int_t)
    | Div -> ([int_t; int_t], int_t)
    | Mod -> ([int_t; int_t], int_t)
    | Eq_int -> ([int_t; int_t], int_t)
    | Lt_int -> ([int_t; int_t], int_t)
    | Leq_int -> ([int_t; int_t], int_t)
    | Int_to_string -> ([int_t], string_t)
    | Eq_char -> ([char_t; char_t], bool_t)
    | Concat -> ([string_t; string_t], string_t)
    | Eq_string -> ([string_t; string_t], bool_t)
    | Lt_string -> ([string_t; string_t], bool_t)
    | Print -> ([string_t], unit_t)
end
