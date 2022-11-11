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
end
