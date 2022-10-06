type primitive =
   | Neg
   | Plus
   | Minus
   | Times
   | Div
   | Mod
   | Eq_int
   | Lt_int
   | Leq_int
   | Int_to_string
   
   | Eq_char
   
   | Concat
   | Eq_string
   | Lt_string
   | Print

val pp_primitive : Format.formatter -> primitive -> unit
val show_primitive : primitive -> string
