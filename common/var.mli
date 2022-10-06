type variable = int
type t = variable

val fresh : unit -> variable
val eq : variable -> variable -> bool
val compare : variable -> variable -> int

val pp_variable : Format.formatter -> variable -> unit
val show_variable : variable -> string
