type variable = int
type t = variable

val fresh : unit -> variable
val eq : variable -> variable -> bool
val compare : variable -> variable -> int
