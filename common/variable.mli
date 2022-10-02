type variable = int

val fresh : unit -> variable
val eq : variable -> variable -> bool
val compare : variable -> variable -> int
