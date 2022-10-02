type kind = Syntax.kind
type constructor = Syntax.constructor
type term = Syntax.term

exception IndexErrorKind of kind * int option * int list
exception IndexErrorConstructor of constructor * int option * int list
exception IndexErrorTerm of term * int option * int list

val checkKind : kind -> unit
val checkConstructor : constructor -> unit

val imposeKind : int -> kind -> kind
val imposeConstructor : int -> constructor -> constructor
val imposeTerm : int -> term -> term
