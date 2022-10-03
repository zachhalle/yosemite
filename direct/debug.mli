type kind = Syntax.kind
type constructor = Syntax.constructor
type term = Syntax.term

exception IndexErrorKind of kind * int option * int list
exception IndexErrorConstructor of constructor * int option * int list
exception IndexErrorTerm of term * int option * int list

val check_kind : kind -> unit
val check_constructor : constructor -> unit

val impose_kind : int -> kind -> kind
val impose_constructor : int -> constructor -> constructor
val impose_term : int -> term -> term
