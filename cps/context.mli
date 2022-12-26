type kind = Syntax.kind
type constructor = Syntax.constructor
type variable = Var.variable

type context

val empty : context

val lookup_kind : context -> int -> kind
val lookup_type : context -> variable -> constructor

val extend_kind : context -> kind -> context
val extend_type : context -> variable -> constructor -> context

val ksize : context -> int
