type kind = Syntax.kind
type constructor = Syntax.constructor
type expr = Syntax.expr
type value = Syntax.value

exception IndexErrorKind of kind * int option * int list
exception IndexErrorConstructor of constructor * int option * int list
exception IndexErrorExpr of expr * int option * int list
exception IndexErrorValue of value * int option * int list

val check_kind : kind -> unit
val check_constructor : constructor -> unit

val impose_kind : int -> kind -> kind
val impose_constructor : int -> constructor -> constructor
val impose_expr : int -> expr -> expr
val impose_value : int -> value -> value
