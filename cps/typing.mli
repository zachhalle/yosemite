type kind = Syntax.kind
type constructor = Syntax.constructor
type expr = Syntax.expr
type value = Syntax.value
type context = Context.context

val whreduce : constructor -> constructor
val whnf : context -> constructor -> constructor
val equiv : context -> constructor -> constructor -> kind -> unit
val samekind : context -> kind -> kind -> unit
val subkind : context -> kind -> kind -> unit
val selfify : constructor -> kind -> kind
val inhabitant : kind -> constructor

val check_kind : context -> kind -> unit
val infer_constructor : context -> constructor -> kind
val check_constructor : context -> constructor -> kind -> unit
val check_expr : context -> expr -> unit
val infer_value : context -> value -> constructor
val check_value : context -> value -> constructor -> unit

val check_program : expr -> unit
