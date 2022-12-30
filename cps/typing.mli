type kind = Syntax.kind
type constructor = Syntax.constructor
type context = Context.context

val whreduce : constructor -> constructor
val whnf : context -> constructor -> constructor
val equiv : context -> constructor -> constructor -> kind -> unit
val samekind : context -> kind -> kind -> unit
val subkind : context -> kind -> kind -> unit
val selfify : constructor -> kind -> kind
val inhabitant : kind -> constructor
