type kind = Syntax.kind
type constructor = Syntax.constructor
type variable = Var.variable

type context

val empty : context

val lookupKind : context -> int -> kind
val lookupType : context -> variable -> constructor

val extendKind : context -> kind -> context
val extendType : context -> variable -> constructor -> context

val ksize : context -> int
