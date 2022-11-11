type kind = Syntax.kind
type constructor = Syntax.constructor
type term = Syntax.term
type context = Context.context

val whreduce : constructor -> constructor

val whnf : context -> constructor -> constructor

(* equiv ctx c c'
 * 
 * Suppose ctx |- c  : k
 *     and ctx |- c' : k
 * 
 * then if ctx |- c <=> c' : k
 *    then return ()
 *    else raise Type_error
 *)
val equiv : context -> constructor -> constructor -> kind -> unit

val samekind : context -> kind -> kind -> unit

val subkind : context -> kind -> kind -> unit

val selfify : constructor -> kind -> kind

val inhabitant : kind -> constructor

val check_kind : context -> kind -> unit

val infer_constructor : context -> constructor -> kind

val check_constructor : context -> constructor -> kind -> unit

val infer_term : context -> term -> constructor

val check_term : context -> term -> constructor -> unit
