type kind = Syntax.kind
type constructor = Syntax.constructor
type term = Syntax.term

(* For X in {Kind, Con, Term}:
   substXGen m [s0, .. sn-1] l exp = exp[0 .. m-1 . s0[^m] .. sn-1[^m] . ^l+m]

   If   d = v0 |-> e0 .. vp-1 |-> ep-1
   then dsubstTermGen m [s0, .. sn-1] l d e
        = [e0[^m] .. ep-1[^m] / v0 .. vp-1] (e[0 .. m-1 . s0[^m] .. sn-1[^m] . ^l+m])
 *)

val substKindGen : int -> constructor list -> int -> kind -> kind
val substConGen : int -> constructor list -> int -> constructor -> constructor
val substTermGen : int -> constructor list -> int -> term -> term
val dsubstTermGen : int -> constructor list -> int -> term Map.Make(Var).t -> term -> term

val liftKind : int -> kind -> kind
val liftConstructor : int -> constructor -> constructor
val liftTerm : int -> term -> term

val substKind : constructor -> kind -> kind
val substConstructor : constructor -> constructor -> constructor
val substTerm : constructor -> term -> term
