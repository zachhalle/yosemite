type kind = Syntax.kind
type constructor = Syntax.constructor
type term = Syntax.term

(* For X in {Kind, Con, Term}:
   substXGen m [s0, .. sn-1] l exp = exp[0 .. m-1 . s0[^m] .. sn-1[^m] . ^l+m]

   If   d = v0 |-> e0 .. vp-1 |-> ep-1
   then dsubstTermGen m [s0, .. sn-1] l d e
        = [e0[^m] .. ep-1[^m] / v0 .. vp-1] (e[0 .. m-1 . s0[^m] .. sn-1[^m] . ^l+m])
 *)

val subst_kind_gen : int -> constructor list -> int -> kind -> kind
val subst_con_gen : int -> constructor list -> int -> constructor -> constructor
val subst_term_gen : int -> constructor list -> int -> term -> term

val lift_kind : int -> kind -> kind
val lift_constructor : int -> constructor -> constructor
val lift_term : int -> term -> term

val subst_kind : constructor -> kind -> kind
val subst_constructor : constructor -> constructor -> constructor
val subst_term : constructor -> term -> term
