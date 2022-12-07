type kind = Syntax.kind
type constructor = Syntax.constructor
type expr = Syntax.expr
type value = Syntax.value

(* For X in {Kind, Con, Term}:
   substXGen m [s0, .. sn-1] l exp = exp[0 .. m-1 . s0[^m] .. sn-1[^m] . ^l+m]

   If   d = v0 |-> e0 .. vp-1 |-> ep-1
   then dsubstTermGen m [s0, .. sn-1] l d e
        = [e0[^m] .. ep-1[^m] / v0 .. vp-1] (e[0 .. m-1 . s0[^m] .. sn-1[^m] . ^l+m])
 *)

val subst_kind_gen : int -> constructor list -> int -> kind -> kind
val subst_constructor_gen : int -> constructor list -> int -> constructor -> constructor
val subst_expr_gen : int -> constructor list -> int -> expr -> expr
val subst_value_gen : int -> constructor list -> int -> value -> value

val lift_kind : int -> kind -> kind
val lift_constructor : int -> constructor -> constructor
val lift_expr : int -> expr -> expr
val lift_value : int -> value -> value

val subst_kind : constructor -> kind -> kind
val subst_constructor : constructor -> constructor -> constructor
val subst_expr : constructor -> expr -> expr
val subst_value : constructor -> value -> value
