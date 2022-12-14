type kind =
  | Ktype
  | Ksing of constructor
  | Kpi of kind * kind     (* binds *)
  | Ksigma of kind * kind  (* binds *)
  | Kunit

and constructor =
  | Cvar of int * int option
  | Clam of kind * constructor         (* binds *)
  | Capp of constructor * constructor
  | Cpair of constructor * constructor
  | Cpi1 of constructor
  | Cpi2 of constructor
  | Cunit

  | Cnot of constructor
  | Cexists of kind * constructor      (* binds *)
  | Cprod of constructor list
  | Csum of constructor list
  | Crec of constructor
  | Ctag of constructor
  | Cref of constructor
  | Cexn
  | Cbool
  | Cint
  | Cchar
  | Cstring

type variable = Var.variable

type expr =
  | Eapp of value * value
  | Eunpack of variable * value * expr
  | Eproj of variable * value * int * expr
  | Ecase of value * (variable * expr) list
  | Eiftag of value * value * variable * expr * expr
  | Enewtag of variable * constructor * expr
  | Eref of variable * value * expr
  | Ederef of variable * value * expr
  | Eassign of value * value * expr
  | Eif of value * expr * expr
  | Elet of variable * value * expr
  | Eprim of variable * Prim.primitive * value list * expr
  | Ehalt

and value =
  | Vvar of variable
  | Vlam of variable * constructor * expr
  | Vpack of constructor * value * constructor
  | Vtuple of value list
  | Vinj of value * int * constructor
  | Vroll of value * constructor
  | Vunroll of value
  | Vtag of value * value
  | Vbool of bool
  | Vint of int
  | Vchar of char
  | Vstring of string

val show_kind : kind -> string
val show_constructor : constructor -> string
val show_expr : expr -> string
val show_value : value -> string
