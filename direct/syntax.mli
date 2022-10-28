type kind =
  | Ktype
  | Ksing of constructor
  | Kpi of kind * kind     (* binds *)
  | Ksigma of kind * kind  (* binds *)
  | Kunit

and constructor =
  | Cvar of int * int option
  | Clam of kind * constructor     (* binds *)
  | Capp of constructor * constructor
  | Cpair of constructor * constructor
  | Cpi1 of constructor
  | Cpi2 of constructor
  | Cunit

  | Carrow of constructor * constructor
  | Cforall of kind * constructor  (* binds *)
  | Cexists of kind * constructor  (* binds *)
  | Cprod of constructor list
  | Csum of constructor list
  | Crec of constructor            (* binds *)
  | Ctag of constructor
  | Cref of constructor
  | Cexn
  | Cbool
  | Cint
  | Cchar
  | Cstring

type variable = Var.variable

type term =
  | Tvar of variable
  
  | Tlam of variable * constructor * term
  | Tapp of term * term
  
  | Tplam of kind * term  (* binds *) 
  | Tpapp of term * constructor

  | Tpack of constructor * term * constructor
  | Tunpack of variable * term * term  (* binds *)

  | Ttuple of term list
  | Tproj of term * int

  | Tinj of term * int * constructor
  | Tcase of term * (variable * term) list

  | Troll of term * constructor
  | Tunroll of term

  | Ttag of term * term
  | Tiftag of term * term * variable * term * term
  | Tnewtag of constructor

  | Traise of term * constructor
  | Thandle of term * variable * term

  | Tref of term
  | Tderef of term
  | Tassign of term * term

  | Tbool of bool
  | Tif of term * term * term

  | Tint of int
  | Tchar of char
  | Tstring of string

  | Tlet of variable * term * term

  | Tprim of Prim.primitive * term list

val show_kind : kind -> string
val show_constructor : constructor -> string
val show_term : term -> string
