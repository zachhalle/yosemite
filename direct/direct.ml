type kind =
  | Ktype
  | Ksing of constructor
  | Kpi of kind * kind     (* binds *)
  | Ksigma of kind * kind  (* binds *)
  | Kunit 

and constructor =
  | Cvar of int * int option
  | Clam of kind * constructor
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
  | Cexn
  | Cbool
  | Cint
  | Cchar
  | Cstring
