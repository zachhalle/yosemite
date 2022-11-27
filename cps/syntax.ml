open Var

type kind =
  | Ktype
  | Ksing of constructor
  | Kpi of kind * kind     (* binds *)
  | Ksigma of kind * kind  (* binds *)
  | Kunit
  [@@deriving show]

and constructor =
  | Cvar of int * int option
  | Clam of kind * constructor         (* binds *)
  | Capp of constructor * constructor
  | Cpair of constructor * constructor
  | Cpi1 of constructor
  | Cpi2 of constructor
  | Cunit of constructor

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
  [@@deriving show]

type variable = Var.variable

type expr =
  | Eapp of value * value
  | Eunpack of variable * value * expr
  | Ecase of value * (variable * expr) list
  | Eiftag of value * value * variable * expr * expr
  | Enewtag of variable * constructor * expr
  | Eref of variable * value * expr
  | Ederef of variable * value * expr
  | Eif of value * expr * expr
  | Elet of variable * value * expr
  | Eprim of variable * Prim.primitive * value list * expr
  | Ehalt
  [@@deriving show]

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
  [@@deriving show]
