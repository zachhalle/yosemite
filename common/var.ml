type variable = int [@@deriving show]
type t = variable 

let next = ref 0

let fresh () =
  let v = !next in
  incr next;
  v

let eq (v1 : variable) (v2 : variable) = v1 = v2

let compare = Int.compare
