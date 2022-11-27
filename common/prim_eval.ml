module Prim_eval_fun (
  M : sig
    type result

    exception Runtime_error

    val runit : result
    val rbool : bool -> result
    val rint : int -> result
    val rchar : char -> result
    val rstring : string -> result

    val dest_rbool : result -> bool option
    val dest_rint : result -> int option
    val dest_rchar : result -> char option
    val dest_rstring : result -> string option
  end
) = struct

  type result = M.result

  open M
  open Prim

  let prim_int f rl =
    match List.map dest_rint rl with
    | [Some x] -> f x 
    | _ -> raise Runtime_error

  let prim_int2 f rl =
    match List.map dest_rint rl with
    | [Some x; Some y] -> f x y
    | _ -> raise Runtime_error

  let prim_char2 f rl =
    match List.map dest_rchar rl with
    | [Some x; Some y] -> f x y
    | _ -> raise Runtime_error

  let prim_string f rl =
    match List.map dest_rstring rl with
    | [Some x] -> f x
    | _ -> raise Runtime_error

  let prim_string2 f rl =
    match List.map dest_rstring rl with
    | [Some x; Some y] -> f x y
    | _ -> raise Runtime_error

  (* silence unused warning *)
  let _ = dest_rbool
  let _ = rchar

  let prim_eval prim =
    match prim with
    | Neg -> prim_int (fun x -> rint (-1 * x))
    | Plus -> prim_int2 (fun x y -> rint (x + y))
    | Minus -> prim_int2 (fun x y -> rint (x - y))
    | Times -> prim_int2 (fun x y -> rint (x * y))
    | Div -> prim_int2 (fun x y -> rint (x / y))
    | Mod -> prim_int2 (fun x y -> rint (x mod y))
    | Eq_int -> prim_int2 (fun x y -> rbool (x == y))
    | Lt_int -> prim_int2 (fun x y -> rbool (x < y))
    | Leq_int -> prim_int2 (fun x y -> rbool (x <= y))
    | Int_to_string -> prim_int (fun x -> rstring (string_of_int x))
    | Eq_char -> prim_char2 (fun x y -> rbool (x == y))
    | Concat -> prim_string2 (fun x y -> rbool (x == y))
    | Eq_string -> prim_string2 (fun x y -> rbool (String.equal x y))
    | Lt_string -> prim_string2 (fun x y -> rbool (String.compare x y < 0))
    | Print -> prim_string (fun x -> print_string x; runit)

end
