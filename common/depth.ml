type depth =
  | AtLeast of int (* >= 0 *)
  | Exactly of int (* >= 0 *)

let bottom = AtLeast 0

let inc d =
  match d with
  | AtLeast n -> AtLeast (n+1)
  | Exactly n -> Exactly (n+1)

let dec exn d =
  match d with
  | AtLeast 0 -> d
  | AtLeast n -> AtLeast (n-1)
  | Exactly 0 -> raise exn
  | Exactly n -> Exactly (n-1)

let join exn d1 d2 =
  match d1, d2 with
  | AtLeast n1, AtLeast n2 ->
    if n1 >= n2 then
      d1
    else
      d2
  | AtLeast n1, Exactly n2 ->
    if n2 >= n1 then
      d2
    else
      raise exn
  | Exactly n1, AtLeast n2 ->
    if n1 >= n2 then
      d1
    else
      raise exn
  | Exactly n1, Exactly n2 ->
    if n1 = n2 then
      d1
    else
      raise exn
