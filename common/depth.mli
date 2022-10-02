type depth =
  | AtLeast of int (* >= 0 *)
  | Exactly of int (* >= 0 *)

val bottom : depth
val inc : depth -> depth
val dec : exn -> depth -> depth
val join : exn -> depth -> depth -> depth
