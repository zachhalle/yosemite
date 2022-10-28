open Direct
open Syntax
open Typing
open Printf

(* whreduce *)

let%expect_test "whreduce Cunit" =
  let result = whreduce Cunit in
  printf "%s\n" (show_constructor result);
  [%expect {| Syntax.Cunit |}]

let%expect_test "whreduce (Cpair (Cunit, Cunit))" =
  let result = whreduce (Cpair (Cunit, Cunit)) in
  printf "%s\n" (show_constructor result);
  [%expect {| (Syntax.Cpair (Syntax.Cunit, Syntax.Cunit)) |}]

let%expect_test "whreduce (Clam (Ktype, Cvar 0))" =
  let result = whreduce (Clam (Ktype, Cvar (0, None))) in
  printf "%s\n" (show_constructor result);
  [%expect {| (Syntax.Clam (Syntax.Ktype, (Syntax.Cvar (0, None)))) |}]

let%expect_test "whreduce (Capp (Clam (Ktype, Cvar 0), Cint))" =
  let result = whreduce (Capp (Clam (Ktype, Cvar (0, None)), Cint)) in
  printf "%s\n" (show_constructor result);
  [%expect {| Syntax.Cint |}]

let%expect_test "whreduce (Capp (Clam (Ktype, Cvar 0), Cvar 0))" =
  let result = whreduce (Capp (Clam (Ktype, Cvar (0, None)), Cvar (0, None))) in
  printf "%s\n" (show_constructor result);
  [%expect {| (Syntax.Cvar (0, None)) |}]

let%expect_test "whreduce (Capp (Clam (Ktype, Cvar 0), Cvar 1))" =
  let result = whreduce (Capp (Clam (Ktype, Cvar (0, None)), Cvar (1, None))) in
  printf "%s\n" (show_constructor result);
  [%expect {| (Syntax.Cvar (1, None)) |}]

let%expect_test "whreduce (Capp (Clam (Ktype, Cvar 1), Cvar 0))" =
  let result = whreduce (Capp (Clam (Ktype, Cvar (1, None)), Cvar (0, None))) in
  printf "%s\n" (show_constructor result);
  [%expect {| (Syntax.Cvar (0, None)) |}]

(* note this wouldn't kind-check *)
let%expect_test "whreduce (Capp (Clam (Ksing Cbool, Cvar 0), Cint))" =
  let result = whreduce (Capp (Clam (Ksing Cbool, Cvar (0, None)), Cint)) in
  printf "%s\n" (show_constructor result);
  [%expect {| Syntax.Cint |}]

let%expect_test "whreduce (Cpair (Capp (Clam (Ktype, Cvar 0), Cbool), Cint))" =
  let result = whreduce (Cpair (Capp (Clam (Ktype, Cvar (0, None)), Cbool), Cint)) in
  printf "%s\n" (show_constructor result);
  [%expect {|
    (Syntax.Cpair (
       (Syntax.Capp ((Syntax.Clam (Syntax.Ktype, (Syntax.Cvar (0, None)))),
          Syntax.Cbool)),
       Syntax.Cint)) |}]

let%expect_test "whreduce (Cpi1 (Cpair (Capp (Clam (Ktype, Cvar 0), Cbool), Cint)))" =
  let result = whreduce (Cpi1 (Cpair (Capp (Clam (Ktype, Cvar (0, None)), Cbool), Cint))) in
  printf "%s\n" (show_constructor result);
  [%expect {| Syntax.Cbool |}]

let%expect_test "whreduce (Cpi2 (Cpair (Cint, Capp (Clam (Ktype, Cvar 0), Cbool))))" =
  let result = whreduce (Cpi2 (Cpair (Cint, Capp (Clam (Ktype, Cvar (0, None)), Cbool)))) in
  printf "%s\n" (show_constructor result);
  [%expect {| Syntax.Cbool |}]
