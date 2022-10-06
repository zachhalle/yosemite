open Direct
open Syntax
open Subst
open Printf

let%expect_test "lift_kind 0 Ktype" = 
  let result = lift_kind 0 Ktype in
  printf "%s\n" (show_kind result);
  [%expect {| Syntax.Ktype |}]

let%expect_test "lift_kind 100 Ktype" =
  let result = lift_kind 100 Ktype in
  printf "%s\n" (show_kind result);
  [%expect {| Syntax.Ktype |}]

let%expect_test "lift_kind 0 (Ksing (Cvar 0))" =
  let result = lift_kind 0 (Ksing (Cvar (0, None))) in
  printf "%s\n" (show_kind result);
  [%expect {| (Syntax.Ksing (Syntax.Cvar (0, None))) |}]

let%expect_test "lift_kind 100 (Ksing (Cvar 0))" =
  let result = lift_kind 100 (Ksing (Cvar (0, None))) in
  printf "%s\n" (show_kind result);
  [%expect {| (Syntax.Ksing (Syntax.Cvar (100, None))) |}]
