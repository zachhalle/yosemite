open Cps
open Syntax

open Printf

let%expect_test "show" =
  printf "%s\n" (show_kind Ktype);
  [%expect {| Syntax.Ktype |}]
