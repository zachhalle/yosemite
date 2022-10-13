open Direct
open Syntax
open Subst
open Printf

(* lift_kind *)

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

let%expect_test "lift_kind 50 (Ksing (Cvar 50))" =
  let result = lift_kind 50 (Ksing (Cvar (50, None))) in
  printf "%s\n" (show_kind result);
  [%expect {| (Syntax.Ksing (Syntax.Cvar (100, None))) |}]

let%expect_test "lift_kind 1 (Ksing (Cvar 0))" =
  let result = lift_kind 1 (Ksing (Cvar (0, None))) in
  printf "%s\n" (show_kind result);
  [%expect {| (Syntax.Ksing (Syntax.Cvar (1, None))) |}]

let%expect_test "lift_kind 1 (Kpi (Ksing (Cvar 0), Ksing (Cvar 0)))" =
  let result = lift_kind 1 (Kpi (Ksing (Cvar (0, None)), Ksing (Cvar (0, None)))) in
  printf "%s\n" (show_kind result);
  [%expect {|
    (Syntax.Kpi ((Syntax.Ksing (Syntax.Cvar (1, None))),
       (Syntax.Ksing (Syntax.Cvar (0, None))))) |}]

let%expect_test "lift_kind 1 (Kpi (Ksing (Cvar 0), Ksing (Cvar 0)))" =
  let result = lift_kind 10 (Kpi (Ksing (Cvar (0, None)), Ksing (Cvar (0, None)))) in
  printf "%s\n" (show_kind result);
  [%expect {|
    (Syntax.Kpi ((Syntax.Ksing (Syntax.Cvar (10, None))),
       (Syntax.Ksing (Syntax.Cvar (0, None))))) |}]

let%expect_test "lift_kind 1 (Kpi (Ksing (Cvar 0), Ksing (Cvar 1)))" =
  let result = lift_kind 1 (Kpi (Ksing (Cvar (0, None)), Ksing (Cvar (1, None)))) in
  printf "%s\n" (show_kind result);
  [%expect {|
    (Syntax.Kpi ((Syntax.Ksing (Syntax.Cvar (1, None))),
       (Syntax.Ksing (Syntax.Cvar (2, None))))) |}]

let%expect_test "lift_kind 10 (Kpi (Kunit, Ksing (Cvar 1)))" =
  let result = lift_kind 10 (Kpi (Kunit, Ksing (Cvar (1, None)))) in
  printf "%s\n" (show_kind result);
  [%expect {| (Syntax.Kpi (Syntax.Kunit, (Syntax.Ksing (Syntax.Cvar (11, None))))) |}]

let%expect_test "lift_kind 10 (Kpi (Kunit, Kpi (Kunit, Kpi (Kunit, Ksing (Cvar 0)))))" =
  let result = lift_kind 10 (Kpi (Kunit, Kpi (Kunit, Kpi (Kunit, Ksing (Cvar (0, None)))))) in
  printf "%s\n" (show_kind result);
  [%expect {|
    (Syntax.Kpi (Syntax.Kunit,
       (Syntax.Kpi (Syntax.Kunit,
          (Syntax.Kpi (Syntax.Kunit, (Syntax.Ksing (Syntax.Cvar (0, None)))))))
       )) |}]

let%expect_test "lift_kind 10 (Kpi (Kunit, Kpi (Kunit, Kpi (Kunit, Ksing (Cvar 2)))))" =
  let result = lift_kind 10 (Kpi (Kunit, Kpi (Kunit, Kpi (Kunit, Ksing (Cvar (2, None)))))) in
  printf "%s\n" (show_kind result);
  [%expect {|
    (Syntax.Kpi (Syntax.Kunit,
       (Syntax.Kpi (Syntax.Kunit,
          (Syntax.Kpi (Syntax.Kunit, (Syntax.Ksing (Syntax.Cvar (2, None)))))))
       )) |}]

let%expect_test "lift_kind 10 (Kpi (Kunit, Kpi (Kunit, Kpi (Kunit, Ksing (Cvar 0)))))" =
  let result = lift_kind 10 (Kpi (Kunit, Kpi (Kunit, Kpi (Kunit, Ksing (Cvar (3, None)))))) in
  printf "%s\n" (show_kind result);
  [%expect {|
    (Syntax.Kpi (Syntax.Kunit,
       (Syntax.Kpi (Syntax.Kunit,
          (Syntax.Kpi (Syntax.Kunit, (Syntax.Ksing (Syntax.Cvar (13, None)))))))
       )) |}]

(* lift_constructor *)

let%expect_test "lift_constructor 1 (Cvar 0)" =
  let result = lift_constructor 1 (Cvar (0, None)) in
  printf "%s\n" (show_constructor result);
  [%expect {| (Syntax.Cvar (1, None)) |}]

let%expect_test "lift_constructor 10 (Cvar 0)" =
  let result = lift_constructor 10 (Cvar (0, None)) in
  printf "%s\n" (show_constructor result);
  [%expect {| (Syntax.Cvar (10, None)) |}]

let%expect_test "lift_constrcutor 10 (Clam (Ktype, Cvar 0))" =
  let result = lift_constructor 10 (Clam (Ktype, Cvar (0, None))) in
  printf "%s\n" (show_constructor result);
  [%expect {| (Syntax.Clam (Syntax.Ktype, (Syntax.Cvar (0, None)))) |}]

let%expect_test "lift_constructor 10 (Cexists (Ktype, Cvar 1))" =
  let result = lift_constructor 10 (Cexists (Ktype, Cvar (1, None))) in
  printf "%s\n" (show_constructor result);
  [%expect {| (Syntax.Cexists (Syntax.Ktype, (Syntax.Cvar (11, None)))) |}]

let%expect_test "lift_constructor 10 (Cforall (Cexists (Cforall (Cvar 0))))" =
  let result = lift_constructor 10 (Cforall (Ktype, Cexists (Ktype, (Cforall (Ktype, Cvar (0, None)))))) in
  printf "%s\n" (show_constructor result);
  [%expect {|
    (Syntax.Cforall (Syntax.Ktype,
       (Syntax.Cexists (Syntax.Ktype,
          (Syntax.Cforall (Syntax.Ktype, (Syntax.Cvar (0, None))))))
       )) |}]

let%expect_test "lift_constructor 10 (Cforall (Cexists (Cforall (Cvar 2))))" =
  let result = lift_constructor 10 (Cforall (Ktype, Cexists (Ktype, (Cforall (Ktype, Cvar (2, None)))))) in
  printf "%s\n" (show_constructor result);
  [%expect {|
    (Syntax.Cforall (Syntax.Ktype,
       (Syntax.Cexists (Syntax.Ktype,
          (Syntax.Cforall (Syntax.Ktype, (Syntax.Cvar (2, None))))))
       )) |}]

let%expect_test "lift_constructor 10 (Cforall (Cexists (Cforall (Cvar 3))))" =
  let result = lift_constructor 10 (Cforall (Ktype, Cexists (Ktype, (Cforall (Ktype, Cvar (3, None)))))) in
  printf "%s\n" (show_constructor result);
  [%expect {|
    (Syntax.Cforall (Syntax.Ktype,
       (Syntax.Cexists (Syntax.Ktype,
          (Syntax.Cforall (Syntax.Ktype, (Syntax.Cvar (13, None))))))
       )) |}]

