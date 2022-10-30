open Direct
open Context
open Debug
open Syntax
open Typing
open Type_error
open Printf

let handle_error show_result f =
  try
    let result = f () in
    printf "%s\n" (show_result result)
  with
  | Type_error -> printf "Uncaught exception: Type_error.\n"
  | IndexErrorKind _ -> printf "Uncaught exception: IndexErrorKind.\n"

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

let%expect_test "whreduce (Cvar 0)" =
  let result = whreduce (Cvar (0, None)) in
  printf "%s\n" (show_constructor result);
  [%expect {| (Syntax.Cvar (0, None)) |}]

(* whnf *)

let%expect_test "whnf empty_context (Cvar 0)" =
  let f () = whnf empty (Cvar (0, None)) in
  handle_error show_constructor f;
  [%expect {| Uncaught exception: Type_error. |}]

let%expect_test "whnf {Ktype} (Cvar 0)" =
  let f () = whnf (extend_kind empty Ktype) (Cvar (0, None)) in
  handle_error show_constructor f;
  [%expect {| (Syntax.Cvar (0, None)) |}]

let%expect_test "whnf {Ksing Cint} (Cvar 0)" =
  let f () = whnf (extend_kind empty (Ksing Cint)) (Cvar (0, None)) in
  handle_error show_constructor f;
  [%expect {| Syntax.Cint |}]

let%expect_test "whnf {Ksing (Cvar 0)} (Cvar 0)" =
  let f () = whnf (extend_kind empty (Ksing (Cvar (0, None)))) (Cvar (0, None)) in
  handle_error show_constructor f;
  [%expect {| Uncaught exception: IndexErrorKind. |}]

let%expect_test "whnf {Ksing (Cvar 0), Ksing Cint} (Cvar 0)" =
  let f () = whnf (extend_kind (extend_kind empty (Ksing (Cvar (0, None)))) (Ksing Cint)) (Cvar (0, None)) in
  handle_error show_constructor f;
  [%expect {| Uncaught exception: IndexErrorKind. |}]

let%expect_test "whnf {Ksing Cint, Ksing (Cvar 0)} (Cvar 0)" =
  let f () = whnf (extend_kind (extend_kind empty (Ksing Cint)) (Ksing (Cvar (0, None)))) (Cvar (0, None)) in
  handle_error show_constructor f;
  [%expect {| Syntax.Cint |}]

let%expect_test "whnf {Ksing Cint, Ksing (Cvar 0)} (Cvar 0)" =
  let f () = whnf (extend_kind (extend_kind empty (Ksing Cint)) (Ksing (Cvar (0, None)))) (Cvar (1, None)) in
  handle_error show_constructor f;
  [%expect {| Syntax.Cint |}]

let%expect_test "whnf {Ksing (Capp (Clam (Ktype, Cvar 0), Cint))} (Cvar 0)" =
  let f () = whnf (extend_kind empty (Ksing (Capp (Clam (Ktype, Cvar (0, None)), Cint)))) (Cvar (0, None)) in
  handle_error show_constructor f;
  [%expect {| Syntax.Cint |}]

let%expect_test "whnf {Ksing (Capp (Clam (Ksing Cint, Cvar 0), Cint))} (Cvar 0)" =
  let f () = whnf (extend_kind empty (Ksing (Capp (Clam (Ksing Cint, Cvar (0, None)), Cint)))) (Cvar (0, None)) in
  handle_error show_constructor f;
  [%expect {| Syntax.Cint |}]

(* note this wouldn't kind-check *)
let%expect_test "whnf {Ksing (Capp (Clam (Ksing Cbool, Cvar 0), Cint))} (Cvar 0)" =
  let f () = whnf (extend_kind empty (Ksing (Capp (Clam (Ksing Cbool, Cvar (0, None)), Cint)))) (Cvar (0, None)) in
  handle_error show_constructor f;
  [%expect {| Syntax.Cint |}]

let%expect_test "whnf {Ksing (Capp (Clam (Ktype, Cvar 0), Cint))} (Cvar 0)" =
  let f () =
    whnf (extend_kind empty (Ksing (Capp (Clam (Ktype, Cvar (0, None)), (Cvar (0, None)))))) (Cvar (0, None))
  in
  handle_error show_constructor f;
  [%expect {| Uncaught exception: IndexErrorKind. |}]

let%expect_test "whnf {Ktype, Ksing (Capp (Clam (Ktype, Cvar 0), Cint))} (Cvar 0)" =
  let f () =
    whnf (
      extend_kind
        (extend_kind empty Ktype) 
        (Ksing (Capp (Clam (Ktype, Cvar (0, None)), (Cvar (0, None))))))
    (Cvar (0, None))
  in
  handle_error show_constructor f;
  [%expect {| (Syntax.Cvar (1, (Some 2))) |}]

let%expect_test "whnf {Ksing Cbool, Ksing (Capp (Clam (Ktype, Cvar 0), Cint))} (Cvar 0)" =
  let f () =
    whnf (
      extend_kind
        (extend_kind empty (Ksing Cbool))
        (Ksing (Capp (Clam (Ktype, Cvar (0, None)), (Cvar (0, None))))))
    (Cvar (0, None))
  in
  handle_error show_constructor f;
  [%expect {| Syntax.Cbool |}]

let%expect_test "whnf {Ksing Cbool, Ksing (Capp (Clam (Ktype, Cvar 0), Cint))} (Cvar 1)" =
  let f () =
    whnf (
      extend_kind
        (extend_kind empty (Ksing Cbool))
        (Ksing (Capp (Clam (Ktype, Cvar (0, None)), (Cvar (0, None))))))
    (Cvar (1, None))
  in
  handle_error show_constructor f;
  [%expect {| Syntax.Cbool |}]

let%expect_test "whnf {Ksing Cbool, Ksing (Capp (Clam (Ktype, Cvar 0), Cvar 1))} (Cvar 0)" =
  let f () =
    whnf (
      extend_kind
        (extend_kind empty (Ksing Cbool))
        (Ksing (Capp (Clam (Ktype, Cvar (0, None)), (Cvar (1, None))))))
    (Cvar (0, None))
  in
  handle_error show_constructor f;
  [%expect {| Uncaught exception: IndexErrorKind. |}]

let%expect_test "whnf {Ksing Cbool, Ksing (Capp (Clam (Ktype, Cvar 0), Cvar 1))} (Cvar 1)" =
  let f () =
    whnf (
      extend_kind
        (extend_kind empty (Ksing Cbool))
        (Ksing (Capp (Clam (Ktype, Cvar (0, None)), (Cvar (1, None))))))
    (Cvar (1, None))
  in
  handle_error show_constructor f;
  [%expect {| Uncaught exception: IndexErrorKind. |}]

