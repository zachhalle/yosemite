open Direct

open Context
open Debug
open Subst
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

(* equiv *)

let show_unit () = "()"

let%expect_test "equiv {} Cexn Cexn : Ktype" =
  let f () = equiv empty Cexn Cexn Ktype in
  handle_error show_unit f;
  [%expect {| () |}]

let%expect_test "equiv {} Cint Cexn : Ktype" =
  let f () = equiv empty Cint Cexn Ktype in
  handle_error show_unit f;
  [%expect {| Uncaught exception: Type_error. |}]

let%expect_test "equiv {} (Carrow (Cint, Cbool)) (Carrow (Cint, Cbool)) : Ktype" =
  let f () = equiv empty (Carrow (Cint, Cbool)) (Carrow (Cint, Cbool)) Ktype in
  handle_error show_unit f;
  [%expect {| () |}]

let%expect_test "equiv {Ktype} (Cvar 0) (Cint) : Ktype" =
  let f () = equiv (extend_kind empty Ktype) (Cvar (0, None)) Cint Ktype in
  handle_error show_unit f;
  [%expect {| Uncaught exception: Type_error. |}]

let%expect_test "equiv {Ksing Cint} (Cvar 0) (Cint) : Ksing Cint" =
  let f () = equiv (extend_kind empty (Ksing Cint)) (Cvar (0, None)) Cint (Ksing Cint) in
  handle_error show_unit f;
  [%expect {| () |}]

(* this one is worth studying *)
let%expect_test "equiv {Kpi (Ktype, Ksing (Cvar (0, None)))} (Capp (Cvar 0, Cint)) Cint : Ktype" =
  let f () =
    equiv
      (extend_kind empty (Kpi (Ktype, Ksing (Cvar (0, None)))))
      (Capp (Cvar (0, None), Cint))
      Cint
      Ktype
  in
  handle_error show_unit f;
  [%expect {| () |}]

let%expect_test "equiv {Kpi (Ktype, Ktype)} (Capp (Cvar 0, Cint)) Cint : Ktype" =
  let f () =
    equiv
      (extend_kind empty (Kpi (Ktype, Ktype)))
      (Capp (Cvar (0, None), Cint))
      Cint
      Ktype
  in
  handle_error show_unit f;
  [%expect {| Uncaught exception: Type_error. |}]

let%expect_test "equiv {} (Capp (Clam (Ktype, Cvar 0), Cexn)) Cexn : Ktype" =
  let f () = equiv empty (Capp (Clam (Ktype, Cvar (0, None)), Cexn)) Cexn Ktype in
  handle_error show_unit f;
  [%expect {| () |}]

let%expect_test "equiv {} (Capp (Clam (Ktype, Cvar 0), Cexn)) Cexn : Ktype" =
  let f () = equiv empty (Capp (Clam (Ksing Cexn, Cvar (0, None)), Cexn)) Cexn Ktype in
  handle_error show_unit f;
  [%expect {| () |}]

let%expect_test "equiv {} (Capp (Clam (Ktype, Cexn), Cint)) Cexn : Ktype" =
  let f () = equiv empty (Capp (Clam (Ktype, Cexn), Cint)) Cexn Ktype in
  handle_error show_unit f;
  [%expect {| () |}]

let%expect_test "equiv {} (Capp (Clam (Ktype, Cint), Cexn)) Cexn : Ktype" =
  let f () = equiv empty (Capp (Clam (Ktype, Cint), Cexn)) Cexn Ktype in
  handle_error show_unit f;
  [%expect {| Uncaught exception: Type_error. |}]

let%expect_test "equiv {} (Cpi1 (Cpair (Cexn, Cint))) Cexn : Ktype" =
  let f () = equiv empty (Cpi1 (Cpair (Cexn, Cint))) Cexn Ktype in
  handle_error show_unit f;
  [%expect {| () |}]

let%expect_test "equiv {} (Cpi2 (Cpair (Cexn, Cint))) Cexn : Ktype" =
  let f () = equiv empty (Cpi2 (Cpair (Cexn, Cint))) Cexn Ktype in
  handle_error show_unit f;
  [%expect {| Uncaught exception: Type_error. |}]

let%expect_test "equiv {} (Cpi2 (Cpair (Cexn, Cint))) Cexn : Ktype" =
  let f () = equiv empty (Cpi2 (Cpair (Cexn, Cint))) Cint Ktype in
  handle_error show_unit f;
  [%expect {| () |}]

let%expect_test "equiv {} (Capp (Clam (Ktype, Cpi1 (Cvar 0)), Cpair (Cexn, Cint))) Cexn : Ktype" =
  let f () =
    equiv empty
      (Capp (Clam (Ktype, Cpi1 (Cvar (0, None))), Cpair (Cexn, Cint)))
      Cexn
      Ktype
  in
  handle_error show_unit f;
  [%expect {| () |}]

let%expect_test
  "equiv {} (Capp (Clam (Ktype, Cpair (Cvar 0, Cvar 0)), Cbool)) (Cpair (Cbool, Cbool)) : Ksigma (Ktype, Ktype)" =
  let f () =
    equiv empty
      (Capp (Clam (Ktype, Cpair (Cvar (0, None), Cvar (0, None))), Cbool))
      (Cpair (Cbool, Cbool))
      (Ksigma (Ktype, Ktype))
  in
  handle_error show_unit f;
  [%expect {| () |}]

let%expect_test "equiv {} (Clam (Ktype, Cvar (0, None))) (Clam (Ktype, Cvar (0, None))) : Kpi (Ktype, Ktype)" =
  let f () = equiv empty (Clam (Ktype, Cvar (0, None))) (Clam (Ktype, Cvar (0, None))) (Kpi (Ktype, Ktype)) in
  handle_error show_unit f;
  [%expect {| () |}]

let%expect_test
  "equiv {} (Clam (Ktype, Cvar (0, None))) (Clam (Ktype, Cvar (0, None))) : Kpi (Ktype, Ksing (Cvar 0))" =
  let f () =
    equiv empty
      (Clam (Ktype, Cvar (0, None)))
      (Clam (Ktype, Cvar (0, None)))
      (Kpi (Ktype, Ksing (Cvar (0, None))))
  in
  handle_error show_unit f;
  [%expect {| () |}]


let%expect_test "equiv {} (Clam (Ktype, Cint)) (Clam (Ktype, Cvar (0, None))) : Kpi (Ktype, Ktype)" =
  let f () = equiv empty (Clam (Ktype, Cint)) (Clam (Ktype, Cvar (0, None))) (Kpi (Ktype, Ktype)) in
  handle_error show_unit f;
  [%expect {| Uncaught exception: Type_error. |}]

let%expect_test "equiv {} (Clam (Ksing Cint, Cint)) (Clam (Ktype, Cvar (0, None))) : Kpi (Ksing Cint, Ksing Cint)" =
  let f () =
    equiv empty
      (Clam (Ksing Cint, Cint))
      (Clam (Ksing Cint, Cvar (0, None)))
      (Kpi (Ksing Cint, Ksing Cint))
  in
  handle_error show_unit f;
  [%expect {| () |}]

let%expect_test "equiv {} (Clam (Ktype, Cint)) (Clam (Ksing Cint, Cvar (0, None))) : Kpi (Ksing Cint, Ksing Cint)" =
  let f () =
    equiv empty
      (Clam (Ktype, Cint))
      (Clam (Ksing Cint, Cvar (0, None)))
      (Kpi (Ksing Cint, Ksing Cint))
  in
  handle_error show_unit f;
  [%expect {| () |}]

let%expect_test "equiv {} (Clam (Ktype, Cint)) (Clam (Ktype, Cvar (0, None))) : Kpi (Ksing Cint, Ksing (Cvar 0))" =
  let f () =
    equiv empty
      (Clam (Ktype, Cint))
      (Clam (Ktype, Cvar (0, None)))
      (Kpi (Ksing Cint, Ksing (Cvar (0, None))))
  in
  handle_error show_unit f;
  [%expect {| () |}]

let%expect_test "equiv {} (fst) (flip snd) : Kpi (Ktype, Kpi (Ktype, Ktype))" =
  let f () =
    let fst = Clam (Ktype, Clam (Ktype, Cvar (1, None))) in
    let snd = Clam (Ktype, Clam (Ktype, Cvar (0, None))) in
    let flip =
      Clam (Kpi (Ktype, Kpi (Ktype, Ktype)),
        Clam (Ktype,
          Clam (Ktype, Capp (Capp (Cvar (2, None), Cvar (0, None)), Cvar (1, None)))))
    in
    equiv empty fst (Capp (flip, snd)) (Kpi (Ktype, Kpi (Ktype, Ktype)))
  in
  handle_error show_unit f;
  [%expect {| () |}]

let%expect_test "equiv {} (Cpair (Cexn, Cint)) (Cpair (Cexn, Cint)) : Ksigma (Ktype, Ktype)" =
  let f () = equiv empty (Cpair (Cexn, Cint)) (Cpair (Cexn, Cint)) (Ksigma (Ktype, Ktype)) in
  handle_error show_unit f;
  [%expect {| () |}]

let%expect_test "equiv {} (Cpair (Cexn, Cint)) (Cpair (Cexn, Cbool)) : Ksigma (Ktype, Ktype)" =
  let f () = equiv empty (Cpair (Cexn, Cint)) (Cpair (Cexn, Cbool)) (Ksigma (Ktype, Ktype)) in
  handle_error show_unit f;
  [%expect {| Uncaught exception: Type_error. |}]

let%expect_test "equiv {} (Cpair (Cexn, Cint)) (Cpair (Cbool, Cint)) : Ksigma (Ktype, Ktype)" =
  let f () = equiv empty (Cpair (Cexn, Cint)) (Cpair (Cbool, Cint)) (Ksigma (Ktype, Ktype)) in
  handle_error show_unit f;
  [%expect {| Uncaught exception: Type_error. |}]

let%expect_test "equiv {0 : Cbool} (Cpair (Cexn, Cbool)) (Cpair (Cexn, Cvar 0)) : Ksigma (Ktype, Ktype)" =
  let f () =
    equiv
      (extend_kind empty (Ksing Cbool)) 
      (Cpair (Cexn, Cbool))
      (Cpair (Cexn, Cvar (0, None)))
      (Ksigma (Ktype, Ktype))
  in
  handle_error show_unit f;
  [%expect {| () |}]

(* samekind *)

let%expect_test "samekind empty Ktype Ktype" =
  let f () = samekind empty Ktype Ktype in
  handle_error show_unit f;
  [%expect {| () |}]

let%expect_test "samekind empty (Ksing (Cvar 0)) (Ksing (Cvar 0))" =
  let f () = samekind empty (Ksing (Cvar (0, None))) (Ksing (Cvar (0, None))) in
  handle_error show_unit f;
  [%expect {| Uncaught exception: Type_error. |}]

let%expect_test "samekind {0 : Ktype} (Ksing (Cvar 0)) (Ksing (Cvar 0))" =
  let f () = samekind (extend_kind empty Ktype) (Ksing (Cvar (0, None))) (Ksing (Cvar (0, None))) in
  handle_error show_unit f;
  [%expect {| () |}]

let%expect_test "samekind {} (Kpi (Ktype, Ktype)) (Kpi (Ktype, Ktype))" =
  let f () = samekind empty (Kpi (Ktype, Ktype)) (Kpi (Ktype, Ktype)) in
  handle_error show_unit f;
  [%expect {| () |}]

let%expect_test "samekind {} (Kpi (Ksing Cexn, Ktype)) (Kpi (Ktype, Ktype))" =
  let f () = samekind empty (Kpi (Ktype, Ktype)) (Kpi (Ktype, Ktype)) in
  handle_error show_unit f;
  [%expect {| () |}]

let%expect_test "samekind {} (Ksigma (Ktype, Ktype)) (Ksigma (Ktype, Ktype))" =
  let f () = samekind empty (Ksigma (Ktype, Ktype)) (Ksigma (Ktype, Ktype)) in
  handle_error show_unit f;
  [%expect {| () |}]

let%expect_test "samekind {} (Ksigma (Ksing Cint, Ktype)) (Ksigma (Ktype, Ktype))" =
  let f () = samekind empty (Ksigma (Ksing Cint, Ktype)) (Ksigma (Ktype, Ktype)) in
  handle_error show_unit f;
  [%expect {| Uncaught exception: Type_error. |}]

(* subkind *)

let%expect_test "subkind empty Ktype Ktype" =
  let f () = subkind empty Ktype Ktype in
  handle_error show_unit f;
  [%expect {| () |}]

let%expect_test "subkind {0 : Ksing Cint} (Ksing Cint) (Ksing (Cvar 0))" =
  let f () = subkind (extend_kind empty (Ksing Cint)) (Ksing Cint) (Ksing (Cvar (0, None))) in
  handle_error show_unit f;
  [%expect {| () |}]

let%expect_test "subkind empty (Ksing Cint) (Ksing (Capp (Clam (Ktype, Cvar 0), Cint)))" =
  let f () = subkind empty (Ksing Cint) (Ksing (Capp (Clam (Ktype, Cvar (0, None)), Cint))) in
  handle_error show_unit f;
  [%expect {| () |}]

let%expect_test "subkind empty (Kpi (Ktype, Ktype)) (Kpi (Ktype, Ktype))" =
  let f () = subkind empty (Kpi (Ktype, Ktype)) (Kpi (Ktype, Ktype)) in
  handle_error show_unit f;
  [%expect {| () |}]

let%expect_test "subkind empty (Kpi (Ksing Cint, Ktype)) (Kpi (Ktype, Ktype))" =
  let f () = subkind empty (Kpi (Ksing Cint, Ktype)) (Kpi (Ktype, Ktype)) in
  handle_error show_unit f;
  [%expect {| Uncaught exception: Type_error. |}]

let%expect_test "subkind empty (Kpi (Ksing Cint, Ktype)) (Kpi (Ktype, Ktype))" =
  let f () = subkind empty (Kpi (Ktype, Ktype)) (Kpi (Ksing Cint, Ktype)) in
  handle_error show_unit f;
  [%expect {| () |}]

(* check_kind *)

let%expect_test "check_kind empty Kunit" =
  let f () = check_kind empty Kunit in
  handle_error show_unit f;
  [%expect {| () |}]

let%expect_test "check_kind empty Ktype" =
  let f () = check_kind empty Ktype in
  handle_error show_unit f;
  [%expect {| () |}]

let%expect_test "check_kind empty (Ksing (Cvar 0))" =
  let f () = check_kind empty (Ksing (Cvar (0, None))) in
  handle_error show_unit f;
  [%expect {| Uncaught exception: Type_error. |}]

let%expect_test "check_kind empty (Kpi (Ktype, Ksing (Cvar (0, None))))" =
  let f () = check_kind empty (Kpi (Ktype, Ksing (Cvar (0, None)))) in
  handle_error show_unit f;
  [%expect {| () |}]

let%expect_test "check_kind empty (Kpi (Kunit, Ksing (Cvar (0, None))))" =
  let f () = check_kind empty (Kpi (Kunit, Ksing (Cvar (0, None)))) in
  handle_error show_unit f;
  [%expect {| Uncaught exception: Type_error. |}]

let%expect_test "check_kind empty (Ksigma (Ktype, Ksing (Cvar (0, None))))" =
  let f () = check_kind empty (Ksigma (Ktype, Ksing (Cvar (0, None)))) in
  handle_error show_unit f;
  [%expect {| () |}]

let%expect_test "check_kind empty (Ksigma (Kunit, Ksing (Cvar (0, None))))" =
  let f () = check_kind empty (Ksigma (Kunit, Ksing (Cvar (0, None)))) in
  handle_error show_unit f;
  [%expect {| Uncaught exception: Type_error. |}]

let%expect_test "check_kind {0 : Ktype} (Kpi (Kunit, Ksing (Cvar (1, None))))" =
  let f () = check_kind (extend_kind empty Ktype) (Kpi (Kunit, Ksing (Cvar (1, None)))) in
  handle_error show_unit f;
  [%expect {| () |}]

(* infer_constructor *)

let%expect_test "infer_constructor empty (Cvar 0)" =
  let f () = infer_constructor empty (Cvar (0, None)) in
  handle_error show_kind f;
  [%expect {| Uncaught exception: Type_error. |}]

let%expect_test "infer_constructor {0 : Ktype} (Cvar 0)" =
  let f () = infer_constructor (extend_kind empty Ktype) (Cvar (0, None)) in
  handle_error show_kind f;
  [%expect {| (Syntax.Ksing (Syntax.Cvar (0, None))) |}]

let%expect_test "infer_constructor {1 : Ktype, 0 : Ksing (Cvar 0)} (Cvar 0)" =
  let f () =
    let ctx = extend_kind (extend_kind empty Ktype) (Ksing (Cvar (0, None))) in
    infer_constructor ctx (Cvar (0, None))
  in
  handle_error show_kind f;
  [%expect {| (Syntax.Ksing (Syntax.Cvar (1, (Some 2)))) |}]

let%expect_test "infer_constructor empty (Clam (Ktype, Cstring))" =
  let f () = infer_constructor empty (Clam (Ktype, Cstring)) in
  handle_error show_kind f;
  [%expect {| (Syntax.Kpi (Syntax.Ktype, (Syntax.Ksing Syntax.Cstring))) |}]

let%expect_test "infer_constructor empty (Clam (Ktype, Clam (Ktype, Cvar (0, None))))" =
  let f () = infer_constructor empty (Clam (Ktype, Clam (Ktype, Cvar (0, None)))) in
  handle_error show_kind f;
  [%expect {|
    (Syntax.Kpi (Syntax.Ktype,
       (Syntax.Kpi (Syntax.Ktype, (Syntax.Ksing (Syntax.Cvar (0, None))))))) |}]

let%expect_test "infer_constructor empty (Capp (Clam (Ktype, Cvar 0), Cint))" =
  let f () = infer_constructor empty (Capp (Clam (Ktype, Cvar (0, None)), Cint)) in
  handle_error show_kind f;
  [%expect {| (Syntax.Ksing Syntax.Cint) |}]

let%expect_test "infer_constructor empty (Cpair (Cbool, Cvar (0, None)))" =
  let f () = infer_constructor empty (Cpair (Cbool, Cvar (0, None))) in
  handle_error show_kind f;
  [%expect {| Uncaught exception: Type_error. |}]

let%expect_test "infer_constructor {0 : Ktype} (Cpair (Cbool, Cvar (0, None)))" =
  let f () = infer_constructor (extend_kind empty Ktype) (Cpair (Cbool, Cvar (0, None))) in
  handle_error show_kind f;
  [%expect {|
    (Syntax.Ksigma ((Syntax.Ksing Syntax.Cbool),
       (Syntax.Ksing (Syntax.Cvar (1, None))))) |}]

let%expect_test "infer_constructor {0 : Ktype} (Cpi1 (Cpair (Cbool, Cvar 0)))" =
  let f () =
    infer_constructor
      (extend_kind empty Ktype)
      (Cpi1 (Cpair (Cbool, Cvar (0, None))))
  in
  handle_error show_kind f;
  [%expect {| (Syntax.Ksing Syntax.Cbool) |}]

let%expect_test "infer_constructor {0 : Ktype} (Cpi2 (Cpair (Cbool, Cvar 0)))" =
  let f () =
    infer_constructor
      (extend_kind empty Ktype)
      (Cpi2 (Cpair (Cbool, Cvar (0, None))))
  in
  handle_error show_kind f;
  [%expect {| (Syntax.Ksing (Syntax.Cvar (0, None))) |}]

let%expect_test "infer_constructor empty (Carrow (Cint, Cstring))" =
  let f () = infer_constructor empty (Carrow (Cint, Cstring)) in
  handle_error show_kind f;
  [%expect {| (Syntax.Ksing (Syntax.Carrow (Syntax.Cint, Syntax.Cstring))) |}]

let%expect_test "infer_constructor {0 : Ktype} (Carrow (Cvar 0, Cstring))" =
  let f () = infer_constructor (extend_kind empty Ktype) (Carrow (Cvar (0, None), Cstring)) in
  handle_error show_kind f;
  [%expect {| (Syntax.Ksing (Syntax.Carrow ((Syntax.Cvar (0, None)), Syntax.Cstring))) |}]

let%expect_test "infer_constructor {0 : Ksing Cint} (Carrow (Cvar 0, Cstring))" =
  let f () = infer_constructor (extend_kind empty (Ksing Cint)) (Carrow (Cvar (0, None), Cstring)) in
  handle_error show_kind f;
  [%expect {| (Syntax.Ksing (Syntax.Carrow ((Syntax.Cvar (0, None)), Syntax.Cstring))) |}]

let%expect_test "infer_constructor empty (Cforall (Ktype, Carrow (Cvar 0, Cvar 0)))" =
  let f () = infer_constructor empty (Cforall (Ktype, Carrow (Cvar (0, None), Cvar (0, None)))) in
  handle_error show_kind f;
  [%expect {|
    (Syntax.Ksing
       (Syntax.Cforall (Syntax.Ktype,
          (Syntax.Carrow ((Syntax.Cvar (0, None)), (Syntax.Cvar (0, None))))))) |}]

let%expect_test "infer_constructor empty (Cexists (Ktype, Cvar 0))" =
  let f () = infer_constructor empty (Cexists (Ktype, Cvar (0, None))) in
  handle_error show_kind f;
  [%expect {| (Syntax.Ksing (Syntax.Cexists (Syntax.Ktype, (Syntax.Cvar (0, None))))) |}]

let%expect_test "infer_constructor empty (Cexists (Ksing Cexn, Cvar 0))" =
  let f () = infer_constructor empty (Cexists (Ksing Cexn, Cvar (0, None))) in
  handle_error show_kind f;
  [%expect {|
    (Syntax.Ksing
       (Syntax.Cexists ((Syntax.Ksing Syntax.Cexn), (Syntax.Cvar (0, None))))) |}]

let%expect_test "infer_constructor empty Csum ...." =
  let f () = infer_constructor empty (Csum [Cexn; Cbool; Cint; Cchar; Cstring]) in
  handle_error show_kind f;
  [%expect {|
    (Syntax.Ksing
       (Syntax.Csum
          [Syntax.Cexn; Syntax.Cbool; Syntax.Cint; Syntax.Cchar; Syntax.Cstring])) |}]

let%expect_test "infer_constructor empty Cprod ...." =
  let f () = infer_constructor empty (Cprod [Cexn; Cbool; Cint; Cchar; Cstring]) in
  handle_error show_kind f;
  [%expect {|
    (Syntax.Ksing
       (Syntax.Cprod
          [Syntax.Cexn; Syntax.Cbool; Syntax.Cint; Syntax.Cchar; Syntax.Cstring])) |}]

let%expect_test "infer_constructor empty (Crec (Cvar 0))" =
  let f () = infer_constructor empty (Crec (Cvar (0, None))) in
  handle_error show_kind f;
  [%expect {| (Syntax.Ksing (Syntax.Crec (Syntax.Cvar (0, None)))) |}]

let%expect_test "infer_constructor empty ('a list)" =
  let f () =
    infer_constructor empty
      (Cforall (Ktype,
        Crec (
          Csum [
            Cprod [];
            Cprod [Cvar (1, None); Cvar (0, None)]])))
  in
  handle_error show_kind f;
  [%expect {|
    (Syntax.Ksing
       (Syntax.Cforall (Syntax.Ktype,
          (Syntax.Crec
             (Syntax.Csum
                [(Syntax.Cprod []);
                  (Syntax.Cprod
                     [(Syntax.Cvar (1, None)); (Syntax.Cvar (0, None))])
                  ]))
          ))) |}]

(* infer_term *)

let%expect_test "infer_term {'0' : Cint} (Tvar '0')" =
  let f () = infer_term (extend_type empty 0 Cint) (Tvar 0) in
  handle_error show_constructor f;
  [%expect {| Syntax.Cint |}]

let%expect_test "infer_term {0 : Ktype ; '9' : Cvar 0} (Tvar '9')" =
  let f () = infer_term (extend_type (extend_kind empty Ktype) 9 (Cvar (0, None))) (Tvar 9) in
  handle_error show_constructor f;
  [%expect {| (Syntax.Cvar (0, (Some 1))) |}]

let%expect_test "infer_term {} (Tlam ('0', Cint, Tbool true))" =
  let f () = infer_term empty (Tlam (0, Cint, Tbool true)) in
  handle_error show_constructor f;
  [%expect {| (Syntax.Carrow (Syntax.Cint, Syntax.Cbool)) |}]

let%expect_test "infer_term {} (Tplam (Ktype, Tlam ('0', Cvar 0, Tvar '0')))" =
  let f () = infer_term empty (Tplam (Ktype, Tlam (0, Cvar (0, None), Tvar 0))) in
  handle_error show_constructor f;
  [%expect {|
    (Syntax.Cforall (Syntax.Ktype,
       (Syntax.Carrow ((Syntax.Cvar (0, None)), (Syntax.Cvar (0, (Some 1))))))) |}]

let%expect_test "infer_term {} id[int](3)" =
  let f () =
    let id = Tplam (Ktype, Tlam (0, Cvar (0, None), Tvar 0)) in
    infer_term empty (Tapp (Tpapp (id, Cint), Tint 3)) 
  in
  handle_error show_constructor f;
  [%expect {| Syntax.Cint |}]

let%expect_test "infer_term {} id[bool](3)" =
  let f () =
    let id = Tplam (Ktype, Tlam (0, Cvar (0, None), Tvar 0)) in
    infer_term empty (Tapp (Tpapp (id, Cbool), Tint 3)) 
  in
  handle_error show_constructor f;
  [%expect {| Uncaught exception: Type_error. |}]

let%expect_test "infer_term {} (Tpack (Cint, 3, Cexists (Ktype, Cvar 0)))" =
  let f () = infer_term empty (Tpack (Cint, Tint 3, Cexists (Ktype, Cvar (0, None)))) in
  handle_error show_constructor f;
  [%expect {| (Syntax.Cexists (Syntax.Ktype, (Syntax.Cvar (0, None)))) |}]

let%expect_test "infer_term {} (Tpack (Cbool, 3, Cexists (Ktype, Cvar 0)))" =
  let f () = infer_term empty (Tpack (Cbool, Tint 3, Cexists (Ktype, Cvar (0, None)))) in
  handle_error show_constructor f;
  [%expect {| Uncaught exception: Type_error. |}]

let%expect_test "infer_term {} (Tunpack ('0', Tpack (Cint, 3, Cexists (Ktype, Cvar 0)), Tvar '0'))" =
  let f () = infer_term empty (Tunpack (0, Tpack (Cint, Tint 3, Cexists (Ktype, Cvar (0, None))), Tvar 0)) in
  handle_error show_constructor f;
  [%expect {| Uncaught exception: Type_error. |}]

let%expect_test "infer_term {} (Tunpack '0', Tpack (string2int*string, .., ..), Capp (Tproj 0 '0', Tproj 1 '0'))" =
  let f () =
    let pkg =
      Tpack (Cstring,
        Ttuple [Tlam (1, Cstring, Tint 0); Tstring ""],
          Cexists (Ktype, Cprod [Carrow (Cvar (0, None), Cint); Cvar (0, None)]))
    in
    let term = Tunpack (2, pkg, Tapp (Tproj (Tvar 2, 0), Tproj (Tvar 2, 1))) in
    infer_term empty term
  in
  handle_error show_constructor f;
  [%expect {| Syntax.Cint |}]

let%expect_test "infer_term {} (Ttuple [])" =
  let f () = infer_term empty (Ttuple []) in
  handle_error show_constructor f;
  [%expect {| (Syntax.Cprod []) |}]

let%expect_test "infer_term {} (Tproj 0 (Ttuple []))" =
  let f () = infer_term empty (Tproj (Ttuple [], 0)) in
  handle_error show_constructor f;
  [%expect {| Uncaught exception: Type_error. |}]

let%expect_test "infer_term {} (Ttuple [Tint 0; Tchar 'c'; Tstring \"s\"; Tbool false])" =
  let f () = infer_term empty (Ttuple [Tint 0; Tchar 'c'; Tstring "s"; Tbool false]) in
  handle_error show_constructor f;
  [%expect {| (Syntax.Cprod [Syntax.Cint; Syntax.Cchar; Syntax.Cstring; Syntax.Cbool]) |}]

let%expect_test "infer_term {} Tproj -1 (Ttuple [Tint 0; Tchar 'c'; Tstring \"s\"; Tbool false])" =
  let f () = infer_term empty (Tproj (Ttuple [Tint 0; Tchar 'c'; Tstring "s"; Tbool false], -1)) in
  handle_error show_constructor f;
  [%expect {| Uncaught exception: Type_error. |}]

let%expect_test "infer_term {} Tproj 0 (Ttuple [Tint 0; Tchar 'c'; Tstring \"s\"; Tbool false])" =
  let f () = infer_term empty (Tproj (Ttuple [Tint 0; Tchar 'c'; Tstring "s"; Tbool false], 0)) in
  handle_error show_constructor f;
  [%expect {| Syntax.Cint |}]

let%expect_test "infer_term {} Tproj 1 (Ttuple [Tint 0; Tchar 'c'; Tstring \"s\"; Tbool false])" =
  let f () = infer_term empty (Tproj (Ttuple [Tint 0; Tchar 'c'; Tstring "s"; Tbool false], 1)) in
  handle_error show_constructor f;
  [%expect {| Syntax.Cchar |}]

let%expect_test "infer_term {} Tproj 2 (Ttuple [Tint 0; Tchar 'c'; Tstring \"s\"; Tbool false])" =
  let f () = infer_term empty (Tproj (Ttuple [Tint 0; Tchar 'c'; Tstring "s"; Tbool false], 2)) in
  handle_error show_constructor f;
  [%expect {| Syntax.Cstring |}]

let%expect_test "infer_term {} Tproj 3 (Ttuple [Tint 0; Tchar 'c'; Tstring \"s\"; Tbool false])" =
  let f () = infer_term empty (Tproj (Ttuple [Tint 0; Tchar 'c'; Tstring "s"; Tbool false], 3)) in
  handle_error show_constructor f;
  [%expect {| Syntax.Cbool |}]

let%expect_test "infer_term {} Tproj 4 (Ttuple [Tint 0; Tchar 'c'; Tstring \"s\"; Tbool false])" =
  let f () = infer_term empty (Tproj (Ttuple [Tint 0; Tchar 'c'; Tstring "s"; Tbool false], 4)) in
  handle_error show_constructor f;
  [%expect {| Uncaught exception: Type_error. |}]

let%expect_test "infer_term {} (nil : int list)" =
  let f () = 
    let sum_t = Csum [Cprod []; Cprod [Cint; Cvar (0, None)]] in
    let list_t = Crec sum_t in
    let nil = Troll (Tinj (Ttuple [], 0, subst_constructor list_t sum_t), list_t) in
    infer_term empty nil
  in
  handle_error show_constructor f;
  [%expect {|
    (Syntax.Crec
       (Syntax.Csum
          [(Syntax.Cprod []);
            (Syntax.Cprod [Syntax.Cint; (Syntax.Cvar (0, None))])])) |}]

let%expect_test "infer_term {} (cons 0 nil : int list)" =
  let f () =
    let sum_t = Csum [Cprod []; Cprod [Cint; Cvar (0, None)]] in
    let list_t = Crec sum_t in
    let nil = Troll (Tinj (Ttuple [], 0, subst_constructor list_t sum_t), list_t) in
    let cons x xs = Troll (Tinj (Ttuple [x; xs], 1, subst_constructor list_t sum_t), list_t) in
    infer_term empty (cons (Tint 0) nil)
  in
  handle_error show_constructor f;
  [%expect {|
    (Syntax.Crec
       (Syntax.Csum
          [(Syntax.Cprod []);
            (Syntax.Cprod [Syntax.Cint; (Syntax.Cvar (0, None))])])) |}] 

let%expect_test "infer_term {} (cons 0 (cons 1 (cons 2 (cons 3 nil)))) : int list" =
  let f () =
    let sum_t = Csum [Cprod []; Cprod [Cint; Cvar (0, None)]] in
    let list_t = Crec sum_t in
    let nil = Troll (Tinj (Ttuple [], 0, subst_constructor list_t sum_t), list_t) in
    let cons x xs = Troll (Tinj (Ttuple [x; xs], 1, subst_constructor list_t sum_t), list_t) in
    infer_term empty (cons (Tint 0) (cons (Tint 1) (cons (Tint 2) (cons (Tint 3) nil))))
  in
  handle_error show_constructor f;
  [%expect {|
    (Syntax.Crec
       (Syntax.Csum
          [(Syntax.Cprod []);
            (Syntax.Cprod [Syntax.Cint; (Syntax.Cvar (0, None))])])) |}]

let%expect_test "infer_term {} List.hd : int list -> int" =
  let f () =
    let sum_t = Csum [Cprod []; Cprod [Cint; Cvar (0, None)]] in
    let list_t = Crec sum_t in
    let raise_t t = Traise (Ttag (Tnewtag (Cprod []), Ttuple []), t) in
    infer_term
      empty
      (Tlam (0, list_t, Tcase (Tunroll (Tvar 0), [(0, raise_t Cint); (1, (Tproj (Tvar 1, 0)))])))
  in
  handle_error show_constructor f;
  [%expect {|
    (Syntax.Carrow (
       (Syntax.Crec
          (Syntax.Csum
             [(Syntax.Cprod []);
               (Syntax.Cprod [Syntax.Cint; (Syntax.Cvar (0, None))])])),
       Syntax.Cint)) |}]

let%expect_test "infer_term {} List.tl : int list -> int list" =
  let f () =
    let sum_t = Csum [Cprod []; Cprod [Cint; Cvar (0, None)]] in
    let list_t = Crec sum_t in
    let raise_t t = Traise (Ttag (Tnewtag (Cprod []), Ttuple []), t) in
    infer_term
      empty
      (Tlam (0, list_t, Tcase (Tunroll (Tvar 0), [(0, raise_t list_t); (1, Tproj (Tvar 1, 1))])))
  in
  handle_error show_constructor f;
  [%expect {|
    (Syntax.Carrow (
       (Syntax.Crec
          (Syntax.Csum
             [(Syntax.Cprod []);
               (Syntax.Cprod [Syntax.Cint; (Syntax.Cvar (0, None))])])),
       (Syntax.Crec
          (Syntax.Csum
             [(Syntax.Cprod []);
               (Syntax.Cprod [Syntax.Cint; (Syntax.Cvar (0, None))])]))
       )) |}]

let%expect_test "infer_term {} nil : 'a list" =
  let f () =
    let sum_t = Csum [Cprod []; Cprod [Cvar (1, None); Cvar (0, None)]] in
    let list_t = Crec sum_t in
    let nil = Tplam (Ktype, Troll (Tinj (Ttuple [], 0, subst_constructor list_t sum_t), list_t)) in
    infer_term empty nil
  in
  handle_error show_constructor f;
  [%expect {|
    (Syntax.Cforall (Syntax.Ktype,
       (Syntax.Crec
          (Syntax.Csum
             [(Syntax.Cprod []);
               (Syntax.Cprod [(Syntax.Cvar (1, None)); (Syntax.Cvar (0, None))])]))
       )) |}]

let%expect_test "infer_term {} cons : 'a -> 'a list -> 'a list" =
  let f () =
    let sum_t = Csum [Cprod []; Cprod [Cvar (1, None); Cvar (0, None)]] in
    let list_t = Crec sum_t in
    let cons =
      Tplam (Ktype,
        Tlam (0, Cvar (0, None),
          Tlam (1, list_t,
            Troll (Tinj (Ttuple [Tvar 0; Tvar 1], 1, subst_constructor list_t sum_t), list_t))))
    in
    infer_term empty cons
  in
  handle_error show_constructor f;
  [%expect {|
    (Syntax.Cforall (Syntax.Ktype,
       (Syntax.Carrow ((Syntax.Cvar (0, None)),
          (Syntax.Carrow (
             (Syntax.Crec
                (Syntax.Csum
                   [(Syntax.Cprod []);
                     (Syntax.Cprod
                        [(Syntax.Cvar (1, None)); (Syntax.Cvar (0, None))])
                     ])),
             (Syntax.Crec
                (Syntax.Csum
                   [(Syntax.Cprod []);
                     (Syntax.Cprod
                        [(Syntax.Cvar (1, None)); (Syntax.Cvar (0, None))])
                     ]))
             ))
          ))
       )) |}]

let%expect_test "infer_term {} List.hd : 'a list -> 'a" =
  let f () =
    let sum_t = Csum [Cprod []; Cprod [Cvar (1, None); Cvar (0, None)]] in
    let list_t = Crec sum_t in
    let raise_t t = Traise (Ttag (Tnewtag (Cprod []), Ttuple []), t) in
    let hd = 
      Tplam (Ktype,
        Tlam (0, list_t,
          Tcase (Tunroll (Tvar 0), [
            (0, raise_t (Cvar (0, None)));
            (1, (Tproj (Tvar 1, 0)))
      ])))
    in
    infer_term empty hd
  in
  handle_error show_constructor f;
  [%expect {|
    (Syntax.Cforall (Syntax.Ktype,
       (Syntax.Carrow (
          (Syntax.Crec
             (Syntax.Csum
                [(Syntax.Cprod []);
                  (Syntax.Cprod
                     [(Syntax.Cvar (1, None)); (Syntax.Cvar (0, None))])
                  ])),
          (Syntax.Cvar (0, None))))
       )) |}]

let%expect_test "infer_term {} List.tl : 'a list -> 'a list" =
  let f () =
    let sum_t = Csum [Cprod []; Cprod [Cvar (1, None); Cvar (0, None)]] in
    let list_t = Crec sum_t in
    let raise_t t = Traise (Ttag (Tnewtag (Cprod []), Ttuple []), t) in
    let tl = 
      Tplam (Ktype,
        Tlam (0, list_t,
          Tcase (Tunroll (Tvar 0), [
            (0, raise_t list_t);
            (1, (Tproj (Tvar 1, 1)))
      ])))
    in
    infer_term empty tl
  in
  handle_error show_constructor f;
  [%expect {|
    (Syntax.Cforall (Syntax.Ktype,
       (Syntax.Carrow (
          (Syntax.Crec
             (Syntax.Csum
                [(Syntax.Cprod []);
                  (Syntax.Cprod
                     [(Syntax.Cvar (1, None)); (Syntax.Cvar (0, None))])
                  ])),
          (Syntax.Crec
             (Syntax.Csum
                [(Syntax.Cprod []);
                  (Syntax.Cprod
                     [(Syntax.Cvar (1, None)); (Syntax.Cvar (0, None))])
                  ]))
          ))
       )) |}]

let%expect_test "infer_term {} (Tnewtag unit)" = 
  let f () = infer_term empty (Tnewtag (Cprod [])) in
  handle_error show_constructor f;
  [%expect {| (Syntax.Ctag (Syntax.Cprod [])) |}]

let%expect_test "infer_term {} (Tnewtag string)" =
  let f () = infer_term empty (Tnewtag Cstring) in
  handle_error show_constructor f;
  [%expect {| (Syntax.Ctag Syntax.Cstring) |}]

let%expect_test "infer_term {} (Ttag (unit tag, ()))" =
  let f () = infer_term empty (Ttag (Tnewtag (Cprod []), Ttuple [])) in
  handle_error show_constructor f;
  [%expect {| Syntax.Cexn |}]

let%expect_test "infer_term {} (Ttag (string tag, str))" =
  let f () = infer_term empty (Ttag (Tnewtag Cstring, Tstring "hi")) in
  handle_error show_constructor f;
  [%expect {| Syntax.Cexn |}]

let%expect_test "infer_term {} iftag matches" =
  let f () =
    let tag = Tnewtag (Cprod []) in
    let exn = Ttag (tag, Ttuple []) in
    let e = Tiftag (tag, exn, 0, Tvar 0, Ttuple []) in
    infer_term empty e
  in
  handle_error show_constructor f;
  [%expect {| (Syntax.Cprod []) |}]

let%expect_test "infer_term {} iftag no match" =
  let f () =
    let tag = Tnewtag Cstring in
    let exn = Ttag (Tnewtag Cint, Tint 0) in
    let e = Tiftag (tag, exn, 0, Tvar 0, Tstring "") in
    infer_term empty e
  in
  handle_error show_constructor f;
  [%expect {| Syntax.Cstring |}]

let%expect_test "infer_term {} raise" =
  let f () = infer_term empty (Tplam (Ktype, Traise (Ttag (Tnewtag (Cprod []), Ttuple []), Cvar (0, None)))) in
  handle_error show_constructor f;
  [%expect {| (Syntax.Cforall (Syntax.Ktype, (Syntax.Cvar (0, None)))) |}]

let%expect_test "infer_term {} handle" =
  let f () =
    let exn = Ttag (Tnewtag Cstring, Tstring "yep") in
    infer_term empty (Thandle (exn, 0, Tvar 0))
  in
  handle_error show_constructor f;
  [%expect {| Syntax.Cexn |}]

let%expect_test "infer_term {} handle more" =
  let f () = infer_term empty (Thandle (Tstring "yep", 0, Tstring "yep")) in
  handle_error show_constructor f;
  [%expect {| Syntax.Cstring |}]

let%expect_test "infer_term {} ref : 'a -> 'a ref" =
  let f () = infer_term empty (Tplam (Ktype, Tlam (0, Cvar (0, None), Tref (Tvar 0)))) in
  handle_error show_constructor f;
  [%expect {|
    (Syntax.Cforall (Syntax.Ktype,
       (Syntax.Carrow ((Syntax.Cvar (0, None)),
          (Syntax.Cref (Syntax.Cvar (0, (Some 1))))))
       )) |}]

let%expect_test "infer_term {} deref : 'a ref -> 'a" =
  let f () = infer_term empty (Tplam (Ktype, Tlam (0, Cref (Cvar (0, None)), Tderef (Tvar 0)))) in
  handle_error show_constructor f;
  [%expect {|
    (Syntax.Cforall (Syntax.Ktype,
       (Syntax.Carrow ((Syntax.Cref (Syntax.Cvar (0, None))),
          (Syntax.Cvar (0, (Some 1)))))
       )) |}]

let%expect_test "infer_term {} (:=) : 'a ref -> 'a -> unit" =
  let f () =
    let e =
      Tplam (Ktype,
        Tlam (0, Cref (Cvar (0, None)),
          Tlam (1, Cvar (0, None),
            Tassign (Tvar 0, Tvar 1))))
    in
    infer_term empty e
  in
  handle_error show_constructor f;
  [%expect {|
    (Syntax.Cforall (Syntax.Ktype,
       (Syntax.Carrow ((Syntax.Cref (Syntax.Cvar (0, None))),
          (Syntax.Carrow ((Syntax.Cvar (0, None)), (Syntax.Cprod [])))))
       )) |}]
