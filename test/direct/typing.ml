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
