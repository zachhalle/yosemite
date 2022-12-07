open Cps
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

let%expect_test "lift_kind 10 (Kpi (Ksing (Cvar 0), Ksing (Cvar 0)))" =
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

let%expect_test "lift_kind 10 (Kpi (Kunit, Kpi (Kunit, Kpi (Kunit, Ksing (Cvar 3)))))" =
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

let%expect_test "lift_constructor 10 (Cexists (Cexists (Cexists (Cvar 0))))" =
  let result = lift_constructor 10 (Cexists (Ktype, Cexists (Ktype, (Cexists (Ktype, Cvar (0, None)))))) in
  printf "%s\n" (show_constructor result);
  [%expect {|
    (Syntax.Cexists (Syntax.Ktype,
       (Syntax.Cexists (Syntax.Ktype,
          (Syntax.Cexists (Syntax.Ktype, (Syntax.Cvar (0, None))))))
       )) |}]

let%expect_test "lift_constructor 10 (Cexists (Cexists (Cexists (Cvar 2))))" =
  let result = lift_constructor 10 (Cexists (Ktype, Cexists (Ktype, (Cexists (Ktype, Cvar (2, None)))))) in
  printf "%s\n" (show_constructor result);
  [%expect {|
    (Syntax.Cexists (Syntax.Ktype,
       (Syntax.Cexists (Syntax.Ktype,
          (Syntax.Cexists (Syntax.Ktype, (Syntax.Cvar (2, None))))))
       )) |}]

let%expect_test "lift_constructor 10 (Cexists (Cexists (Cexists (Cvar 3))))" =
  let result = lift_constructor 10 (Cexists (Ktype, Cexists (Ktype, (Cexists (Ktype, Cvar (3, None)))))) in
  printf "%s\n" (show_constructor result);
  [%expect {|
    (Syntax.Cexists (Syntax.Ktype,
       (Syntax.Cexists (Syntax.Ktype,
          (Syntax.Cexists (Syntax.Ktype, (Syntax.Cvar (13, None))))))
       )) |}]

(* lift_expr *)

let expr_of v = Elet (0, v, Ehalt)

let%expect_test "lift_expr 10 Eunpack (0, Vvar 1, Eunpack (2, Vvar 3, Eunpack (4, Vvar 5, Vroll (Ttuple [], Cvar (0, None)))))" =
  let result = lift_expr 10 (
    Eunpack (0, Vvar 1, Eunpack (2, Vvar 3, Eunpack (4, Vvar 5, expr_of (Vroll (Vtuple [], Cvar (0, None))))))
  ) in
  printf "%s\n" (show_expr result);
  [%expect {|
    (Syntax.Eunpack (0, (Syntax.Vvar 1),
       (Syntax.Eunpack (2, (Syntax.Vvar 3),
          (Syntax.Eunpack (4, (Syntax.Vvar 5),
             (Syntax.Elet (0,
                (Syntax.Vroll ((Syntax.Vtuple []), (Syntax.Cvar (0, None)))),
                Syntax.Ehalt))
             ))
          ))
       )) |}]

let%expect_test "lift_expr 10 Eunpack (0, Vvar 1, Eunpack (2, Vvar 3, Eunpack (4, Vvar 5, Vroll (Ttuple [], Cvar (2, None)))))" =
  let result = lift_expr 10 (
    Eunpack (0, Vvar 1, Eunpack (2, Vvar 3, Eunpack (4, Vvar 5, expr_of (Vroll (Vtuple [], Cvar (2, None))))))
  ) in
  printf "%s\n" (show_expr result);
  [%expect {|
    (Syntax.Eunpack (0, (Syntax.Vvar 1),
       (Syntax.Eunpack (2, (Syntax.Vvar 3),
          (Syntax.Eunpack (4, (Syntax.Vvar 5),
             (Syntax.Elet (0,
                (Syntax.Vroll ((Syntax.Vtuple []), (Syntax.Cvar (2, None)))),
                Syntax.Ehalt))
             ))
          ))
       )) |}]

let%expect_test "lift_expr 10 Eunpack (0, Vvar 1, Eunpack (2, Vvar 3, Eunpack (4, Vvar 5, Vroll (Ttuple [], Cvar (3, None)))))" =
  let result = lift_expr 10 (
    Eunpack (0, Vvar 1, Eunpack (2, Vvar 3, Eunpack (4, Vvar 5, expr_of (Vroll (Vtuple [], Cvar (3, None))))))
  ) in
  printf "%s\n" (show_expr result);
  [%expect {|
    (Syntax.Eunpack (0, (Syntax.Vvar 1),
       (Syntax.Eunpack (2, (Syntax.Vvar 3),
          (Syntax.Eunpack (4, (Syntax.Vvar 5),
             (Syntax.Elet (0,
                (Syntax.Vroll ((Syntax.Vtuple []), (Syntax.Cvar (13, None)))),
                Syntax.Ehalt))
             ))
          ))
       )) |}]


(* subst_kind *)

 let%expect_test "subst_kind Cunit Ktype" =
   let result = subst_kind Cunit Ktype in
   printf "%s\n" (show_kind result);
   [%expect {| Syntax.Ktype |}]

let%expect_test "subst_kind Cunit (Ksing (Cvar 0))" =
  let result = subst_kind Cunit (Ksing (Cvar (0, None))) in
  printf "%s\n" (show_kind result);
  [%expect {| (Syntax.Ksing Syntax.Cunit) |}]

let%expect_test "subst_kind Cunit (Ksing (Cvar 1))" =
  let result = subst_kind Cunit (Ksing (Cvar (1, None))) in
  printf "%s\n" (show_kind result);
  [%expect {| (Syntax.Ksing (Syntax.Cvar (0, None))) |}]

let%expect_test "subst_kind Cunit (Kpi (Ksing (Cvar 0), Ksing (Cvar 0)))" =
  let result = subst_kind Cunit (Kpi (Ksing (Cvar (0, None)), Ksing (Cvar (0, None)))) in
  printf "%s\n" (show_kind result);
  [%expect {|
    (Syntax.Kpi ((Syntax.Ksing Syntax.Cunit),
       (Syntax.Ksing (Syntax.Cvar (0, None))))) |}]

let%expect_test "subst_kind Cunit (Kpi (Ksing (Cvar 0), Ksing (Cvar 1)))" =
  let result = subst_kind Cunit (Kpi (Ksing (Cvar (0, None)), Ksing (Cvar (1, None)))) in
  printf "%s\n" (show_kind result);
  [%expect {| (Syntax.Kpi ((Syntax.Ksing Syntax.Cunit), (Syntax.Ksing Syntax.Cunit))) |}]

let%expect_test "subst_kind Cunit (Kpi (Ksing (Cvar 1), Ksing (Cvar 1)))" =
  let result = subst_kind Cunit (Kpi (Ksing (Cvar (1, None)), Ksing (Cvar (1, None)))) in
  printf "%s\n" (show_kind result);
  [%expect {|
    (Syntax.Kpi ((Syntax.Ksing (Syntax.Cvar (0, None))),
       (Syntax.Ksing Syntax.Cunit))) |}]

let%expect_test "subst_kind Cunit (Kpi (Ksing (Cvar 0), Ksing (Cvar 2)))" =
  let result = subst_kind Cunit (Kpi (Ksing (Cvar (1, None)), Ksing (Cvar (2, None)))) in
  printf "%s\n" (show_kind result);
  [%expect {|
    (Syntax.Kpi ((Syntax.Ksing (Syntax.Cvar (0, None))),
       (Syntax.Ksing (Syntax.Cvar (1, None))))) |}]

(* subst_constructor *)

let%expect_test "subst_con Cunit Cunit" =
  let result = subst_constructor Cunit Cunit in
  printf "%s\n" (show_constructor result);
  [%expect {| Syntax.Cunit |}]

let%expect_test "subst_con (Cvar 0) Cunit" =
  let result = subst_constructor (Cvar (0, None)) Cunit in
  printf "%s\n" (show_constructor result);
  [%expect {| Syntax.Cunit |}]

let%expect_test "subst_con Cunit (Cvar 0)" =
  let result = subst_constructor Cunit (Cvar (0, None)) in
  printf "%s\n" (show_constructor result);
  [%expect {| Syntax.Cunit |}]

let%expect_test "subst_con Cunit (Cvar 1)" =
  let result = subst_constructor Cunit (Cvar (1, None)) in
  printf "%s\n" (show_constructor result);
  [%expect {| (Syntax.Cvar (0, None)) |}]

let%expect_test "subst_con (Cvar 0) (Cvar 0)" =
  let result = subst_constructor (Cvar (0, None)) (Cvar (0, None)) in
  printf "%s\n" (show_constructor result);
  [%expect {| (Syntax.Cvar (0, None)) |}]

let%expect_test "subst_con (Cvar 1) (Cvar 0)" =
  let result = subst_constructor (Cvar (1, None)) (Cvar (0, None)) in
  printf "%s\n" (show_constructor result);
  [%expect {| (Syntax.Cvar (1, None)) |}]

let%expect_test "subst_con (Cvar 0) (Cvar 1)" =
  let result = subst_constructor (Cvar (0, None)) (Cvar (1, None)) in
  printf "%s\n" (show_constructor result);
  [%expect {| (Syntax.Cvar (0, None)) |}]

let%expect_test "subst_con Cunit (Crec (Crec (Crec (Cvar 0))))" =
  let result = subst_constructor Cunit (Crec (Crec (Crec (Cvar (0, None))))) in
  printf "%s\n" (show_constructor result);
  [%expect {| (Syntax.Crec (Syntax.Crec (Syntax.Crec (Syntax.Cvar (0, None))))) |}]

let%expect_test "subst_con Cunit (Crec (Crec (Crec (Cvar 0))))" =
  let result = subst_constructor Cunit (Crec (Crec (Crec (Cvar (2, None))))) in
  printf "%s\n" (show_constructor result);
  [%expect {| (Syntax.Crec (Syntax.Crec (Syntax.Crec (Syntax.Cvar (2, None))))) |}]

let%expect_test "subst_con Cunit (Crec (Crec (Crec (Cvar 0))))" =
  let result = subst_constructor Cunit (Crec (Crec (Crec (Cvar (3, None))))) in
  printf "%s\n" (show_constructor result);
  [%expect {| (Syntax.Crec (Syntax.Crec (Syntax.Crec Syntax.Cunit))) |}]
 
let%expect_test "subst_con Cunit (Crec (Crec (Crec (Cvar 0))))" =
  let result = subst_constructor Cunit (Crec (Crec (Crec (Cvar (4, None))))) in
  printf "%s\n" (show_constructor result);
  [%expect {| (Syntax.Crec (Syntax.Crec (Syntax.Crec (Syntax.Cvar (3, None))))) |}]

let%expect_test "subst_con Cexn (Cexists (Cexists (Cexists (Cvar 0))))" =
  let result = subst_constructor Cexn (Cexists (Ktype, Cexists (Ktype, Cexists (Ktype, Cvar (0, None))))) in
  printf "%s\n" (show_constructor result);
  [%expect {|
    (Syntax.Cexists (Syntax.Ktype,
       (Syntax.Cexists (Syntax.Ktype,
          (Syntax.Cexists (Syntax.Ktype, (Syntax.Cvar (0, None))))))
       )) |}]

let%expect_test "subst_con Cexn (Cexists (Cexists (Cexists (Cvar 2))))" =
  let result = subst_constructor Cexn (Cexists (Ktype, Cexists (Ktype, Cexists (Ktype, Cvar (2, None))))) in
  printf "%s\n" (show_constructor result);
  [%expect {|
    (Syntax.Cexists (Syntax.Ktype,
       (Syntax.Cexists (Syntax.Ktype,
          (Syntax.Cexists (Syntax.Ktype, (Syntax.Cvar (2, None))))))
       )) |}]

let%expect_test "subst_con Cexn (Cexists (Cexists (Cexists (Cvar 3))))" =
  let result = subst_constructor Cexn (Cexists (Ktype, Cexists (Ktype, Cexists (Ktype, Cvar (3, None))))) in
  printf "%s\n" (show_constructor result);
  [%expect {|
    (Syntax.Cexists (Syntax.Ktype,
       (Syntax.Cexists (Syntax.Ktype,
          (Syntax.Cexists (Syntax.Ktype, Syntax.Cexn))))
       )) |}]

let%expect_test "subst_con Cexn (Cexists (Cexists (Cexists (Cvar 4))))" =
  let result = subst_constructor Cexn (Cexists (Ktype, Cexists (Ktype, Cexists (Ktype, Cvar (4, None))))) in
  printf "%s\n" (show_constructor result);
  [%expect {|
    (Syntax.Cexists (Syntax.Ktype,
       (Syntax.Cexists (Syntax.Ktype,
          (Syntax.Cexists (Syntax.Ktype, (Syntax.Cvar (3, None))))))
       )) |}]

(* subst_expr *)

let%expect_test "subst_expr Cexn (Eunpack (1, Vvar 0, Vtuple []))" =
  let result = subst_expr Cexn (Eunpack (1, Vvar 0, expr_of (Vtuple []))) in
  printf "%s\n" (show_expr result);
  [%expect {|
    (Syntax.Eunpack (1, (Syntax.Vvar 0),
       (Syntax.Elet (0, (Syntax.Vtuple []), Syntax.Ehalt)))) |}]

let%expect_test "subst_expr Cexn (Eunpack (0, Vvar 1, Vroll (Vtuple [], Cvar 0)))" =
  let result = subst_expr Cexn (Eunpack (0, Vvar 1, expr_of (Vroll (Vtuple [], Cvar (0, None))))) in
  printf "%s\n" (show_expr result);
  [%expect {|
    (Syntax.Eunpack (0, (Syntax.Vvar 1),
       (Syntax.Elet (0,
          (Syntax.Vroll ((Syntax.Vtuple []), (Syntax.Cvar (0, None)))),
          Syntax.Ehalt))
       )) |}]

let%expect_test "subst_expr Cexn (Eunpack (0, Vvar 1, Vroll (Vtuple [], Cvar 0)))" =
  let result = subst_expr Cexn (Eunpack (0, Vvar 1, expr_of (Vroll (Vtuple [], Cvar (1, None))))) in
  printf "%s\n" (show_expr result);
  [%expect {|
    (Syntax.Eunpack (0, (Syntax.Vvar 1),
       (Syntax.Elet (0, (Syntax.Vroll ((Syntax.Vtuple []), Syntax.Cexn)),
          Syntax.Ehalt))
       )) |}]

let%expect_test "subst_expr Cexn (Eunpack (0, Vvar 1, Eunpack (2, Vvar 3, Cvar 0)))" =
  let result = subst_expr Cexn (Eunpack (0, Vvar 1, Eunpack (2, Vvar 3, expr_of (Vroll (Vtuple [], Cvar (0, None))))))
  in
  printf "%s\n" (show_expr result);
  [%expect {|
    (Syntax.Eunpack (0, (Syntax.Vvar 1),
       (Syntax.Eunpack (2, (Syntax.Vvar 3),
          (Syntax.Elet (0,
             (Syntax.Vroll ((Syntax.Vtuple []), (Syntax.Cvar (0, None)))),
             Syntax.Ehalt))
          ))
       )) |}]

let%expect_test "subst_expr Cexn (Eunpack (0, Vvar 1, Eunpack (2, Vvar 3, Cvar 0)))" =
  let result = subst_expr Cexn (Eunpack (0, Vvar 1, Eunpack (2, Vvar 3, expr_of (Vroll (Vtuple [], Cvar (1, None))))))
  in
  printf "%s\n" (show_expr result);
  [%expect {|
    (Syntax.Eunpack (0, (Syntax.Vvar 1),
       (Syntax.Eunpack (2, (Syntax.Vvar 3),
          (Syntax.Elet (0,
             (Syntax.Vroll ((Syntax.Vtuple []), (Syntax.Cvar (1, None)))),
             Syntax.Ehalt))
          ))
       )) |}]

let%expect_test "subst_expr Cexn (Eunpack (0, Vvar 1, Eunpack (2, Vvar 3, Cvar 0)))" =
  let result = subst_expr Cexn (Eunpack (0, Vvar 1, Eunpack (2, Vvar 3, expr_of (Vroll (Vtuple [], Cvar (2, None))))))
  in
  printf "%s\n" (show_expr result);
  [%expect {|
    (Syntax.Eunpack (0, (Syntax.Vvar 1),
       (Syntax.Eunpack (2, (Syntax.Vvar 3),
          (Syntax.Elet (0, (Syntax.Vroll ((Syntax.Vtuple []), Syntax.Cexn)),
             Syntax.Ehalt))
          ))
       )) |}]


