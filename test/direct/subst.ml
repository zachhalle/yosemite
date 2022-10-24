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

(* lift_term *)

let%expect_test "lift_term 10 (Tunpack (Tvar 0, Tvar 1, Tplam (Ktype, Tplam (Ktype, Tpapp (Tvar 2, Cvar 0)))))" =
  let result = lift_term 10 (
    Tunpack (0, Tvar 1, Tplam (Ktype, Tplam (Ktype, Tpapp (Tvar 2, Cvar (0, None)))))
  ) in
  printf "%s\n" (show_term result);
  [%expect {|
    (Syntax.Tunpack (0, (Syntax.Tvar 1),
       (Syntax.Tplam (Syntax.Ktype,
          (Syntax.Tplam (Syntax.Ktype,
             (Syntax.Tpapp ((Syntax.Tvar 2), (Syntax.Cvar (0, None))))))
          ))
       )) |}]

let%expect_test "lift_term 10 (Tunpack (Tvar 0, Tvar 1, Tplam (Ktype, Tplam (Ktype, Tpapp (Tvar 2, Cvar 2)))))" =
  let result = lift_term 10 (
    Tunpack (0, Tvar 1, Tplam (Ktype, Tplam (Ktype, Tpapp (Tvar 2, Cvar (2, None)))))
  ) in
  printf "%s\n" (show_term result);
  [%expect {|
    (Syntax.Tunpack (0, (Syntax.Tvar 1),
       (Syntax.Tplam (Syntax.Ktype,
          (Syntax.Tplam (Syntax.Ktype,
             (Syntax.Tpapp ((Syntax.Tvar 2), (Syntax.Cvar (2, None))))))
          ))
       )) |}]

 let%expect_test "lift_term 10 (Tunpack (Tvar 0, Tvar 1, Tplam (Ktype, Tplam (Ktype, Tpapp (Tvar 2, Cvar 3)))))" =
  let result = lift_term 10 (
    Tunpack (0, Tvar 1, Tplam (Ktype, Tplam (Ktype, Tpapp (Tvar 2, Cvar (3, None)))))
  ) in
  printf "%s\n" (show_term result);
   [%expect {|
     (Syntax.Tunpack (0, (Syntax.Tvar 1),
        (Syntax.Tplam (Syntax.Ktype,
           (Syntax.Tplam (Syntax.Ktype,
              (Syntax.Tpapp ((Syntax.Tvar 2), (Syntax.Cvar (13, None))))))
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

let%expect_test "subst_con Cexn (Cexists (Cforall (Cexists (Cvar 0))))" =
  let result = subst_constructor Cexn (Cexists (Ktype, Cforall (Ktype, Cexists (Ktype, Cvar (0, None))))) in
  printf "%s\n" (show_constructor result);
  [%expect {|
    (Syntax.Cexists (Syntax.Ktype,
       (Syntax.Cforall (Syntax.Ktype,
          (Syntax.Cexists (Syntax.Ktype, (Syntax.Cvar (0, None))))))
       )) |}]

let%expect_test "subst_con Cexn (Cexists (Cforall (Cexists (Cvar 2))))" =
  let result = subst_constructor Cexn (Cexists (Ktype, Cforall (Ktype, Cexists (Ktype, Cvar (2, None))))) in
  printf "%s\n" (show_constructor result);
  [%expect {|
    (Syntax.Cexists (Syntax.Ktype,
       (Syntax.Cforall (Syntax.Ktype,
          (Syntax.Cexists (Syntax.Ktype, (Syntax.Cvar (2, None))))))
       )) |}]

let%expect_test "subst_con Cexn (Cexists (Cforall (Cexists (Cvar 3))))" =
  let result = subst_constructor Cexn (Cexists (Ktype, Cforall (Ktype, Cexists (Ktype, Cvar (3, None))))) in
  printf "%s\n" (show_constructor result);
  [%expect {|
    (Syntax.Cexists (Syntax.Ktype,
       (Syntax.Cforall (Syntax.Ktype,
          (Syntax.Cexists (Syntax.Ktype, Syntax.Cexn))))
       )) |}]

let%expect_test "subst_con Cexn (Cexists (Cforall (Cexists (Cvar 4))))" =
  let result = subst_constructor Cexn (Cexists (Ktype, Cforall (Ktype, Cexists (Ktype, Cvar (4, None))))) in
  printf "%s\n" (show_constructor result);
  [%expect {|
    (Syntax.Cexists (Syntax.Ktype,
       (Syntax.Cforall (Syntax.Ktype,
          (Syntax.Cexists (Syntax.Ktype, (Syntax.Cvar (3, None))))))
       )) |}]

(* subst_term *)

let%expect_test "subst_term Cexn (Tunpack (1, Tvar 0, Ttuple []))" =
  let result = subst_term Cexn (Tunpack (1, Tvar 0, Ttuple [])) in
  printf "%s\n" (show_term result);
  [%expect {| (Syntax.Tunpack (1, (Syntax.Tvar 0), (Syntax.Ttuple []))) |}]

let%expect_test "subst_term Cexn (Tplam (Ksing (Cvar 0), Tvar 0))" =
  let result = subst_term Cexn (Tplam (Ksing (Cvar (0, None)), Tvar 0)) in
  printf "%s\n" (show_term result);
  [%expect {| (Syntax.Tplam ((Syntax.Ksing Syntax.Cexn), (Syntax.Tvar 0))) |}]

let%expect_test "subst_term Cexn (Tplam (Ksing (Cvar 0), Tvar 0))" =
  let result = subst_term Cexn (Tplam (Ksing (Cvar (1, None)), Tvar 0)) in
  printf "%s\n" (show_term result);
  [%expect {| (Syntax.Tplam ((Syntax.Ksing (Syntax.Cvar (0, None))), (Syntax.Tvar 0))) |}]

let%expect_test "subst_term Cexn (Tplam (Ksing (Cvar 0), Tpapp (Tvar 0, Cvar 0)))" =
  let result = subst_term Cexn (Tplam (Ksing (Cvar (0, None)), Tpapp (Tvar 0, Cvar (0, None)))) in
  printf "%s\n" (show_term result);
  [%expect {|
    (Syntax.Tplam ((Syntax.Ksing Syntax.Cexn),
       (Syntax.Tpapp ((Syntax.Tvar 0), (Syntax.Cvar (0, None)))))) |}]

let%expect_test "subst_term Cexn (Tunpack (0, Tvar 1, Tplam (Ktype, Tplam (Ktype, Tnewtag (Cvar 0)))))" =
  let result = subst_term Cexn (Tunpack (0, Tvar 1, Tplam (Ktype, Tplam (Ktype, Tnewtag (Cvar (0, None)))))) in
  printf "%s\n" (show_term result);
  [%expect {|
    (Syntax.Tunpack (0, (Syntax.Tvar 1),
       (Syntax.Tplam (Syntax.Ktype,
          (Syntax.Tplam (Syntax.Ktype, (Syntax.Tnewtag (Syntax.Cvar (0, None)))))
          ))
       )) |}]

let%expect_test "subst_term Cexn (Tunpack (0, Tvar 1, Tplam (Ktype, Tplam (Ktype, Tnewtag (Cvar 2)))))" =
  let result = subst_term Cexn (Tunpack (0, Tvar 1, Tplam (Ktype, Tplam (Ktype, Tnewtag (Cvar (2, None)))))) in
  printf "%s\n" (show_term result);
  [%expect {|
    (Syntax.Tunpack (0, (Syntax.Tvar 1),
       (Syntax.Tplam (Syntax.Ktype,
          (Syntax.Tplam (Syntax.Ktype, (Syntax.Tnewtag (Syntax.Cvar (2, None)))))
          ))
       )) |}]

let%expect_test "subst_term Cexn (Tunpack (0, Tvar 1, Tplam (Ktype, Tplam (Ktype, Tnewtag (Cvar 3)))))" =
  let result = subst_term Cexn (Tunpack (0, Tvar 1, Tplam (Ktype, Tplam (Ktype, Tnewtag (Cvar (3, None)))))) in
  printf "%s\n" (show_term result);
  [%expect {|
    (Syntax.Tunpack (0, (Syntax.Tvar 1),
       (Syntax.Tplam (Syntax.Ktype,
          (Syntax.Tplam (Syntax.Ktype, (Syntax.Tnewtag Syntax.Cexn)))))
       )) |}]

let%expect_test "subst_term Cexn (Tunpack (0, Tvar 1, Tplam (Ktype, Tplam (Ktype, Tnewtag (Cvar 4)))))" =
  let result = subst_term Cexn (Tunpack (0, Tvar 1, Tplam (Ktype, Tplam (Ktype, Tnewtag (Cvar (4, None)))))) in
  printf "%s\n" (show_term result);
  [%expect {|
    (Syntax.Tunpack (0, (Syntax.Tvar 1),
       (Syntax.Tplam (Syntax.Ktype,
          (Syntax.Tplam (Syntax.Ktype, (Syntax.Tnewtag (Syntax.Cvar (3, None)))))
          ))
       )) |}]

