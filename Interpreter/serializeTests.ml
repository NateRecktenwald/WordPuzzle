
open Expr
open Tests

(* `id` is used as the "show" function for `eval_test`. This function
   converts the value in the test to a string. Here, the values are
   strings so the identify function is all we need. 
 *)
let id str = str

let some_tests =
  [ 
    eval_test (fun () ->
        serialize_expr (Add (Val (Int 3), Val (Int 5))),
       "Add (Val (Int 3), Val (Int 5))"  )
       "serialize_expr (Add (Val (Int 3), Val (Int 5)))"
        id serialize_excp;

    eval_test (fun () ->
        serialize_expr (Add (Val (Int 3), Val (Bool true))),
       "Add (Val (Int 3), Val (Bool true))"  )
       "serialize_expr (Add (Val (Int 3), Val (Bool true)))"
       id serialize_excp;

    eval_test (fun () ->
        serialize_expr (Not (Val (Bool true))),
       "Not (Val (Bool true))"  )
       "serialize_expr (Not (Val (Bool true)))"
       id serialize_excp;

    eval_test (fun () ->
        serialize_expr (Let ("x", Add (Val (Int 3), Val (Int 4)),
                        Lt (Id "x", Val (Int 5)))),
       "Let (\"x\", Add (Val (Int 3), Val (Int 4)), Lt (Id \"x\", Val (Int 5)))"   )
       "serialize_expr (Let (\"x\", Add (Val (Int 3), Val (Int 4)), Lt (Id \"x\", Val (Int 5))))"
        id serialize_excp;

    eval_test (fun () ->
        serialize_expr (Lam ("n", Add (Id "n", Val (Int 1)))),
       "Lam (\"n\", Add (Id \"n\", Val (Int 1)))"   )
       "serialize_expr (Lam (\"n\", Add (Id \"n\", Val (Int 1))))"
       id serialize_excp;
    
  ]

let run_tests tests = List.iter (fun f -> f ()) tests

let _ = run_tests some_tests

