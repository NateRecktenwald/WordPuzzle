
open Expr
open Tests

let some_tests = [ 
  eval_test (fun () -> 
     eval [] (App (Lam ("x", Add (Id "x", Val (Int 1))), 
                   Val (Int 2))),
              Int 3 )
    "eval [] (App (Lam (\"x\", Add (Id \"x\", Val (Int 1))),
                   Val (Int 2)))"
     serialize_value serialize_excp;

  eval_test (fun () -> 
     eval [] (Let ("inc", Lam ("x", Add (Id "x", Val (Int 1))),
                   App (Id "inc", Val (Int 2)))),
              Int 3 )
    "eval [] (Let (\"inc\", Lam (\"x\", Add (Id \"x\", Val (Int 1))),
                   App (Id \"inc\", Val (Int 2))))"
     serialize_value serialize_excp;

  eval_test (fun () -> 
     eval [] (App (App (Lam ("x", Lam ("y", Add (Id "x", Id "y"))), 
                   Val (Int 2)), Val (Int 3))),
              Int 5 )
    "eval [] (App (App (Lam (\"x\", Lam (\"y\", Add (Id \"x\", Id \"y\"))), 
                   Val (Int 2)), Val (Int 3)))"
     serialize_value serialize_excp;

  eval_test (fun () -> 
     eval [] (Let ("add", Lam ("x", Lam ("y", Add (Id "x", Id "y"))),
              Let ("inc", Lam ("n", App (App (Id "add", Id "n"), Val (Int 1))),
              App (Id "inc", Val (Int 9))))),
              Int 10 )
    "eval [] (Let (\"add\", Lam (\"x\", Lam (\"y\", Add (Id \"x\", Id \"y\"))),
              Let (\"inc\", Lam (\"n\", App (App (Id \"add\", Id \"n\"), Val (Int 1))),
              App (Id \"inc\", Val (Int 9)))))
     (* let add = fun x -> (fun y -> x + y) in
        let inc = fun n -> (add n) 1 in
        inc 9 
      *)"
     serialize_value serialize_excp;

  eval_test (fun () -> 
     eval [] (Let ("add", Lam ("x", Lam ("y", Add (Id "x", Id "y"))),
              Let ("x", Val (Int 3),
              Let ("inc_by_x", Lam ("n", App (App (Id "add", Id "n"), Id "x")),
              App (Id "inc_by_x", Val (Int 9)))))),
              Int 12 )
    "eval [] (Let (\"add\", Lam (\"x\", Lam (\"y\", Add (Id \"x\", Id \"y\"))),
              Let (\"inc\", Lam (\"n\", App (App (Id \"add\", Id \"n\"), Val (Int 1))),
              App (Id \"inc\", Val (Int 9)))))
     (* let add = fun x -> (fun y -> x + y) in
        let x = 3 in
        let inc_by_x = fun n -> (add n) x in
        inc_by_x 9 
      *)"
     serialize_value serialize_excp;

  check_excp (fun () ->
     eval [] (App (Val (Bool true), Val (Int 2))) )
     (IncorrectType (App (Val (Bool true), Val (Int 2))) )
    "eval [] (App (Val (Bool true), Val (Int 2)))"
     serialize_value serialize_excp;

]


let run_tests tests = List.iter (fun f -> f ()) tests

let _ = run_tests some_tests

