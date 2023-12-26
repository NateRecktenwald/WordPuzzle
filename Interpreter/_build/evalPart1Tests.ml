
open Expr
open Tests

let some_tests =
  [ 
    eval_test (fun () ->
        eval [] (Add (Val (Int 3), Val (Int 5))), Int 8 )
       "eval [] (Add (Val (Int 3), Val (Int 5)))"
        serialize_value serialize_excp;

    eval_test (fun () ->
        eval [] (And (Val (Bool true), Val (Bool true))), Bool true )
       "eval [] (And (Val (Bool true), Val (Bool true)))"
        serialize_value serialize_excp;

    eval_test (fun () ->
        eval [] (And (Val (Bool true), Val (Bool false))), Bool false )
       "eval [] (And (Val (Bool true), Val (Bool false)))"
        serialize_value serialize_excp;

    eval_test (fun () ->
        eval [] (Eq (Val (Bool true), Val (Bool true))), Bool true )
       "eval [] (Eq (Val (Bool true), Val (Bool true)))"
        serialize_value serialize_excp;

    eval_test (fun () ->
        eval [] (Eq (Val (Bool true), Val (Bool false))), Bool false )
       "eval [] (Eq (Val (Bool true), Val (Bool false)))"
        serialize_value serialize_excp;

    eval_test (fun () ->
        eval [] (Eq (Val (Int 5), Val (Int 5))), Bool true )
       "eval [] (Eq (Val (Int 5), Val (Int 5)))"
        serialize_value serialize_excp;

    eval_test (fun () ->
        eval [("x", Int 4)] (Id "x"), Int 4)
       "eval [(\"x\", Int 4)] (Id \"x\")"
        serialize_value serialize_excp;

    eval_test (fun () ->
        eval [] (Let ("x", Add (Val (Int 3), Val (Int 4)),
                        Lt (Id "x", Val (Int 5)))), Bool false )
       "eval [] (Let (\"x\", Add (Val (Int 3), Val (Int 4)), Lt (Id \"x\", Val (Int 5))))"
        serialize_value serialize_excp;

    check_excp (fun () ->
        eval [] (Div (Val (Int 3), Sub (Val (Int 2), Val (Int 2)))) )
        (DivisionByZero (Div (Val (Int 3), Sub (Val (Int 2), Val (Int 2)))) )
       "eval [] (Div (Val (Int 3), Sub (Val (Int 2), Val (Int 2))))"
        serialize_value
        serialize_excp;

    check_excp (fun () ->
        eval [] (Add (Val (Bool true), Val (Int 2))) )
        (IncorrectType (Add (Val (Bool true), Val (Int 2))) )
       "eval [] (Add (Val (Bool true), Val (Int 2)))"
        serialize_value
        serialize_excp;

    check_excp (fun () ->
        eval [] (Div (Val (Bool true), Sub (Val (Int 2), Val (Int 2)))) )
        (IncorrectType (Div (Val (Bool true), Sub (Val (Int 2), Val (Int 2)))) )
       "eval [] (Div (Val (Bool true), Sub (Val (Int 2), Val (Int 2))))"
        serialize_value
        serialize_excp;

    check_excp (fun () ->
        eval [] (Id "x"))
        (UnboundVariable (Id "x"))
       "eval [] (Id \"x\")"
        serialize_value
        serialize_excp;

    check_excp (fun () ->
        eval [] (Add (Val (Int 4), Id "x")) )
        (UnboundVariable (Id "x"))
       "eval [] (Add (Val (Int 4), Id \"x\")"
        serialize_value
        serialize_excp;

    check_excp (fun () ->
        eval [("x", Int 4)] (Id "y"))
        (UnboundVariable (Id "y"))
       "eval [(\"y\", Int 4)] (Id \"x\")"
        serialize_value
        serialize_excp;

  ]

let run_tests tests = List.iter (fun f -> f ()) tests

let _ = run_tests some_tests

