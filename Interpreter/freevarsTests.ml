
open Expr
open Tests

let show_freevars (vs: string list) : string =
  let wrap s = "\"" ^ s ^ "\"" in
  "[" ^ (String.concat "; " (List.map wrap vs)) ^ "]"

let some_tests =
  [ 
    eval_test (fun () ->
        freevars (Add (Val (Int 3), Val (Int 5))), [] )
       "freevars (Add (Val (Int 3), Val (Int 5)))"
        show_freevars serialize_excp;

    eval_test (fun () ->
        freevars (Add (Id "a", Id "b")), ["a"; "b"] )
       "freevars (Add (Id \"a\", Id \"b\"))"
        show_freevars serialize_excp;

    eval_test (fun () ->
        freevars (Let ("x", Add (Val (Int 3), Val (Int 4)),
                        Lt (Id "x", Val (Int 5)))),  [] )
       "freevars (Let (\"x\", Add (Val (Int 3), Val (Int 4)), Lt (Id \"x\", Val (Int 5))))"
        show_freevars serialize_excp;

    eval_test (fun () ->
        freevars (Let ("x", Add (Val (Int 3), Id "x"),
                        Lt (Id "x", Val (Int 5)))),  ["x"] )
       "freevars (Let (\"x\", Add (Val (Int 3), Id \"x\"), Lt (Id \"x\", Val (Int 5))))"
        show_freevars serialize_excp;

    eval_test (fun () ->
        freevars (Let ("x", Add (Val (Int 3), Id "x"),
                        Lt (Id "y", Val (Int 5)))),  ["x"; "y"] )
       "freevars (Let (\"x\", Add (Val (Int 3), Id \"x\"), Lt (Id \"y\", Val (Int 5))))"
        show_freevars serialize_excp;

    eval_test (fun () ->
        freevars (Lam ("n", Add (Id "n", Val (Int 1)))), [] )
       "freevars (Lam (\"n\", Add (Id \"n\", Val (Int 1))))"
       show_freevars serialize_excp;

    eval_test (fun () ->
        freevars (Lam ("n", Add (Id "m", Val (Int 1)))), ["m"] )
       "freevars (Lam (\"n\", Add (Id \"m\", Val (Int 1))))"
       show_freevars serialize_excp;

  ]

let run_tests tests = List.iter (fun f -> f ()) tests

let _ = run_tests some_tests

