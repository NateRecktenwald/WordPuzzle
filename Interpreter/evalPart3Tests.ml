
open Expr
open Tests

let id str = str

let show_freevars (vs: string list) : string =
  let wrap s = "\"" ^ s ^ "\"" in
  "[" ^ (String.concat "; " (List.map wrap vs)) ^ "]"

let some_tests = [ 
  eval_test (fun () ->
     serialize_expr (If (Val (Bool true), Val (Int 3), Val (Int 2))),
     "If (Val (Bool true), Val (Int 3), Val (Int 2))"  )
    "serialize_expr (If (Val (Bool true), Val (Int 3), Val (Int 2)))"
     id serialize_excp;

  eval_test (fun () ->
     freevars (If (Id "cond", Val (Int 3), Id "else")),
     [ "cond"; "else" ] )
    "freevars (If (Id \"cond\", Val (Int 3), Id \"else\"))"
     show_freevars serialize_excp;

  eval_test (fun () ->
    freevars 
      (LetRec ("sumToN", 
               Lam ("n", 
                    If (Eq (Id "n", Val (Int 0)),
                        Val (Int 0),
                        Add (Id "n",
                             App (Id "sumToN",
                                  Sub (Id "n", Val (Int 1))
                                 )
                            )
                       )
                   ),
               App (Id "sumToN", Val (Int 10))
              )), 
       [] )
    "freevars 
      (LetRec (\"sumToN\", 
               Lam (\"n\", 
                    If (Eq (Id \"n\", Val (Int 0)),
                        Val (Int 0),
                        Add (Id \"n\",
                             App (Id \"sumToN\",
                                  Sub (Id \"n\", Val (Int 1))
                                 )
                            )
                       )
                   ),
               App (Id \"sumToN\", Val (Int 10))
              ))"
     show_freevars serialize_excp;

  eval_test (fun () ->
     eval [] 
      (LetRec ("sumToN", 
               Lam ("n", 
                    If (Eq (Id "n", Val (Int 0)),
                        Val (Int 0),
                        Add (Id "n",
                             App (Id "sumToN",
                                  Sub (Id "n", Val (Int 1))
                                 )
                            )
                       )
                   ),
               App (Id "sumToN", Val (Int 10))
              )), 
       Int 55 )
    "eval []
      (LetRec (\"sumToN\", 
               Lam (\"n\", 
                    If (Eq (Id \"n\", Val (Int 0)),
                        Val (Int 0),
                        Add (Id \"n\",
                             App (Id \"sumToN\",
                                  Sub (Id \"n\", Val (Int 1))
                                 )
                            )
                       )
                   ),
               App (Id \"sumToN\", Val (Int 10))
              ))"
     serialize_value serialize_excp;
]

let run_tests tests = List.iter (fun f -> f ()) tests

let _ = run_tests some_tests



