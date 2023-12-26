
open Expr
open Tests

let id str = str

let show_freevars (vs: string list) : string =
  let wrap s = "\"" ^ s ^ "\"" in
  "[" ^ (String.concat "; " (List.map wrap vs)) ^ "]"

let some_tests = [ 
  eval_test (fun () ->
     freevars
       (Lam ("n", 
             If (Eq (Id "n", Val (Int 0)),
                 Val (Int 0),
                 Add (Id "n",
                      App (Id "sumToN",
                           Sub (Id "n", Val (Int 1))
                        )
                   )
               )
          )
       ),
     [ "sumToN" ] )
    "freevars
       (Lam (\"n\", 
             If (Eq (Id \"n\", Val (Int 0)),
                 Val (Int 0),
                 Add (Id \"n\",
                      App (Id \"sumToN\",
                           Sub (Id \"n\", Val (Int 1))
                        )
                   )
               )
          )
       )"
     show_freevars serialize_excp;

  check_excp (fun () ->
     eval [ ("x", Int 1); ("y", Int 2); ("z", Int 3) ]
       (Lam ("n", 
             If (Eq (Id "n", Val (Int 0)),
                 Val (Int 0),
                 Add (Id "n",
                      App (Id "sumToN",
                           Sub (Id "n", Val (Int 1))
                        )
                   )
               )
          )
       )
    )
    (UnboundVariable 
       (Lam ("n", 
             If (Eq (Id "n", Val (Int 0)),
                 Val (Int 0),
                 Add (Id "n",
                      App (Id "sumToN",
                           Sub (Id "n", Val (Int 1))
                        )
                   )
               )
          )
       )
    )
    "eval [ (\"x\", Int 1); (\"y\", Int 2); (\"z\", Int 3) ]
       (Lam (\"n\", 
             If (Eq (Id \"n\", Val (Int 0)),
                 Val (Int 0),
                 Add (Id \"n\",
                      App (Id \"sumToN\",
                           Sub (Id \"n\", Val (Int 1))
                        )
                   )
               )
          )
       )"
     serialize_value serialize_excp;


  eval_test (fun () ->
     (match 
       eval [ ("sumToN", Ref (ref (Int 999))) ]
         (Lam ("n", 
               If (Eq (Id "n", Val (Int 0)),
                   Val (Int 0),
                   Add (Id "n",
                        App (Id "sumToN",
                             Sub (Id "n", Val (Int 1))
                          )
                     )
                 )
            )
         )
     with
     | Closure (name, body, closure_env) -> List.length closure_env
     | _ -> -1000 )
     , 1 )
    "match 
       eval [ (\"sumToN\", Ref (ref (Int 999))) ]
         (Lam (\"n\", 
               If (Eq (Id \"n\", Val (Int 0)),
                   Val (Int 0),
                   Add (Id \"n\",
                        App (Id \"sumToN\",
                             Sub (Id \"n\", Val (Int 1))
                          )
                     )
                 )
            )
         )
     with
     | Closure (name, body, closure_env) -> List.length closure_env
     | _ -> -1000"
     string_of_int serialize_excp;
]

let run_tests tests = List.iter (fun f -> f ()) tests

let _ = run_tests some_tests



