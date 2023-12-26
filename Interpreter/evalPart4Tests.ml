
open Expr
open Tests

let id str = str

let show_freevars (vs: string list) : string =
  let wrap s = "\"" ^ s ^ "\"" in
  "[" ^ (String.concat "; " (List.map wrap vs)) ^ "]"

let some_tests = [ 
  eval_test (fun () ->
     serialize_expr (Cons (Add (Val (Int 1), Val (Int 2)),
      Cons (Add (Val (Int 3), Val (Int 4)),
            Cons (Add (Val (Int 5), Val (Int 6)), 
                  Nil))) ),
     "Cons (Add (Val (Int 1), Val (Int 2)), Cons (Add (Val (Int 3), Val (Int 4)), Cons (Add (Val (Int 5), Val (Int 6)), Nil)))" )
    "serialize_expr 
       (Cons (Add (Val (Int 1), Val (Int 2)),
              Cons (Add (Val (Int 3), Val (Int 4)),
                    Cons (Add (Val (Int 5), Val (Int 6)), 
                          Nil))) )"
     id serialize_excp;

  eval_test (fun () ->
     eval []
       (Cons (Add (Val (Int 1), Val (Int 2)),
              Cons (Add (Val (Int 3), Val (Int 4)),
                    Cons (Add (Val (Int 5), Val (Int 6)), 
                          Nil))) ),
     List [ Int 3; Int 7; Int 11] )
    "eval []
       (Cons (Add (Val (Int 1), Val (Int 2)),
              Cons (Add (Val (Int 3), Val (Int 4)),
                    Cons (Add (Val (Int 5), Val (Int 6)), 
                          Nil)))"
     serialize_value serialize_excp;

  eval_test (fun () ->
     eval [] 
      (LetRec ("downFrom", 
               Lam ("n", 
                    If (Eq (Id "n", Val (Int 0)),
                        Val (List []),
                        Cons (Id "n",
                              App (Id "downFrom",
                                   Sub (Id "n", Val (Int 1))
                                 )
                            )
                       )
                   ),
               App (Id "downFrom", Val (Int 3))
              )), 
       List [ Int 3; Int 2; Int 1] )
    "eval [] 
      (LetRec (\"downFrom\", 
               Lam (\"n\", 
                    If (Eq (Id \"n\", Val (Int 0)),
                        Val (List []),
                        Cons (Id \"n\",
                              App (Id \"downFrom\",
                                   Sub (Id \"n\", Val (Int 1))
                                 )
                            )
                       )
                   ),
               App (Id \"downFrom\", Val (Int 3)
              )))"
     serialize_value serialize_excp;


  eval_test (fun () ->
     eval [] 
      (LetRec ("sum", 
               Lam ("lst", 
                    If (IsEmpty (Id "lst"),
                        Val (Int 0),
                        Add (Head (Id "lst"),
                             App (Id "sum",
                                  Tail (Id "lst")
                                 )
                            )
                       )
                   ),
       Let ("numbers", 
            Cons (Add (Val (Int 1), Val (Int 2)),
                  Cons (Add (Val (Int 3), Val (Int 4)),
                        Cons (Add (Val (Int 5), Val (Int 6)), 
                              Nil))),
       App (Id "sum", Id "numbers") ) ) ) ,
     Int 21 )  
    "eval [] 
      (LetRec (\"sum\", 
               Lam (\"lst\", 
                    If (IsEmpty (Id \"lst\"),
                        Val (Int 0),
                        Add (Head (Id \"lst\"),
                             App (Id \"sum\",
                                  Tail (Id \"lst\")
                                 )
                            )
                       )
                   ),
       Let (\"numbers\", 
            Cons (Add (Val (Int 1), Val (Int 2)),
                  Cons (Add (Val (Int 3), Val (Int 4)),
                        Cons (Add (Val (Int 5), Val (Int 6)), 
                              Nil))),
       App (Id \"sum\", Id \"numbers\") ) ) )"
     serialize_value serialize_excp;

]

let run_tests tests = List.iter (fun f -> f ()) tests

let _ = run_tests some_tests



