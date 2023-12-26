
type test = unit -> unit

let eval_test (f: unit -> ('a * 'a)) (expr_str: string)
      (show: 'a -> string) : test =
  fun () ->
  (try
     let expr, expected = f ()
     in
     if expr = expected then
       let msg = "PASSED:\n    `" ^ expr_str ^ 
                   "`\n correctly evaluates to\n    `" ^
                     show expected ^ "`."
       in
       print_endline msg
     else
       let msg = "FAILED:\n    `" ^ expr_str ^ 
                   "`\n  inccorrectly evaluates to\n    `" ^
                     show expr ^ "`\n  but should have evaluated to\n    `" ^
                     show expected ^ "`."
       in
       print_endline msg
   with
   | _ ->
      let msg = "FAILED:\n    `" ^ expr_str ^ 
                   "`\n  raised an exception but should have computed a avalue."
       in
       print_endline msg
  )

let run_tests tests = List.iter (fun f -> f ()) tests
