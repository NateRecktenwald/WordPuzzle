
type test = unit -> unit


let eval_test (f: unit -> ('a * 'a)) (expr_str: string)
      (show: 'a -> string) (show_exn: exn -> string) : test =
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
   | e ->
      let msg = "FAILED:\n    `" ^ expr_str ^ 
                   "`\n  raised the exception `" ^ show_exn e ^ "`."
       in
       print_endline msg
  )

let check_excp (f: unit -> 'a) (excp: exn) (expr_str: string) 
      (show: 'a -> string) (show_exn: exn -> string) : test =
  fun () ->
  try
    let v = f () in
    let msg = "FAILED:\n    `" ^ expr_str ^ 
                "`\n  should have raised the exception\n    `" ^
                  show_exn excp ^ "`\n  but instead evaluated to\n    `" ^
                    show v ^ "`."
    in
    print_endline msg
  with 
  | e ->
     if e = excp then
       let msg = "PASSED:\n    `" ^ expr_str ^
                   "`\n  correctly raised the exception\n    `" ^
                     show_exn excp ^ "`."
       in print_endline msg
     else
       let msg = "FAILED:\n    `" ^ expr_str ^
                   "`\n  should have raised the exception\n    `" ^
                     show_exn excp ^ "`\n  but instead raised " ^
                       (let showe = show_exn e in
                        if showe = "Unexpected Exception"
                        then "an unexpected exception."
                        else "the exception\n    `" ^ show_exn e ^
                               "`."
                       )
       in print_endline msg

let msg_test (s: string) : test =
  fun () ->
  print_endline s

let run_tests tests = List.iter (fun f -> f ()) tests
