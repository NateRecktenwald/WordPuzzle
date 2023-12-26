

type test = unit -> unit

let check_test (f: unit -> bool) (f_str: string) : test =
  fun () ->
  (try
     (if f () then
        print_endline ("PASSED: " ^ f_str)
      else
        print_endline ("FAILED: " ^ f_str)
     )
   with
   | _ -> print_endline ("FAILED, exception raised: " ^ f_str)
  )

let run_tests tests = List.iter (fun f -> f ()) tests
