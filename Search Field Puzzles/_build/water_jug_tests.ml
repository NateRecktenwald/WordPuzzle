
open Water_jug
open Tests

(* This function checks the validity of as sequence of steps. *)
let check_validity (some_steps: (operation * state) list option) : bool =
  match some_steps with
  | None -> false
  | Some steps -> 
     let f (sofar, s1) (op, s2) =
       if sofar && List.exists ((=) (op,s2)) (possible_steps s1)
       then (true, s2)
       else (false, s2)
     in
     match List.fold_left f (true, initial_state) steps with
     | (true, s) -> final_state s
     | _ -> false

let some_tests =
  [ 
    eval_test (fun () ->
        List.exists ((=) (Fill4GallonJugFromTap, (4, 0))) (possible_steps (0,0)),
        true )
       "List.exists ((=) (Fill4GallonJugFromTap, (4, 0))) (possible_steps (0,0))"
        string_of_bool;

    eval_test (fun () ->
        List.exists ((=) (Fill3GallonJugFromTap, (0, 3))) (possible_steps (0,0)),
        true )
       "List.exists ((=) (Fill3GallonJugFromTap, (0, 3))) (possible_steps (0,0))"
        string_of_bool;

    eval_test (fun () ->
        List.exists ((=) (Fill4GallonJugFromTap, (4, 2))) (possible_steps (3,2)),
        true )
       "List.exists ((=) (Fill4GallonJugFromTap, (4, 2))) (possible_steps (3,2))"
        string_of_bool;

    eval_test (fun () ->
        List.exists ((=) (Fill4GallonJugFrom3GallonJug, (4, 1))) (possible_steps (3,2)),
        true )
       "List.exists ((=) (Fill4GallonJugFrom3GallonJug, (4, 1))) (possible_steps (3,2))"
        string_of_bool;


    eval_test (fun () ->
        check_validity (play (0, 0)), true )
       "check_validity (play (0, 0))"
        string_of_bool;
  ]

let run_tests tests = List.iter (fun f -> f ()) tests

let _ = run_tests some_tests

