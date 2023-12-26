
open Graph_coloring
open Tests

let g1 : graph = 
  ( [N 1; N 2; N 3; N 4], 
    [ (N 1,N 2); (N 1,N 3); (N 2,N 3); (N 3,N 4) ] )

let g1_coloring : coloring = 
  [ (N 1,C 0); (N 2,C 1); (N 3,C 2); (N 4,C 1) ]

let bad_g1_coloring : coloring = 
  [ (N 1, C 1); (N 2, C 1); (N 3,C 3); (N 4,C 2) ]

let g2 : graph =
  ( [N 1; N 2; N 3; N 4], 
    [ (N 1, N 2); (N 1, N 3); (N 1, N 4); 
      (N 2, N 3); (N 3, N 4); (N 4, N 2) ] )


let some_tests =
  [ 
    eval_test (fun () ->
        valid_coloring g1_coloring g1, true )
       "valid_coloring g1_coloring g1"
        string_of_bool;

    eval_test (fun () ->
        valid_coloring bad_g1_coloring g1, false )
       "valid_coloring bad_g1_coloring g1"
        string_of_bool;

    eval_test (fun () ->
        (match color_option g1 with
         | Some c -> valid_coloring c g1
         | None -> false),   
        true )
       "match color_option g1 with
         | Some c -> valid_coloring c g1
         | None -> false"   
        string_of_bool;

    eval_test (fun () ->
        (match color_option g2 with
         | Some c -> valid_coloring c g2
         | None -> false),   
        false )
       "match color_option g2 with
         | Some c -> valid_coloring c g2
         | None -> false"   
        string_of_bool;

    eval_test (fun () ->
       (try color_exception g1 <> () with
         | FoundColoring c -> valid_coloring c g1),
       true )
      "(try color_exception g1 <> () with
         | FoundColoring c -> valid_coloring c g1)"
      string_of_bool;

    eval_test (fun () ->
       (try color_exception g2 <> () with
         | FoundColoring c -> valid_coloring c g2),
       false )
      "(try color_exception g2 <> () with
         | FoundColoring c -> valid_coloring c g2)"
      string_of_bool;

  ]

let run_tests tests = List.iter (fun f -> f ()) tests

let _ = run_tests some_tests

