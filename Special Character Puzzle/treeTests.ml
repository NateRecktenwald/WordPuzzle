
open Tests

module TreeTests (T: Tree.TreeS) = struct
  open T

  (* The odd use of List.rev below is used since fold_right will start
     at the end of the list and we might like to read the order in which 
     the values are inserted by going from the left to the right.

     Thus, 2 is inserted into the empty tree, then 4, etc.
   *)

  let evens_tree_unbalanced = 
    List.fold_right insert (List.rev [2; 4; 6; 8; 10]) empty

  let odds_tree_balanced = 
    List.fold_right insert (List.rev [5; 3; 1; 7; 9]) empty

  let some_tests = [
      check_test (fun () -> 
          size evens_tree_unbalanced = 5)
         "size evens_tree_unbalanced = 5";

      check_test (fun () -> 
          size evens_tree_unbalanced = 5)
         "size evens_tree_unbalanced = 5";

      check_test (fun () ->
          height Leaf = 1)
          "height Leaf = 1";

      check_test (fun () ->
          height (Fork (Leaf, 2, Fork (Leaf, 3, Leaf))) = 3)
         "height (Fork (Leaf, 2, Fork (Leaf, 3, Leaf))) = 3";

      check_test (fun () ->
          height evens_tree_unbalanced > height odds_tree_balanced)
         "height evens_tree_unbalanced > height odds_tree_balanced";
    ]
                 
  let run () = run_tests some_tests

end

module TreeMTests = TreeTests (Tree.TreeM)


let _ =
  print_endline ("Tree Tests.\n");

  print_endline ("Tree Tests:");
  TreeMTests.run ();



