(* Some tests for checking your puzzle solutions. 

   You may want to extend these for cases in which
   words can be modified in more than one way to
   create another valid word.
 *)

open Util
open Tests
open Puzzle

module PuzzleTests (P: PuzzleS) = struct
  open P

  let d1 = "words-small.txt"
  let d2 = "words-google-10000.txt"

  let dictionary_words : string list = UtilM.read_words d1

  let answers = P.answers dictionary_words

  let some_tests = [
      check_test (fun () ->
          List.exists ( (=) ("convey", ["convex"]) ) answers)
         "List.exists ( (=) (\"convey\", [\"convex\"]) ) answers)";

      check_test (fun () ->
          List.exists ( (=) ("manor", ["major"]) ) answers)
         "List.exists ( (=) (\"manor\", [\"major\"]) ) answers)";
    ]

  let run () = run_tests some_tests

end



module ListImpl = PuzzleF ( ListSet.ListM )
module L = PuzzleTests ( ListImpl )

module TreeImpl = PuzzleF ( Tree.TreeSet )
module T = PuzzleTests ( TreeImpl )

let _ = 
  print_endline ("Puzzle tests.\n");

  print_endline ("\nPuzzle with list sets:\n");
  L.run ();

  print_endline ("\nPuzzle with tree sets:\n");
  T.run ();






