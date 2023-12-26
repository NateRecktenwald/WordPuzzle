open Util
open Puzzle

module L = PuzzleF ( ListSet.ListM )

module T = PuzzleF ( Tree.TreeSet )

(* a function to print out the answers *)
let rec print_answers (ans: (string * string list) list) =
  let print_word (word: string) : unit =
    print_string "\""; print_string word; print_string "\""
  in
  let rec print_words (words: string list): unit =
    match words with
    | [] -> raise (Failure "We do not expect to call this with the empty list")
    | [w] -> print_word w
    | w::ws -> print_word w; print_string "; "; print_words ws
  in
  match ans with
  | [] -> ()
  | (w,ws) :: rest -> 
     print_string "("; 
     print_word w;
     print_string ", [";
     print_words ws;
     print_endline "])";
     print_answers rest


(* our two word lists *)
let d1 = "words-small.txt"
let d2 = "words-google-10000.txt"

let dictionary_words : string list = UtilM.read_words d1

let _ = 
  print_endline "\n\nAnswers from module T:\n" ; 
  print_answers (List.sort compare (T.answers dictionary_words));

  print_endline "\n\nAnswers from module L:\n" ; 
  print_answers (List.sort compare (L.answers dictionary_words));

