
open Set
open Util

module type PuzzleS = sig

   
  val letter_insert : char list -> char -> char list list  

  val letters_to_strings : char list list -> string list 

  val all_rares : char list -> string list

  val to_tuple : string -> string list -> string * string list

  val answers : string list -> (string * string list) list

end

module PuzzleF (S: SetS) : PuzzleS = struct

	

	let letter_insert (word: char list) (letter: char): char list list = 

		let rec helper (lst: char list) (prefix: char list) (letter: char): char list list =
			match lst with 
			| [] -> []
			| elem::rest -> if letter = elem 
							then (helper rest (prefix@[elem]) letter)
							else ((prefix@[letter])@rest)::(helper rest (prefix@[elem]) letter)
		in
		helper word [] letter

	let rec letters_to_strings (replaced: char list list): string list =
		match replaced with 
		| [] -> []
		| elem::rest -> (UtilM.implode elem)::(letters_to_strings rest)

	let all_rares (word: char list) : string list = 
	  letters_to_strings((letter_insert word 'x')@(letter_insert word 'j')@(letter_insert word 'q')@(letter_insert word 'z'))

	let to_tuple (word: string) (rare_words: string list) : string * string list =
		(word, rare_words)

	let rec remove_empty (lst: (string * string list) list): (string * string list) list =
		match lst with 
		| [] -> []
		| (elem, [])::rest -> remove_empty rest
		| (elem, rares)::rest -> (elem, rares)::(remove_empty rest)

	let rec check_all_words (rares: string list) (dic: string S.t) : string list =
		match rares with 
		| [] -> []
		| x::rest -> if S.elem x dic
					 then x::(check_all_words rest dic)
					 else check_all_words rest dic
		

	let rec answers (word_list: string list) : (string * string list) list = 

	  	let rec dict (words: string list): string S.t  = 
	  	 List.fold_right (S.insert) words (S.empty)
	  	in 
	  	let rec rare_words (lst: string list) : (string * string list) list =
			match lst with 
			| [] -> []
			| elem::rest -> (to_tuple elem (check_all_words (all_rares(UtilM.explode elem)) (dict word_list)))::(rare_words rest)
		in
		remove_empty (rare_words word_list)


end	 
