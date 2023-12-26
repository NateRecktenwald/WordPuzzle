
module type UtilS = sig
  val implode: char list -> string

  val explode: string -> char list

  val read_file: string -> char list

  val split_by: ('a -> bool) -> 'a list -> 'a list list

  val read_words: string -> string list
end

module UtilM : UtilS = struct

  let implode (cs: char list) : string =
    String.concat "" (List.map  (String.make 1) cs)

  let explode (s: string) : char list =
    let l = String.length s
    in
    let rec f i = 
      if i = l then [] else s.[i] :: f (i+1)
    in f 0

  let read_file (file_name: string) : char list =
    let ic = open_in file_name 
    in 
    let rec read_chars ic =
      try 
        let next_char = input_char ic
        in next_char :: read_chars ic
      with 
        _ -> [] 
    in read_chars ic

  let split_by (f: 'a -> bool) (lst: 'a list): 'a list list = 
     let rec helper (word: 'a list) (splitter: 'a -> bool) (sofar: 'a  list list) (input: 'a list) =
        match input with 
        | [] -> if word = [] 
                then List.rev(sofar)
                else List.rev(word::sofar)
        | elem::rest -> if (splitter elem)
                        then match word with 
                             | [] -> helper [] splitter sofar rest
                             | _ -> helper [] splitter (word::sofar) rest
                        else helper (word@[elem]) splitter sofar rest
      in 
      helper [] f [] lst


  let read_words (file_name: string) : string list =
    let is_whitespace c = match c with
      | ' ' | '\t' | '\n' | '\r' -> true
      | _ -> false
    in
    List.map implode (split_by is_whitespace (read_file file_name))
end
