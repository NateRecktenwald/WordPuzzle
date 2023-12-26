open Set

module ListM : SetS = struct

  type 'a t = 'a list

  let empty = []

  let insert (elem: 'a) (lst: 'a t): 'a t = elem::lst

  let rec elem (element: 'a) (lst: 'a t): bool = 
	match element,lst with 
	| element,[] -> false
	| element,next_elem::rest -> (element = next_elem) || (elem element rest)

end
