
type node = N of int
type edge = node * node
type graph = node list * edge list

type color = C of int
type coloring = (node * color) list


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


exception FoundColoring of coloring

let rec get_color (n: node) (col: coloring) : color =
	match col with 
	| [] -> raise (Failure "this shouldn't happen!!")
	| (num, c)::xs -> if num = n 
					  then c
					  else get_color n xs

let rec check_if_valid (e: edge list) (col: coloring) : bool =
	match e with
	| [] -> true
	| (n1, n2)::xs -> if (get_color n1 col) = (get_color n2 col)
					  then false 
					  else check_if_valid xs col


let rec valid_coloring (col: coloring) (g: graph) : bool =
	match g with 
	| (n, e) -> check_if_valid e col

let color_option (g: graph) : coloring option = 
	let rec assign_color_to_node (lst: node list) (new_lst: coloring) (g: graph) : coloring option = 
		match lst with 
		| [] -> if valid_coloring new_lst g 
				then Some (List.rev new_lst)
				else None
		| x::xs -> match (assign_color_to_node xs ((x, C 1)::new_lst) g) with 
				   | Some l -> Some l
				   | None -> match assign_color_to_node xs ((x, C 2)::new_lst) g with 
				   			 | Some l -> Some l
				   			 | None -> assign_color_to_node xs ((x, C 3)::new_lst) g
	in 
	match g with 
	| (n, e) -> assign_color_to_node n [] g

let color_exception (g: graph) : unit = 
	let rec try_color (lst: node list) (sol: coloring) : unit = 
		if (lst = [] && (valid_coloring sol g)) 
		then raise (FoundColoring sol)
		else match lst with 
			 | [] -> ()
			 | x::xs -> try_color xs ((x, C 1)::sol);
			 			try_color xs ((x, C 2)::sol);
			 			try_color xs ((x, C 3)::sol);
	in
	match g with 
	| (n, e) -> try_color n []