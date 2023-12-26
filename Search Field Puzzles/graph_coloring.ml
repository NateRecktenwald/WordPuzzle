(*

1. The search space we are exploring in the graph coloring problem is looking for a valid solution 
   to the graph coloring problem. Which is when two nodes next to each other or connected do not have
   the same color. The steps to find a solution are to first create a function that checks if the 
   solution is valid by checking the connected nodes and seeing if they have the same color which
   is done in valid_coloring. Next we need to take a graph with a node list and an edge list and give
   each node a color while checking the node it is conneted to doesn't have the same color this is done 
   in a helper function called assign_color_to_node. So, for one node we give it a color and check if 
   the color works with the node it is connected to then if it works add it to the solution list. If it 
   doesn't then we check another color and if none of the colors work there is no solution for the graph. 
   This process is done in the match statements in assign_color_to_node where options are used to check
   if there is some solution and to try another color if necessary. The same is done for the exception
   function except we don't need the match cases because the try function will try the color and if it
   doesn't work it will move to the next color without using a match statement instead a semi-colon is 
   used.

2. In the exception function and in the option function after all of the colors are tried and none of 
   them have sucess then the helper function just ends and returns either unit or None because no
   answer exists. The function stops because there isn't any code to deal with if none of the colors work.


*)


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