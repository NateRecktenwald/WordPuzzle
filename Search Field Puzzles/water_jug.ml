(*
  The state a of type int * int and in the possible_steps function 
  I split the state into the four_gal and three_gal which store the 
  amount in each jug and also so I can change them in the function 
  to their proper value
*)

type operation = Fill4GallonJugFromTap
          | Fill3GallonJugFromTap
          | Empty4GallonJugOnGround
          | Empty3GallonJugOnGround
          | Fill4GallonJugFrom3GallonJug
          | Fill3GallonJugFrom4GallonJug
          | Empty4GallonJugInto3GallonJug
          | Empty3GallonJugInto4GallonJug

type state = int * int


let describe (four:int) (three:int) : string = 
  let describe' jug amount =
    "The " ^ string_of_int jug ^ " gallon jug " ^
    match amount with
    | 0 -> " is empty"
    | 1 -> " contains 1 gallon"
    | x -> " contains " ^ string_of_int x ^ " gallons"
  in
  describe' 4 four ^ ", " ^ describe' 3 three ^ "."

(* some functions that you may find helpful *)
let initial_state : state = (0, 0)

let final_state ((x,y): state) : bool = x = 2

let valid_state ((x,y): state) : bool 
  = x >= 0 && x <= 4 && y >= 0 && y <= 3

let possible_steps (s: state) : (operation * state) list = 
  match s with 
  | (four_gal, three_gal) ->
      let four_gal_from_tap =
        if four_gal < 4 
        then [(Fill4GallonJugFromTap, (4, three_gal))]
        else []
      in
      let three_gal_from_tap =
        if three_gal < 3
        then [(Fill3GallonJugFromTap, (four_gal, 3))]
        else []
      in
      let empty_four_gal =
        if four_gal > 0 
        then [(Empty4GallonJugOnGround, (0, three_gal))]
        else []
      in
      let empty_three_gal =
        if three_gal > 0
        then [(Empty3GallonJugOnGround, (four_gal, 0))]
        else []
      in 
      let four_gal_from_three_gal =
        if (four_gal < 4) && (three_gal > 0)
        then [(Fill4GallonJugFrom3GallonJug, (4, three_gal - (4 - four_gal)))]
        else []
      in
      let three_gal_from_four_gal =  
        if (three_gal < 3) && (four_gal > 0) 
        then [(Fill3GallonJugFrom4GallonJug, (four_gal - (3 - three_gal), 3))]
        else []
      in 
      let empty_four_into_three =
        if ((four_gal + three_gal) < 4) && (four_gal > 0) 
        then [(Empty4GallonJugInto3GallonJug, (0, four_gal + three_gal))]
        else []
      in 
      let empty_three_into_four =
        if ((four_gal + three_gal) < 5) && (three_gal > 0) 
        then [(Empty3GallonJugInto4GallonJug, (four_gal + three_gal, 0))]
        else []
      in
      four_gal_from_tap @ three_gal_from_tap @ empty_four_gal @ empty_three_gal @
      four_gal_from_three_gal @ three_gal_from_four_gal @ empty_four_into_three @
      empty_three_into_four

let play (s: state) : (operation * state) list option =
  let rec go (curr_state: state) (sol: (operation * state) list) : (operation * state) list option =
    if final_state curr_state
    then Some sol
    else 
      let path = List.map (fun (o, st)  -> st) sol
      in
      let possibles =  List.filter (fun (o, st) -> not (List.mem st path))
          (possible_steps curr_state)
      in 
      let rec try_state (lst: (operation * state) list) : (operation * state) list option =
        match lst with 
        | [] -> None
        | x::xs -> match x with 
                   | (o, st) -> match go st (sol @ [x]) with 
                                | Some l -> Some l
                                | None -> try_state xs 
      in 
      try_state possibles
  in 
  go s []
