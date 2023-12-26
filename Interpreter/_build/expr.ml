(* Hwk 06.  Extend the construts below as specified.

   This a first version of the solution to Hwk 6.

   Any furter changes happened in repo-solutions.
 *)

type value 
  = Int of int
  | Bool of bool
  | Ref of value ref
  | Closure of string * expr * value_environment

and value_environment = (string * value) list
                               
and expr 
  = Val of value
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr 
  | Lt  of expr * expr
  | Eq  of expr * expr
  | And of expr * expr
  | Not of expr
  | Let of string * expr * expr
  | Id  of string
  | App of expr * expr
  | Lam of string * expr
  | LetRec of string * expr * expr
  | If of expr * expr * expr


(* These 5 are for part 4, uncomment them when you start that part *)
(*
  | Nil
  | Cons of expr * expr
  | Head of expr
  | Tail of expr
  | IsEmpty of expr
*)


(* Part 1: Serialize 
   -----------------
 *)

let rec serialize_value (v: value) : string =
  match v with
  | Int i -> "Int " ^ string_of_int i
  | Bool b -> "Bool " ^ string_of_bool b
  | Ref r -> "Ref " ^ serialize_value !r
  | Closure (s, e1, env) -> "<fun>"

let rec serialize_expr (e: expr) : string =
  match e with
  | Val v -> "Val (" ^ (serialize_value v) ^ ")"
  | Id x -> "Id \"" ^ x ^ "\""
  | Add (e1, e2) -> "Add (" ^ serialize_expr e1 ^ ", " ^ serialize_expr e2 ^ ")"
  | Sub (e1, e2) -> "Sub (" ^ serialize_expr e1 ^ ", " ^ serialize_expr e2 ^ ")"
  | Mul (e1, e2) -> "Mul (" ^ serialize_expr e1 ^ ", " ^ serialize_expr e2 ^ ")"
  | Div (e1, e2) -> "Div (" ^ serialize_expr e1 ^ ", " ^ serialize_expr e2 ^ ")"
  | Lt (e1, e2) -> "Lt (" ^ serialize_expr e1 ^ ", " ^ serialize_expr e2 ^ ")"
  | Eq (e1, e2) -> "Eq (" ^ serialize_expr e1 ^ ", " ^ serialize_expr e2 ^ ")"
  | And (e1, e2) -> "And (" ^ serialize_expr e1 ^ ", " ^ serialize_expr e2 ^ ")"
  | Not e1 -> "Not (" ^ serialize_expr e1 ^ ")"
  | App (e1, e2) -> "App (" ^ serialize_expr e1 ^ ", " ^ serialize_expr e2 ^ ")"
  | Lam (s, e1) -> "Lam (" ^ "\"" ^ s ^ "\"" ^ ", " ^ serialize_expr e1 ^ ")"
  | Let (x, e1, e2) -> "Let (" ^ "\"" ^ x ^ "\", " ^ (serialize_expr e1) ^ ", " ^ (serialize_expr e2) ^ ")"
  | LetRec (x, e1, e2) -> "LetRec (" ^ "\"" ^ x ^ "\", " ^ (serialize_expr e1) ^ ", " ^ (serialize_expr e2) ^ ")"
  | If (e1, e2, e3) -> "If (" ^ (serialize_expr e1) ^ ", " ^ (serialize_expr e2) ^ ", " ^ (serialize_expr e3) ^ ")"



(* Part 2: Free Variables
   ----------------------
 *)


let rec freevars (e: expr) : string list =
  match e with
  | Val _ -> []
  | Id x -> [x]
  | Add (e1, e2) -> freevars e1 @ freevars e2
  | Sub (e1, e2) -> freevars e1 @ freevars e2
  | Mul (e1, e2) -> freevars e1 @ freevars e2
  | Div (e1, e2) -> freevars e1 @ freevars e2
  | Lt (e1, e2) -> freevars e1 @ freevars e2
  | Eq (e1, e2) -> freevars e1 @ freevars e2
  | And (e1, e2) -> freevars e1 @ freevars e2
  | Not e1  -> freevars e1
  | App (e1, e2) -> freevars e1 @ freevars e2
  | Lam (s, e2) -> List.filter (fun v -> if v = s then false else true) (freevars e2)
  | Let (x, e1, e2) -> freevars e1 @
                       List.filter (fun v -> if v = x then false else true) (freevars e2)
  | LetRec (x, e1, e2) -> List.filter (fun v -> if v = x then false else true) (freevars e1) @ 
                          List.filter (fun v -> if v = x then false else true) (freevars e2)
  | If (e1, e2, e3) -> freevars e1 @ freevars e2 @ freevars e3
 


(* Part 3: Evaluation 
   ------------------
 *)


exception DivisionByZero of expr
exception UnboundVariable of expr
exception IncorrectType of expr

(* The following 2 exceptions are used in part 4 in
   evaluating lists. You can disregard them until then.
 *)
exception HeadOfEmptyList of expr
exception TailOfEmptyList of expr


let serialize_excp (e: exn) : string =
  match e with
  | DivisionByZero e -> "DivisionByZero (" ^ serialize_expr e ^ ")"
  | UnboundVariable e -> "UnboundVariable (" ^ serialize_expr e ^ ")"
  | IncorrectType e -> "IncorrectType (" ^ serialize_expr e ^ ")"
  | Division_by_zero -> "OCaml built-in Division_by_zero exception"

  (* These are not needed until part 4 and can be disregared until then *)
  | HeadOfEmptyList e -> "HeadOfEmptyList (" ^ serialize_expr e ^ ")"
  | TailOfEmptyList e -> "TailOfEmptyList (" ^ serialize_expr e ^ ")"

  | _ -> "Unexpected Exception"


let rec lookup (name: string) (env: value_environment) (e: expr): value =
  match env with
  | [] -> raise (UnboundVariable e)
  | (n,v) :: rest ->
     if n = name then v
     else lookup name rest e

let rec mapping (en: value_environment) (s_list: string list) (e: expr): (string * value) list =
    match s_list with 
    | [] -> []
    | x::xs -> (x,lookup x en e) :: (mapping en xs e)
            

let rec eval (env: value_environment) (e:expr) : value =
  match e with
  | Val v -> v
  | Id x -> lookup x env e
  | Add (e1, e2) ->
     ( match eval env e1, eval env e2 with
       | Int v1, Int v2 -> Int (v1 + v2)
       | _ -> raise (IncorrectType e)
     )
  | Sub (e1, e2) ->
     ( match eval env e1, eval env e2 with
       | Int v1, Int v2 -> Int (v1 - v2)
       | _ -> raise (IncorrectType e)
     )
  | Mul (e1, e2) ->
     ( match eval env e1, eval env e2 with
       | Int v1, Int v2 -> Int (v1 * v2)
       | _ -> raise (IncorrectType e)
     )
  | Div (e1, e2) ->
     ( match eval env e1, eval env e2 with
       | Int v1, Int v2 -> if v2 = 0 then raise (DivisionByZero e) else Int (v1 / v2)
       | _ -> raise (IncorrectType e)
     )
  | Lt (e1, e2) ->
     ( match eval env e1, eval env e2 with
       | Int v1, Int v2 -> if (v1 < v2) then Bool true else Bool false
       | _ -> raise (IncorrectType e)
     )
  | Eq (e1, e2) ->
     ( match eval env e1, eval env e2 with
       | Int v1, Int v2 -> if (v1 = v2) then Bool true else Bool false
       | Bool v1, Bool v2 -> if (v1 = v2) then Bool true else Bool false
       | _ -> raise (IncorrectType e)
     )
  | And (e1, e2) ->
     ( match eval env e1, eval env e2 with
       | Bool v1, Bool v2 -> if (v1 = true && v2 = true) then Bool true else Bool false
       | _ -> raise (IncorrectType e)
     )
  | Not e1 ->
  (
       match eval env e1 with
       | Bool v1 -> (
                    match v1 with 
                    |true -> Bool false 
                    |false -> Bool true
                    )
       | _ -> raise (IncorrectType e)
  )
  | Lam (s, e2) -> Closure (s, e2, (mapping env (freevars e) e))
  | App (e1, e2) ->
    let ex1 = (eval env e1) 
    in 
    let m1 = match ex1 with
                           | Ref r -> !r
                           | _ -> ex1
    in 
    (
    match m1 with
    | Closure (str, ex, en) -> eval ((str, eval env e2)::en) ex 
    | _ -> raise (IncorrectType e)
    )
  | Let (x, e1, e2) -> 
    (
     let v = eval env e1 in
     let env' = (x, v) :: env in
     eval env' e2
    )
  | LetRec (x, e1, e2) -> 
    (
     let r = ref (Int 0)
     in
     let v = eval ((x, Ref r) :: env) e1
     in
     let () = r := v
     in
     let env' = (x, v) :: env 
     in
     eval env' e2 
    )
  | If (e1, e2, e3) -> 
    (
     if (eval env e1) = Bool true 
     then eval env e2 
     else eval env e3
    )



