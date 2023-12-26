open Set

module type  TreeS = sig
  type 'a tree = Leaf
               | Fork of 'a tree * 'a * 'a tree

  type 'a t = 'a tree

  val empty : 'a t

  val insert : 'a -> 'a t -> 'a t

  val elem : 'a -> 'a t -> bool

  val height: 'a t -> int

  val size: 'a t -> int

end

module TreeM : TreeS = struct

  type 'a tree = Leaf
               | Fork of 'a tree * 'a * 'a tree

  type 'a t = 'a tree

  let empty = Leaf

  let rec insert (elem: 'a) (tree: 'a t): 'a t =
    match tree with 
    | Leaf -> Fork (Leaf, elem, Leaf) 
    | Fork (left, v, right) -> if (elem < v) 
                               then Fork (insert elem left, v, right)
                               else Fork (left, v, insert elem right)

  let rec elem (element: 'a) (tree:'a t): bool = 
    match tree with 
    | Leaf -> false
    | Fork (left, v, right) -> if element < v 
                               then elem element left
                               else if element > v
                               then elem element right
                               else true

  let rec height (tree: 'a t): int = 
    match tree with 
    | Leaf -> 0
    | Fork (left, v, right) -> 1 + max (height left) (height right)

  let rec size (tree: 'a t): int =
    match tree with 
    | Leaf -> 0
    | Fork (left, v, right) -> 1 + (size left) + (size right)


end

module TreeSet : SetS = TreeM
