(* Ocaml implementations of red-black trees *)
(* ref: Purely Functional Data Structures by by C Okasaki *)

(* Data type definition *)
type color = Black | Red

type 'a rbtree = Leaf | Node of color * 'a * 'a rbtree * 'a rbtree

(* check membership in red-black trees *)
let rec mem (x: 'a) (tree: 'a rbtree): bool  = match tree with
  | Leaf -> false
  | Node (_, y, left, right) -> 
      x = y || (x < y && mem x left) || (x > y && mem x right)

(* Consider all the cases where a Red node has a Red child and 
 * rearranges the tree *)
let balance (tree: 'a rbtree) : 'a rbtree = match tree with
  | Node (Black, z, Node (Red, y, Node (Red, x, a, b), c), d) 
  | Node (Black, z, Node (Red, x, a, Node (Red, y, b, c)), d)
  | Node (Black, x, a, Node (Red, z, Node (Red, y, b, c), d))
  | Node (Black, x, a, Node (Red, y, b, Node (Red, z, c, d)))
    -> Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
  | _ -> tree

(* insert new element to a red-black tree *)
let rec insert (x: 'a) (tree: 'a rbtree) : 'a rbtree = 
  let rec ins subtree = match subtree with
    | Leaf -> Node (Red, x, Leaf, Leaf)
    | Node (color, y, left, right) -> 
        if x < y then balance (Node (color, y, ins left, right))
        else if x > y then balance (Node (color, y, left, ins right))
        else subtree in
  match ins tree with
  | Node (_, y, a, b) -> Node (Black, y, a, b)
  | Leaf -> (* guaranteed to be nonempty *)
      failwith "rbtree insert failed with ins returning leaf"

(* convert list of elements to a red-black tree *)
let rec tree_of_list (lst: 'a list) : 'a rbtree = match lst with
  | [] -> Leaf
  | x::xs -> insert x (tree_of_list xs)


