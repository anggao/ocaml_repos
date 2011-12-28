(* Ocaml implementations of Binomial heaps *)
(* ref: Purely Functional Data Structures by by C Okasaki *)

(* Data type definition *)
type 'a binomialTree = Node of int * 'a * ('a binomialTree) list

type 'a binomialHeap = ('a binomialTree) list

(* Link two binomialTrees with same rank *)
let link (Node (r, x1, c1) as t1) (Node (_, x2, c2) as t2) = 
  if x1 <= x2 then Node (r+1, x1, t2::c1)
  else Node (r+1, x2, t1::c2)

let empty = []
let isEmpty s = s = []
let rank (Node (r, _, _)) = r
let root (Node (_, x, _)) = x

(* Insert a new singleton binomialTree into a binomialHeap *)
let rec insTree (nt: 'a binomialTree) (trees : 'a binomialHeap) 
        : 'a binomialHeap
  = match trees with
  | [] -> [nt]
  | t::ts -> 
      if rank nt < rank t then nt::trees
      else insTree (link nt t) ts 

(* Insert a new element into a binomialHeap *)
let insert (x : 'a) (trees: 'a binomialHeap) : 'a binomialHeap
  = insTree (Node (0, x, [])) trees

(* Merge two binomialHeaps as one *)  
let rec merge (ts1 : 'a binomialHeap) (ts2 : 'a binomialHeap) : 'a binomialHeap 
  = match ts1, ts2 with
  | (trees, []) -> trees
  | ([], trees) -> trees
  | (hd1::tl1, hd2::tl2) -> 
      if rank hd1 < rank hd2 then hd1 :: merge tl1 ts2
      else if rank hd1 > rank hd2 then hd2 :: merge ts1 tl2
      else insTree (link hd1 hd2) (merge tl1 tl2)

(* Finds the tree with the minimum root and removes it from the list, 
 * returning both the tree and the remaining list *)
let rec removeMinTree (bheap : 'a binomialHeap) 
        : 'a binomialTree * 'a binomialTree list
  = match bheap with
  | [] -> raise (Invalid_argument "removeMinTree: can't call with []")
  | [t] -> (t, [])
  | t::ts -> 
      let (t', ts') = removeMinTree ts in
      if root t <= root t' then (t, ts)
      else (t', t::ts')

(* Get the minimum element in the heap *)
let findMin bheap = let (t,_) = removeMinTree bheap in root t

(* Delete minimum element from binomialHeap *)
let deleteMin bheap = let (Node(_, x, ts1), ts2) = removeMinTree bheap in
                      merge (List.rev ts1) ts2

(* convert list of elements to a binomialHeap *)
let rec bheap_of_list (lst: 'a list) : 'a binomialHeap = match lst with
  | [] -> []
  | x::xs -> insert x (bheap_of_list xs)

