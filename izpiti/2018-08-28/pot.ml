type 'a tree = Empty | Node of 'a tree * 'a * 'a tree

let longer lst1 lst2 = if List.length lst1 > List.length lst2 then lst1 else lst2

let rec decreasing upper_bound = function
  | Empty -> []
  | Node (l, x, r) when x > upper_bound -> []
  | Node (l, x, r) -> x :: longer (decreasing x l) (decreasing x r)

let rec increasing lower_bound = function
  | Empty -> []
  | Node (l, x, r) when x < lower_bound -> []
  | Node (l, x, r) -> x :: longer (increasing x l) (increasing x r)

let rec longest = function
  | Empty -> []
  | Node (l, x, r) ->
      let combined1 = List.rev (decreasing x l) @ [x] @ increasing x r in
      let combined2 = List.rev (increasing x l) @ [x] @ decreasing x r in
      let long_combined = longer combined1 combined2 in
      let long_subtrees = longer (longest l) (longest r) in
      longer long_combined long_subtrees

(* TESTING *)

let leaf x = Node(Empty, x, Empty)

let test1 =
  let l = Node(leaf 3, 10, Node(leaf 14, 13, leaf 6)) in
  let r = Node(leaf 2, 8, leaf 10) in
  Node(l, 11, r)

let test2 =
  let l = Node(leaf 20, 10, Node(leaf 15, 9, leaf 6)) in
  let r = Node(leaf 1, 4, leaf 10) in
  Node(l, 7, r)
