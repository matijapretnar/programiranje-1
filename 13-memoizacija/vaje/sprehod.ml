(*----------------------------------------------------------------------------*]
 Učinkovito poiščite vrdnost najdražjega sprehoda od korena do listov drevesa.

 Popravite tudi funkcijo, ki poišče seznam smeri najdražjega sprehoda.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_path Empty ;;
 - : 'a option = None
 # max_path test_tree;;
- : int option = 21
[*----------------------------------------------------------------------------*)


type 'a tree
 = Empty
 | Node of ('a tree) * 'a * ('a tree)

let leaf x = Node (Empty, x, Empty)

let test_tree = Node( Node(leaf 0, 2, leaf 13), 5, Node(leaf 9, 7, leaf 4))

type direcion 
  = Left
  | Right
