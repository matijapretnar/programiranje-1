(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 ADDITIONAL EXERCISES 
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 The function [bst_of_list] constructs a bst out of the elements of a list.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # [11; 6; 7; 0; 2; 5] |> bst_of_list |> is_bst;;
 - : bool = true
[*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*]
 The function [tree_sort] sorts a list by transforming it to a tree and back.

 Note: Please do not actually use this in your code.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # tree_sort ["a"; "c"; "f"; "b"; "e"; "d"];;
 - : string list = ["a"; "b"; "c"; "d"; "e"; "f"]
[*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*]
 The function [follow directions tree] of type [direction list -> 'a tree -> 
 'a option] accepts a list of directions for traversing the tree and returns the
 data in the node at the end of the traversal. Because the directions might not
 lead to an actual node in the tree, the result is returned as an [option] type.
 Don't forget to define the type [directions].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # follow [Right; Left] test_tree;;
 - : int option = Some 6
 # follow [Right; Left; Right; Right] test_tree;;
 - : int option = None
[*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*]
 The function [prune directions tree] finds the node given by [directions] and
 removes the subtree that starts in the node.

 Warning: When using [Some Node(l, x, r)] Ocaml complains because it reads it 
 as [(Some Node)(l, x, r)] so use paranthesis when necessary.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # prune [Right] test_tree;;
 - : int tree option =
 Some (Node (Node (Node (Empty, 0, Empty), 2, Empty), 5, Empty))
[*----------------------------------------------------------------------------*)


(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 PHANTOM TREES

 An additional approach to deletion is to modify the type of the tree. Define a
 new type of tree where nodes additionaly contain information about its state,
 which can either [Exist] or be a [Ghost] if the node is only used for searching
 but is not considered present. We assume that all trees are BST.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)


(*----------------------------------------------------------------------------*]
 The function [phantomize] of type ['a tree -> 'a phantom_tree] maps a regular
 tree into a phantom tree.
 The function [kill x ptree] removes the element [x] from the tree by setting
 it's state to [Ghost].
 Assume that there are no repeated elements in input trees.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # phantomize test_tree;;
 - : int phantom_tree =
 P_Node (P_Node (P_Node (P_Empty, 0, P_Empty, Exists), 2, P_Empty, Exists), 5,
 P_Node (P_Node (P_Empty, 6, P_Empty, Exists), 7,
 P_Node (P_Empty, 11, P_Empty, Exists), Exists),
 Exists)

 # bst_of_list [3; 4; 2] |> phantomize |> kill 3 |> kill 6;;
 - : int phantom_tree =
 P_Node (P_Empty, 2,
 P_Node (P_Node (P_Empty, 3, P_Empty, Ghost), 4, P_Empty, Exists), Exists)
[*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*]
 The function [unphantomize] of type ['a phantom_tree -> 'a tree] maps a
 phantom tree into a regular one, keeping only nodes that exist (no ghosts
 allowed). The order of nodes in the output tree is not important.

 Hint: You may use a transformation to another data structure.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # test_tree |> phantomize |> kill 7 |> kill 0 |> kill 5 |> unphantomize;;
 - : int tree = Node (Node (Node (Empty, 2, Empty), 6, Empty), 11, Empty)
[*----------------------------------------------------------------------------*)

