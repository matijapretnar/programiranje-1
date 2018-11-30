(* ========== Exercise 4: Search trees  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 In Ocaml working with trees is fairly simple. We construct a new type for
 trees, which are either empty or they contain some data and two (possibly
 empty) subtrees. We assume no further structure of the trees.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)


(*----------------------------------------------------------------------------*]
 We define a test case for simpler testing of functions. The test case
 represents the tree below. The function [leaf], which constructs a leaf from a
 given data, is used for simpler notation.
          5
         / \
        2   7
       /   / \
      0   6   11
[*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*]
 The function [mirror] returns a mirrored tree. When applied to our test tree
 it returns
          5
         / \
        7   2
       / \   \
      11  6   0
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # mirror test_tree ;;
 - : int tree =
 Node (Node (Node (Empty, 11, Empty), 7, Node (Empty, 6, Empty)), 5,
 Node (Empty, 2, Node (Empty, 0, Empty)))
[*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*]
 The function [height] returns the height (or depth) of the tree and the
 function [size] returns the number of nodes in the tree.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # height test_tree;;
 - : int = 3
 # size test_tree;;
 - : int = 6
[*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*]
 The function [map_tree f tree] maps the tree into a new tree with nodes that
 contain data from [tree] mapped with the function [f].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # map_tree ((<)3) test_tree;;
 - : bool tree =
 Node (Node (Node (Empty, false, Empty), false, Empty), true,
 Node (Node (Empty, true, Empty), true, Node (Empty, true, Empty)))
[*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*]
 The function [list_of_tree] returns the list of all elements in the tree. If
 the tree is a binary search tree the returned list should be ordered.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # list_of_tree test_tree;;
 - : int list = [0; 2; 5; 6; 7; 11]
[*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*]
 The function [is_bst] checks wheter a tree is a binary search tree (BST). 
 Assume that the input tree has no repetitions of elements. An empty tree is a
 BST.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # is_bst test_tree;;
 - : bool = true
 # test_tree |> mirror |> is_bst;;
 - : bool = false
[*----------------------------------------------------------------------------*)


(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 In the remaining exercises we assume that all trees are binary search trees.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 The function [insert] correctly inserts an element into the bst. The function
 [member] checks wheter an element is present in the bst.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 2 (leaf 4);;
 - : int tree = Node (Node (Empty, 2, Empty), 4, Empty)
 # member 3 test_tree;;
 - : bool = false
[*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*]
 The function [member2] does not assume that the tree is a bst.
 
 Note: Think about the differences of time complexity for [member] and 
 [member2] assuming an input tree with n nodes and depth of log(n). 
[*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*]
 The function [succ] returns the successor of the root of the given tree, if
 it exists. For the tree [bst = Node(l, x, r)] it returns the least element of
 [bst] that is larger than [x].
 The function [pred] symetrically returns the largest element smaller than the
 root, if it exists.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # succ test_tree;;
 - : int option = Some 6
 # pred (Node(Empty, 5, leaf 7));;
 - : int option = None
[*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*]
 In lectures you two different approaches to deletion, using either [succ] or
 [pred]. The function [delete x bst] deletes the element [x] from the tree. If
 it does not exist, it does not change the tree. For practice you can implement
 both versions of the algorithm.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # (*<< For [delete] defined with [succ]. >>*)
 # delete 7 test_tree;;
 - : int tree =
 Node (Node (Node (Empty, 0, Empty), 2, Empty), 5,
 Node (Node (Empty, 6, Empty), 11, Empty))
[*----------------------------------------------------------------------------*)


(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 DICTIONARIES

 Using BST we can (sufficiently) implement dictionaries. While in practice we
 use the even more efficient hash tables, we assume that our dictionaries [dict]
 are implemented using BST. Every node includes a key and a value and the three
 has the BST structure according to the value of node keys. Because the
 dictionary requires a type for keys and a type for values, we parametrize the
 type as [('key, 'value) dict].
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)


(*----------------------------------------------------------------------------*]
 Write the test case [test_dict]:
      "b":1
      /    \
  "a":0  "d":2
         /
     "c":-2
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*]
 The function [dict_get key dict] returns the value with the given key. Because
 the  dictionary might not include the given key, we return an [option].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_get "banana" test_dict;;
 - : 'a option = None
 # dict_get "c" test_dict;;
 - : int option = Some (-2)
[*----------------------------------------------------------------------------*)

      
(*----------------------------------------------------------------------------*]
 The function [print_dict] accepts a dictionary with key of type [string] and
 values of type [int] and prints (in the correct order) lines containing 
 "key : value" for all nodes of the dictionary. Hint: Use functions
 [print_string] and [print_int]. Strings are concatenated with the operator [^].
 Observe how using those functions fixes the type parameters of our function, as
 opposed to [dict_get]. 
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # print_dict test_dict;;
 a : 1
 b : 1
 c : -2
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*]
 The function [dict_insert key value dict] inserts [value] into [dict] under the
 given [key]. If a key already exists, it replaces the value.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_insert "1" 14 test_dict |> print_dict;;
 1 : 14
 a : 1
 b : 1
 c : -2
 d : 2
 - : unit = ()
 # dict_insert "c" 14 test_dict |> print_dict;;
 a : 1
 b : 1
 c : 14
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

