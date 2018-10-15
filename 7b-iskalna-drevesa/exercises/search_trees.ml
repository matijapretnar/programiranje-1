(* ========== Exercise 4: Search trees  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 In Ocaml working with trees is fairly simple. We construct a new type for
 trees, which are either empty or they contain some data and two (possibly
 empty) subtrees. We assume no further structure of the trees.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)
type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

(*----------------------------------------------------------------------------*]
 We define a test case for simpler testing of functions. The test case
 represents the tree below. The function [leaf] which constructs a leaf from a
 given data is used for simpler notation.
          5
         / \
        2   7
       /   / \
      0   6   11
[*----------------------------------------------------------------------------*)

let leaf x = Node(Empty, x, Empty) 

let test_tree = Node( Node(leaf 0, 2, Empty), 5, Node(leaf 6, 7, leaf 11))

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

let rec mirror = function
  | Empty -> Empty
  | Node(l, x, r) -> Node(mirror r, x, mirror l)

(*----------------------------------------------------------------------------*]
 The function [height] returns the height (or depth) of the tree and the
 function [size] returns the number of nodes in the tree.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # height test_tree;;
 - : int = 3
 # size test_tree;;
 - : int = 6
[*----------------------------------------------------------------------------*)

let rec height = function
  | Empty -> 0
  | Node(l, _, r) -> 1 + max (height l) (height r)

let rec size = function
  | Empty -> 0
  | Node(l, _, r) -> 1 + (size l) + (size r)

(*----------------------------------------------------------------------------*]
 The function [follow directions tree] of type [direction list -> 'a tree -> 
 'a option] accepts a list of directions for traversing the tree and returns the
 data in the node at the end of the traversal. Because the directions might not
 lead to an actual node in the tree, the result is returned as an [option] type.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # follow [Right; Left] test_tree;;
 - : int option = Some 6
 # follow [Right; Left; Right; Right] test_tree;;
 - : int option = None
[*----------------------------------------------------------------------------*)

type direction = Left | Right

let rec follow directions = function
  | Empty -> None
  | Node(l, x, r) ->
    (match directions with
     | [] -> Some x
     | Left :: tl -> follow tl l
     | Right :: tl -> follow tl r)

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

let rec prune directions tree = 
  match directions, tree with
  | [], _ -> Some Empty
  | _, Empty -> None
  | Left :: tl, Node(l, x, r) ->
    (match prune tl l with
     | None -> None
     | Some new_l -> Some (Node(new_l, x, r)))
  | Right :: tl, Node(l, x, r) ->
    (match prune tl r with
     |None -> None
     | Some new_l -> Some (Node(new_l, x, r)))

(*----------------------------------------------------------------------------*]
 The function [map_tree f tree] maps [tree] into a new tree with nodes that
 contain data from [tree] mapped with the function [f].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # map_tree ((<)3) test_tree;;
 - : bool tree =
 Node (Node (Node (Empty, false, Empty), false, Empty), true,
 Node (Node (Empty, true, Empty), true, Node (Empty, true, Empty)))
[*----------------------------------------------------------------------------*)

let rec map_tree f = function
  | Empty -> Empty
  | Node(l, x, r) -> Node(map_tree f l, f x, map_tree f r)

(*----------------------------------------------------------------------------*]
 The function [list_of_tree] returns the list of all elements in the tree. If
 the tree is a binary search tree the returned list should be ordered.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # list_of_tree test_tree;;
 - : int list = [0; 2; 5; 6; 7; 11]
[*----------------------------------------------------------------------------*)

let rec list_of_tree = function
  | Empty -> []
  | Node(l, x, r) -> (list_of_tree l) @ [x] @ (list_of_tree r)

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

let rec is_bst t =
  let rec list_is_ordered = function
    | [] | _ :: [] -> true
    | x :: y :: tl -> if x <= y then list_is_ordered (y :: tl) else false
  in t |> list_of_tree |> list_is_ordered

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

let rec insert x = function
  | Empty -> leaf x
  | Node(l, y, r) when x = y -> Node(l, y, r)
  | Node(l, y, r) when x < y -> Node(insert x l, y, r)
  | Node(l, y, r) when x > y -> Node(l, y, insert x r)

let rec member x = function
  | Empty -> false
  | Node(l, y, r) when x = y -> true
  | Node(l, y, r) when x < y -> member x l
  | Node(l, y, r) when x > y -> member x r

(*----------------------------------------------------------------------------*]
 The function [member2] does not assume that the tree is a bst.
 
 Note: Think about the differences of time complexity for [member] and 
       [member2] assuming an input tree with n nodes and depth of log(n). 
[*----------------------------------------------------------------------------*)

let rec member2 x = function
  | Empty -> false
  | Node(l, y, r) -> x = y || (member2 x l) || (member2 x r)

(*----------------------------------------------------------------------------*]
 The function [bst_of_list] constructs a bst out of the elements of a list.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # [11; 6; 7; 0; 2; 5] |> bst_of_list |> is_bst;;
 - : bool = true
[*----------------------------------------------------------------------------*)

let bst_of_list list = List.fold_right insert list Empty

(*----------------------------------------------------------------------------*]
 The function [tree_sort] sorts a list by transforming it to a tree and back.

 Note: Please do not actually use this in your code.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # tree_sort ["a"; "c"; "f"; "b"; "e"; "d"];;
 - : string list = ["a"; "b"; "c"; "d"; "e"; "f"]
[*----------------------------------------------------------------------------*)

let tree_sort list = list |> bst_of_list |> list_of_tree

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

let succ bst =
  let rec minimal = function
    | Empty -> None
    | Node(Empty, x, _) -> Some x
    | Node(l, _, _) -> minimal l
  in
  match bst with
  | Empty -> None
  | Node(_, _, r) -> minimal r

let pred bst =
  let rec maximal = function
    | Empty -> None
    | Node(_, x, Empty) -> Some x
    | Node(_, _, r) -> maximal r
  in
  match bst with
  | Empty -> None
  | Node(l, _, _) -> maximal l

(*----------------------------------------------------------------------------*]
 In lectures you two different approaches to deletion, using either [succ] or
 [pred]. The function [delete x bst] deletes the element [x] from the tree. If
 it does not exist, it does not change the tree. For practice you can implement
 both versions of the algorithm.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   # (*<< Za [delete] definiran s funkcijo [succ]. >>*)
   # delete 7 test_tree;;
   - : int tree =
   Node (Node (Node (Empty, 0, Empty), 2, Empty), 5,
   Node (Node (Empty, 6, Empty), 11, Empty))
[*----------------------------------------------------------------------------*)

let rec delete x = function
  | Empty -> Empty
  | Node(l, y, r) when x > y -> Node(l, y, delete x r)
  | Node(l, y, r) when x < y -> Node(delete x l, y, r)
  | Node(l, y, r) as bst -> (
      (*We need to delete the root.*)
      match succ bst with
      | None -> l (*Only happens when [r] is [Empty].*)
      | Some s ->
        let clean_r = delete s r in
        Node(l, s, clean_r))

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 An additional approach to deletion is to modify the type of the tree. Define 
 a new type of tree where nodes additionaly contain information about its state,
 which can either [Exist] or be a [Ghost] if the node is only used for
 searching but is not considered present.
 We assume that all trees are BST.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type state = Exists | Ghost

type 'a phantom_tree =
  | P_Empty
  | P_Node of 'a phantom_tree * 'a * 'a phantom_tree * state

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

let rec phantomize = function
  | Empty -> P_Empty
  | Node(l, x, r) ->
    let p_l = phantomize l in
    let p_r = phantomize r in
    P_Node(p_l, x, p_r, Exists)

let rec kill x = function
  | P_Empty -> P_Empty
  | P_Node(p_l, y, p_r, s) when x = y -> P_Node(p_l, y, p_r, Ghost)
  | P_Node(p_l, y, p_r, s) when x < y -> P_Node(kill x p_l, y, p_r, s)
  | P_Node(p_l, y, p_r, s) -> P_Node(p_l, y, kill x p_r, s)
      

(*----------------------------------------------------------------------------*]
 The function [unphantomize] of type ['a phantom_tree -> 'a tree] maps a
 phantom tree into a regular one, keeping only nodes that exist (no ghosts
 allowed). The order of nodes in the output tree is not important.

 Hint: You may use a transformation to another data structure.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # test_tree |> phantomize |> kill 7 |> kill 0 |> kill 5 |> unphantomize;;
 - : int tree = Node (Node (Node (Empty, 2, Empty), 6, Empty), 11, Empty)
[*----------------------------------------------------------------------------*)

let unphantomize ptree =
  let rec list_of_ptree = function
    | P_Empty -> []
    | P_Node(l, x, r, Ghost) -> (list_of_ptree l) @ (list_of_ptree r)
    | P_Node(l, x, r, Exists) -> (list_of_ptree l) @ [x] @ (list_of_ptree r)
  in
  ptree |> list_of_ptree |> bst_of_list
