(* ========== Exercise 4: Search trees  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 In Ocaml working with trees is fairly simple. We construct a new type for
 trees, which are either empty or they contain some data and two (possibly
 empty) subtrees. We assume no further structure of the trees.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type 'a tree = Empty | Node of 'a tree * 'a * 'a tree

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

let leaf x = Node (Empty, x, Empty)

let test_tree = Node (Node (leaf 0, 2, Empty), 5, Node (leaf 6, 7, leaf 11))

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
  | Node (l, x, r) -> Node (mirror r, x, mirror l)

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
  | Node (l, _, r) -> 1 + max (height l) (height r)

let rec size = function
  | Empty -> 0
  | Node (l, _, r) -> 1 + (size l) + (size r)

(*----------------------------------------------------------------------------*]
 The function [map_tree f tree] maps the tree into a new tree with nodes that
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
  | Node (l, x, r) -> let lr = list_of_tree r
                      and ll = list_of_tree l in
                      ll @ [x] @ lr

(* An example of a tail recursive function on trees. If [verbose] is [true],
   print a trace of the algorithm as it traverses the tree. *)
let list_of_tree_tailrec verbose tree =
  let printer = if verbose then print_endline else ignore
  and string_of_int_list lst =
    (List.fold_left (fun acc x -> acc ^ (string_of_int x) ^ "; ") "[" lst) ^ "]"
  in
  (* The final result goes into the list [acc]. The recursion will descend
     into the right subtree because lists allow us naturally to add things in
     front, where the left subtree will go. We represent the work that still
     needs to be done once the right subtree is taken care of by [k], which we
     call the "continuation". If we see an empty tree, no further work is to be
     done and [k] can do what it needs to do with [acc]. If we see a node, the
     continuation needs to be updated, that is to say we build a new
     continuation [k'], which adds [x] in front of the accumulator list, and
     then collects the left sub-tree. *)
  let rec collect acc k = function
    | Empty -> printer "Empty"; k acc
    | Node (l, x, r) ->
        let k' acc =
          printer ("Resuming the continuation of " ^ (string_of_int x) ^ ", adding it to acc") ;
          collect (x :: acc) k l
        in
        printer ("Visiting node " ^ (string_of_int x) ^ "\n\tacc: " ^ (string_of_int_list acc)) ;
        collect acc k' r
  in
  collect [] (fun x -> x) tree

(* A similar way to write the same function is by explicitly keeping a list [k]
   of the work that's still to be done, that is to say the further left
   subtrees to visit, and elements we found on their nodes. [continue] then
   chews through that list. *)
let list_of_tree_explicit tree =
  let rec aux acc k = function
    | Empty -> continue acc k
    | Node (l, x, r) -> aux acc ((x, l) :: k) r
  and continue acc k = match k with
    | [] -> acc
    | (x, l) :: k -> aux (x :: acc) k l
  in aux [] [] tree

(* To test, we can watch [list_of_tree] die with a stack overflow on a big tree, while
   the tail-recursive version does fine. *)
let big_tree =
    let rec tree_of_list lst =
      let rec aux acc = function [] -> acc | h :: t -> aux (Node (Empty, h, acc)) t
      in aux Empty lst
    in
    tree_of_list (List.init 1_000_000 (fun x -> x))

let print_tree tree =
  let rec str = function
    | Empty -> "Empty"
    | Node (l, x, r) -> Format.sprintf "Node (%s, %d, %s)" (str l) x (str r)
  in print_endline (str tree)

(* Finally, here's a tail recursive implementation of [mirror]. It works
   similarly to [list_of_tree_tailrec], by wrapping the work that the naive
   version of [mirror] has left to do after a recursive call in a continuation
   which gets the result of this recursion as an argument. *)
let mirror_tailrec tree =
  let rec aux k = function
    | Empty -> k Empty
    | Node (l, x, r) ->
      let k_l l_mirrored =
        let k_r r_mirrored =
          k (Node (r_mirrored, x, l_mirrored))
        in
        aux k_r r
      in aux k_l l
  in aux (fun x -> x) tree


(*----------------------------------------------------------------------------*]
 The function [is_bst] checks whether a tree is a binary search tree (BST).
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
  | Node (l, y, r) when x = y -> Node (l, y, r)
  | Node (l, y, r) when x < y -> Node (insert x l, y, r)
  | Node (l, y, r) (* when x > y *) -> Node (l, y, insert x r)

let rec member x = function
  | Empty -> false
  | Node (l, y, r) when x = y -> true
  | Node (l, y, r) when x < y -> member x l
  | Node (l, y, r) (* when x > y *) -> member x r

(*----------------------------------------------------------------------------*]
 The function [member2] does not assume that the tree is a bst.

 Note: Think about the differences of time complexity for [member] and
 [member2] assuming an input tree with n nodes and depth of log(n).
[*----------------------------------------------------------------------------*)

let rec member2 x = function
  | Empty -> false
  | Node(l, y, r) -> x = y || (member2 x l) || (member2 x r)

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
    | Node (Empty, x, _) -> Some x
    | Node (l, _, _) -> minimal l
  in
  match bst with
  | Empty -> None
  | Node (_, _, r) -> minimal r

let pred bst =
  let rec maximal = function
    | Empty -> None
    | Node (_, x, Empty) -> Some x
    | Node (_, _, r) -> maximal r
  in
  match bst with
  | Empty -> None
  | Node (l, _, _) -> maximal l

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

let rec delete x = function
  | Empty -> Empty
  | Node (l, y, r) when x > y -> Node (l, y, delete x r)
  | Node (l, y, r) when x < y -> Node (delete x l, y, r)
  | Node (l, y, r) as bst -> (
      (*We need to delete the root.*)
      match succ bst with
      | None -> l (*Only happens when [r] is [Empty].*)
      | Some s ->
        let clean_r = delete s r in
        Node (l, s, clean_r))

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 DICTIONARIES

 Using BST we can (sufficiently) implement dictionaries. While in practice we
 use the even more efficient hash tables, we assume that our dictionaries [dict]
 are implemented using BST. Every node includes a key and a value and the three
 has the BST structure according to the value of node keys. Because the
 dictionary requires a type for keys and a type for values, we parametrize the
 type as [('key, 'value) dict].
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type ('key, 'value) dict = ('key * 'value) tree

(*----------------------------------------------------------------------------*]
 Write the test case [test_dict]:
      "b":1
      /    \
  "a":0  "d":2
         /
     "c":-2
[*----------------------------------------------------------------------------*)

let test_dict
  : (string, int) dict
  = Node (leaf ("a", 0), ("b", 1), Node (leaf ("c", -2), ("d", 2), Empty))

(*----------------------------------------------------------------------------*]
 The function [dict_get key dict] returns the value with the given key. Because
 the  dictionary might not include the given key, we return an [option].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_get "banana" test_dict;;
 - : 'a option = None
 # dict_get "c" test_dict;;
 - : int option = Some (-2)
[*----------------------------------------------------------------------------*)

let rec dict_get k = function
  | Empty -> None
  | Node (l, (k', v), r) ->
    if k = k' then
      Some v
    else if k < k' then
      dict_get k l
    else
      dict_get k r

(*----------------------------------------------------------------------------*]
 The function [print_dict] accepts a dictionary with key of type [string] and
 values of type [int] and prints (in the correct order) lines containing
 "key : value" for all nodes of the dictionary. Hint: Use functions
 [print_string] and [print_int]. Strings are concatenated with the operator [^].
 Observe how using those functions fixes the type parameters of our function, as
 opposed to [dict_get].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # print_dict test_dict;;
 a : 0
 b : 1
 c : -2
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

let rec print_dict = function
  | Empty -> ()
  | Node (d_l, (k, v), d_r) -> (
      print_dict d_l;
      print_string (k ^ " : "); print_int v; print_newline ();
      print_dict d_r)

(*----------------------------------------------------------------------------*]
 The function [dict_insert key value dict] inserts [value] into [dict] under the
 given [key]. If a key already exists, it replaces the value.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_insert "1" 14 test_dict |> print_dict;;
 1 : 14
 a : 0
 b : 1
 c : -2
 d : 2
 - : unit = ()
 # dict_insert "c" 14 test_dict |> print_dict;;
 a : 0
 b : 1
 c : 14
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)


let rec dict_insert k v = function
  | Empty -> leaf (k, v)
  | Node (l, (k', _), r) when k = k' -> Node (l, (k, v), r)
  | Node (l, (k', v'), r) when k < k' -> Node (dict_insert k v l, (k', v'), r)
  | Node (l, (k', v'), r) (* when k > k' *) -> Node (l, (k', v'), dict_insert k v r)
