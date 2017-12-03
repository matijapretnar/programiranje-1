(* ===== Exercise 4: Search Tree  ===== *)

type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

(* Standard test example.
          5
         / \
        2   7
       /   / \
      0   6   11
   *)

let leaf x = Node(Empty, x, Empty) (* Function to shorten tree creation. *)

let test_tree = Node( Node(leaf 0, 2, Empty), 5, Node(leaf 6, 7, leaf 11))

(* The function "mirror t" returns a mirrored tree. On the test_tree example:
       5
      / \
     7   2
    / \   \
   11  6   0
   ----------
   # mirror test_tree ;;
   - : int tree =
   Node (Node (Node (Empty, 11, Empty), 7, Node (Empty, 6, Empty)), 5,
   Node (Empty, 2, Node (Empty, 0, Empty)))
   ---------- *)

let rec mirror t =
	match t with
	  |Empty -> Empty
	  |Node(l, x, r) -> Node(mirror r, x, mirror l)

(* The function "height t" returns the height (or depth) of the tree and
   the function "size t" returns the number of all tree nodes.
   ----------
   # height test_tree;;
   - : int = 3
   # size test_tree;;
   - : int = 6
   ---------- *)

let rec height t =
	if t = Empty then 0 else
	  let Node(l, _, r) = t in
	    (max (height l) (height r)) + 1

let rec size t = 
	if t = Empty then 0 else
	  let Node(l, _, r) = t in
	    size l + size r + 1

(* The function "follow directions t" [direction list -> 'a tree -> 'a option]
   takes as input a list of directions for traversing the tree. Because the
   directions might not lead to a node we use the option type.
   ----------
   # follow [Right;Left] test_tree;;
   - : int option = Some 6
   # follow [Right;Left;Right;Right] test_tree;;
   - : int option = None
   ---------- *)

type direction = Left | Right

let rec follow directions t =
	match t with
	  |Empty -> None
	  |Node(l, x, r) -> if directions = [] then Some x else
		let step::directions = directions in
		  if step = Left then (follow directions l) else (follow directions r)

let rec follow2 directions t =
  match directions, t with
  | _, Empty -> None
  | [], Node(_,x,_) -> Some x
  | Left::tl, Node(l,_,_) -> follow2 tl l
  | Right::tl, Node(_,_,r) -> follow2 tl r
		  
(* The function "prune directions t" [direction list -> 'a tree -> 'a tree option]
   finds the node determined by the directions and deltes the subtree rooted
   in the selected node.
   Warning: using Some Node(l, x, r) causes an error in OCaml because it is
   read as (Some Node)(l, x, r). Use appropriate paranthesis.
   ----------
   # prune [Right] test_tree;;
   - : int tree option =
   Some (Node (Node (Node (Empty, 0, Empty), 2, Empty), 5, Empty))
   ---------- *)

let rec prune directions t =
	if t = Empty then None else
	let Node(l, x, r) = t in
	  match directions with
	    |[] -> Some Empty
	    |step::dir -> match step with
			|Left -> let check = prune dir l in if check = None then None else let Some (new_l) = check in Some (Node(new_l, x, r))
			|Right -> let check = prune dir r in if check = None then None else let Some (new_r) = check in Some (Node(l, x, new_r))

(* The function "map_tree f t"  [('a -> 'b) -> 'a tree -> 'b tree] maps the
   nodes of the tree t with the function f.
   ----------
   # map_tree ((<)3) test_tree;;
   - : bool tree =
   Node (Node (Node (Empty, false, Empty), false, Empty), true,
   Node (Node (Empty, true, Empty), true, Node (Empty, true, Empty)))
   ---------- *)

let rec map_tree f t = 
	match t with
	  |Empty -> Empty
	  |Node(l, x, r) -> Node(map_tree f l, f x, map_tree f r)

(* The function "list_of_tree t" ['a tree -> 'a list] maps the data of the tree
   into a list. If the tree is a binary search tree the returned list should be
   ordered.
   ----------
   # list_of_tree test_tree;;
   - : int list = [0; 2; 5; 6; 7; 11]
   ---------- *)

let rec list_of_tree t =
	match t with
	  |Empty -> []
	  |Node(l, x, r) -> (list_of_tree l) @ [x] @ (list_of_tree r)

(* The function "is_bst t" ['a tree -> bool] checks wheter a tree is a
   binary search tree (BST). Assume that a tree has no repetitions (a tree
   Node(leaf 1, 1, leaf 2) is not allowed).
   An empty tree is a BST.
   ----------
   # is_bst test_tree;;
   - : bool = true
   # test_tree |> mirror |> is_bst;;
   - : bool = false
   ---------- *)

let rec is_bst t = 
	match t with
	  |Empty -> true
	  |Node(l, x, r) -> 
	    (match l with
	      |Empty -> (match r with
		    |Empty -> true
		    |Node(_, y, _) -> x < y && is_bst r)
		  |Node(_, z, _) -> (match r with
		    |Empty -> z < x && is_bst l
		    |Node(_, y, _) -> z < x && x < y && is_bst l && is_bst r)
		)

(*------------------------------------------------------------------------------
   In the remaining exercises the variable name bst assumes a BST input.
  ----------------------------------------------------------------------------*)

(* The function "insert x bst" ['a -> 'a tree -> 'a tree] inserts the element x
   into the bst. The function "member x bst" ['a -> 'a tree -> bool] checks
   wheter an element is present in the bst.
   ----------
   # insert 2 (leaf 4);;
   - : int tree = Node (Node (Empty, 2, Empty), 4, Empty)
   # member 3 test_tree;;
   - : bool = false
   ---------- *)

let rec insert x bst =
	match bst with
	  |Empty -> Node(Empty, x, Empty)
	  |Node(l, y, r) ->
	    if x = y then bst else
		if x > y then Node(l, y, insert x r) else
		Node(insert x l, y, r)

let rec member x bst =
	match bst with
	  |Empty -> false
	  |Node(l, y, r) ->
	    if x = y then true else
		if x > y then member x r else
		member x l

(* Write the function "member2", where you do not assume a BST structure.
   Think about the differences of time complexity for "member" and "member2"
   if you assume that the tree has n nodes and a depth of log(n). *)

let rec member2 x t =
	match t with
	  |Empty -> false
	  |Node(l, y, r) -> member2 x l || x = y || member2 x r

(* The function "bst_of_list l" ['a list -> 'a tree] forms a bst from a list.
   Hint: in lectures the professor first defined the function "insert".
   ----------
   # [11;6;7;0;2;5] |> bst_of_list = test_tree;;
   - : bool = true
   ---------- *)

let bst_of_list l = 
	let rec list_to_bst l bst =
	  match l with
	    |[] -> bst
		|hd::tl -> list_to_bst tl (insert hd bst)
	in
	list_to_bst l Empty

(* Create a function "tree_sort l" ['a list -> 'a list] that sorts the list l
   by combining previously defining functions.
   ----------
   # tree_sort ["a";"c";"f";"b";"e";"d"];;
   - : string list = ["a"; "b"; "c"; "d"; "e"; "f"]
   ---------- *)

let tree_sort l = l |> bst_of_list |> list_of_tree

(* The function "succ bst" ['a tree -> 'a option] returns the succesor of the
   tree root, if it exists. For instance, for bst = Node(l, x, r) it returns
   the smallest element larger than x.
   The function "pred bst" ['a tree -> 'a option] symetrically returns the
   largest elements smaller tha the root, if it exists.
   ----------
   # succ test_tree;;
   - : int option = Some 6
   # pred (Node(Empty, 5, leaf 7));;
   - : int option = None
   ---------- *)

let succ bst = if bst = Empty then None else
	let rec smoll t =
	  match t with
		|Node(Empty, x, _) -> Some x
		|Node(l, _, _) -> smoll l
	in 
	  let Node(l, x, r) = bst in
	    if r = Empty then None else smoll r

let pred bst = bst |> mirror |> succ

(* In lectures you mentioned multiple different algorithms for deletion.
   One uses "succ" and the other "pred".
   Write a function "delete x bst" ['a tree -> 'a tree], that deletes the
   elements x, should it exist in the tree. For practice, you can implement
   both different algorithms.
   Hint: Think about dealing with cases
      Node(Empty, x, Empty)
      Node(Empty, x, r)
      Node(l, x, Empty)
      Node(l, x, r)
   ----------
   [Za delete definiran s funkcijo "succ".]
   # delete 7 test_tree;;
   - : int tree =
   Node (Node (Node (Empty, 0, Empty), 2, Empty), 5,
   Node (Node (Empty, 6, Empty), 11, Empty))
   ---------- *)

let rec delete x bst = 
	match bst with
	  |Node(Empty, y, Empty) -> 
	    if x = y then Empty 
		else bst
      |Node(Empty, y, r) -> 
	    if x > y then Node(Empty, y, delete x r) 
		else if x = y then r
		else bst
      |Node(l, y, Empty) -> 
	    if x > y then bst
		else if x = y then l
		else Node(delete x l, y, Empty)
      |Node(l, y, r) -> 
	    if x > y then Node(l, y, delete x r)
		else if x = y then 
		  let new_y = succ bst in 
		    if new_y = None then failwith "fuckme"
		    else let Some new_y = new_y in
		      Node(l, new_y, delete new_y r)
		else Node(delete x l, y, r)
	  

(* An additional option is to change the type of the tree. Define a new tree
   type that additionally contains an information about its state, that can be
   either Exists or Ghost if it is not present in the tree anymore. *)

type state = Exists | Ghost

type 'a phantom_tree =
	|P_Empty
	|P_Node of 'a phantom_tree * 'a * 'a phantom_tree * state

(* The function "phantomize t" ['a tree -> 'a phantom_tree], that maps a regular
   tree into a phantom tree.
   Then write the function "kill x pt" ['a -> 'a phantom_tree -> 'a phantom_tree]
   that removes an element from the tree by changing it's state to Ghost.
   ----------
   # phantomize test_tree;;
   - : int phantom_tree =
   P_Node (P_Node (P_Node (P_Empty, 0, P_Empty, Exists), 2, P_Empty, Exists), 5,
   P_Node (P_Node (P_Empty, 6, P_Empty, Exists), 7,
   P_Node (P_Empty, 11, P_Empty, Exists), Exists),
   Exists)

   # bst_of_list [3;4;2] |> phantomize |> kill 3 |> kill 6;;
   - : int phantom_tree =
   P_Node (P_Empty, 2,
   P_Node (P_Node (P_Empty, 3, P_Empty, Ghost), 4, P_Empty, Exists), Exists)
   ---------- *)

let rec phantomize t = 
	match t with
	  |Empty -> P_Empty
	  |Node(l, x, r) -> P_Node(phantomize l, x, phantomize r, Exists)

let rec kill x pt =
	match pt with
	  |P_Empty -> P_Empty
	  |P_Node(l, y, r, spook_factor) ->
	    if x = y then P_Node(l, y, r, Ghost) else
		if x > y then P_Node(l, y, kill x r, spook_factor) else
		P_Node(kill x l, y, r, spook_factor)

(* The function "unphantomize pt" ['a phantom_tree -> 'a tree] that maps a
   phantom tree to a regular tree, that only contains existing states.
   The order of elements in the tree is not important.
   Hint: you may use a transition to another data structure in between.
   ----------
   # test_tree |> phantomize |> kill 7 |> kill 0 |> kill 5 |> unphantomize;;
   - : int tree = Node (Node (Node (Empty, 2, Empty), 6, Empty), 11, Empty)
   ---------- *)

let unphantomize pt =
	let rec the_listening p_tree =
	  match p_tree with
	    |P_Empty -> []
	    |P_Node(l, x, r, spoop) -> 
		  if spoop = Exists then (the_listening l) @ [x] @ (the_listening r) 
		  else (the_listening l) @ (the_listening r)
	in
	  the_listening pt |> bst_of_list
	  
(*========== Ideas for additional exercises ==========*)
(*
1.) Change the functions "insert" and "member" to work with phantom trees.
2.) Create a more general decision tree, that additionally contains a
    decision function in every node that decides in which subtree an element
    belongs. [Such trees are used in machine learning, where comparing vectors
    only uses a chosen component of the vector.]
3.) By using exercise 2 you can define dictionaries based on trees, where a pair
    (data, key) is saved according to key value. Define a new type of nodes
    that keeps the value of data and an additional key and define some of the
    function "member", "insert", "dict_of_list", ... so that they coincide with
    the expected behaviour on dictionaries.
*)