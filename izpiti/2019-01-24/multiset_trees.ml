(* a *)
(* Define the type of trees modeling multisets. *)
type 'a count_tree = 
  | Node of 'a count_tree * 'a * int * 'a count_tree
  | Empty

(* b *)
(* Write an insert function. *)
let rec insert x = function
  | Empty -> Node (Empty, x, 1, Empty)
  | Node (lt, y, count, rt) when y = x -> Node(lt, y, count + 1, rt)
  | Node (lt, y, count, rt) when y > x -> Node(insert x lt, y, count, rt)
  | Node (lt, y, count, rt) -> Node(lt, y, count, insert x rt)

(* c *)
(* Write to_list and generate two examples. *)
let rec ctree_from_list = List.fold_left (fun ctree x -> insert x ctree) Empty

let test1 = ctree_from_list [3; 3; 3; 3; 1; 2; 2; 4; 4; 4; 4; 4]
let test2 = ctree_from_list [4; 0; 0; 5; 5; 5]

(* d *) 
(* Calculate the size of the multiset, to see if they understand what it
  represents.*) 
let rec size = function
  | Empty -> 0
  | Node (lt, _, count, rt) -> size lt + count + size rt

(* e *)
(* Check if it's monotone (larger elements appear more often). *)
let is_monotone ctree =
  let check_if f = function
    | None -> true
    | Some x -> f x
  in
  let rec count_between low up = function
    | Empty -> true
    | Node(lt, y, count, rt) ->
      let left_ok = count_between low (Some count) lt in
      let right_ok = count_between (Some count) up rt in
      let this_ok = check_if ((<) count) up && check_if((>) count) low in
      this_ok && left_ok && right_ok
  in
  count_between None None ctree

let tlrec_is_monotone ctree =
  (* This is not much harder than the original one. *)
  let check_if f = function
    | None -> true
    | Some x -> f x
  in
  let rec checker queue =
    match queue with
    | [] -> true
    | (low, up, ctree) :: queue -> (
      match ctree with
      | Empty -> checker queue
      | Node(lt, y, count, rt) ->
        let left_ok = (low, (Some count), lt) in
        let right_ok = ((Some count), up, rt) in
        let this_ok = check_if ((<) count) up && check_if((>) count) low in
        if not this_ok then false else checker (left_ok :: right_ok :: queue)
      )
  in
  checker [(None, None, ctree)]

(* Generate a list from the tree, elements must end up ordered and you are not
  allowed to sort. *)
  
let ctree_to_list ctree = 
  let rec generate x n = if n<=0 then [] else x :: generate x (n-1) in
  let rec to_list = function
    | Empty -> []
    | Node (lt, x, c, rt) -> (to_list lt) @ generate x c @ (to_list rt)
  in
  to_list ctree

type 'a action = DoTree of 'a count_tree | Generate of 'a * int 

let rec ctree_to_list ctree =
  let rec add_to_list x n lst =
    if n <= 0 then
      lst
    else
      add_to_list x (n - 1) (x :: lst)
  in
  let rec to_list queue acc =
    match queue with
    | [] -> acc
    | (Generate (x, n)) :: queue -> 
        let new_acc = add_to_list x n acc in
        to_list queue new_acc
    | (DoTree Empty) :: queue -> to_list queue acc
    | (DoTree (Node (lt, x, c, rt))) :: queue ->
        let do_left, do_right = DoTree lt, DoTree rt in
        let gen_x = Generate (x, c) in
        to_list (do_right :: gen_x :: do_left :: queue) acc
  in
  to_list [DoTree ctree] []
