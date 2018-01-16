(* ================= *)
(* Part 1: functions *)
(* ================= *)

(* 1.1) Write a function that takes a pair of and swaps its components. *)
let swap (x, y) = (y, x)

(* 1.2) Write a function that takes a value x and a pair p, and replaces the second
   component of p by x. *)
let pair_with x (a, _) = (a, x)

(* 1.3) Write a function takes a list of pairs and replaces each second component by 42. *)
let with_42_list l = List.map (pair_with 42) l

(* 1.4) Write a safe-to-use head function, that returns the first element of a
   list, if the list is non-empty. Use the option type. *)
let head_opt = function [] -> None | x::_ -> Some x

(* 1.5) Write a function that takes a function f : 'a -> 'b, some x : 'a and an
   integer n and applies f n-times to x:   f (f ... (f x)) *)
let rec app_n f x n  =
  if n <= 0
  then x
  else app_n f (f x) (n-1)

(* ============================== *)
(* Part 2: data types & recursion *)
(* ============================== *)

(* 2.1) Rose trees are a tree data structure with variable number of branches per
   node. We can represent them as a parametric type 'a rosetree with a single
   constructor. This constructor takes a value of type 'a (the "root") and a
   list of 'a rosetrees (the "forest") as arguments. *)
type 'a rosetree = Rose of 'a * 'a rosetree list

(* 2.2) Define these rose trees:

   t = 1,  t' =  2   ,      t'' =  3
                / \               /| \
               t  t              / |  \
                               -1  t'  0

 *)

let t = Rose (1,[])
let t' = Rose (2, [t;t])
let t'' = Rose (3, [Rose (-1, []); t'; Rose (0, [])])

(* 2.3) Define a projection function that returns the forest of a rose tree. *)
let forest (Rose (_, forest)) = forest

(* 2.4) Write a function that prints all the elements of an integer tree, one per line. *)
let rec print (Rose (root, forest)) =
  let rec iter f = function
    | [] -> ()
    | x :: xs -> f x; iter f xs
  in
  print_endline (string_of_int root); iter print forest

(* 2.5) Write a function that computes the depth of a rose tree, i.e. the longest
   path from the root to a leaf. *)
let rec depth (Rose (_, forest)) =
  List.map depth forest |> List.fold_left max 0 |> (+) 1

(* 2.6) Write a function deep_tree n that generates trees with a depth of n. *)
let deep_tree n =
  let rec aux acc n =
    if n > 0
    then aux (Rose (n, [acc])) (n-1)
    else acc
  in aux (Rose (n, [])) (n-1)


(* 2.7) Write a function that takes a function (f : 'b -> 'a -> 'b) and a starting
   value (acc : 'b) and folds f through a tree (t : 'a rosetree).

   For example,
   fold (fun acc x -> x + acc) 0 t = 1
   fold (fun acc x -> x * acc) 3 t' = 6
   fold (fun acc x -> acc - x) 0 t'' = -6

   For full credit, your function should be tail recursive.

   Remark: Like we observed in the tutorials on the example of List.map,
   functions from the List module are not always tail recursive, so better
   avoid them if you want your function to be tail recursive. *)

(* Naive fold, prefix left-to-right traversal, not tail recursive *)
let rec fold (f : 'a -> 'b -> 'a) b (Rose (root, forest)) =
  List.fold_left (fun acc t -> fold f acc t) (f b root) forest

(* Naive fold, with a strange left-to-right, post-fix traversal order *)
let rec fold' (f : 'a -> 'b -> 'a) b (Rose (root, forest)) =
  f (List.fold_left (fun acc t -> fold' f acc t) b forest) root

(* Tail recursive fold, prefix left-to-right *)
let fold_tlrec (f : 'a -> 'b -> 'a) (b : 'a) (t : 'b rosetree) =
  let rec aux (acc : 'a) (rests : 'b rosetree list list) =
    match rests with
    | [] -> acc
    | ts :: rests ->
       (match ts with
        | [] -> aux acc rests
        | (Rose (root, forest)) :: ts ->
           aux (f acc root) (forest :: ts :: rests))
  in aux b [[t]]
