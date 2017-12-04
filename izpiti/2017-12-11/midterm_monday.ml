(* ================= *)
(* Part 1: functions *)
(* ================= *)

(* Write a function that takes three numbers and multiplies them. *)
let mult3 x y z = x * y * z

(* Write a function f(x) that computes x^3 + k. *)
let affine_cube x k = mult3 x x x + k

(* Write a function that adds computes x^3 + 2 for all elements x of a list. *)
let cube_plus_two_lst = List.map (fun x -> affine_cube x 2)

(* Write a function that safely returns the last element of a list, if the list
   is non-empty. Use the option type. *)
let rec last_opt = function [] -> None | [x] -> Some x | _ :: xs -> last_opt xs

(* Write a function f(n) that computes the n-th Fibonacci number  *)
let rec fib n =
  if n <= 1
  then 1
  else fib (n-1) + fib (n-2)

(* ============================== *)
(* Part 2: data types & recursion *)
(* ============================== *)

(* Rose trees are a tree data structure with variable number of branches per
   node. We can represent them as a parametric type 'a rosetree with a single
   constructor. This constructor takes a value of type 'a (the "root") and a
   list of 'a rosetrees (the "forest") as arguments. *)
type 'a rosetree = Rose of 'a * 'a rosetree list

(* Define these rose trees:

   t = 1,  t' =  2   ,      t'' =  3
                / \               /| \
               t  t              / |  \
                               -1  t'  0

 *)

let t = Rose (1,[])
let t' = Rose (2, [t;t])
let t'' = Rose (3, [Rose (-1, []); t'; Rose (0, [])])

(* Define a function that tests if a tree is a leaf, i.e. it has no subtrees. *)
let leaf_p (Rose (_, forest)) = forest = []

(* Write a function that tests if a rose tree of integers contains only
   positive integers. *)
let rec all_positive (Rose (root, forest)) =
  let rec for_all f = function
    | [] -> true
    | x :: xs -> f x && for_all f xs
  in
  root > 0 && for_all all_positive forest

(* Write a function that computes the maximal width of a rose tree t, i.e. the
   maximal length of the a forest occurring in t. *)
let rec max_width (Rose (_, forest)) =
  List.map max_width forest |> List.fold_left max (List.length forest)

(* Write a function deep_tree n that generates trees with a depth of n. *)
let deep_tree n =
  let rec aux acc n =
    if n > 0
    then aux (Rose (n, [acc])) (n-1)
    else acc
  in aux (Rose (n, [])) (n-1)


(* Write a function to_list that transforms a rose tree into a list.

   For example,
   to_list t = [1],  to_list t' = [2; 1; 1]  (or some permutation of [2;1;1]),
   to_list t'' = [3; -1; 2; 1; 1; 0]

   For full credit, your function should be tail recursive.

   Remark: Like we observed in the tutorials on the example of List.map,
   functions from the List module are not always tail recursive, so better
   avoid them if you want your function to be tail recursive. *)

(* naive translation to a list *)
let rec to_list (Rose (root, forest)) =
  root :: (List.map to_list forest |> List.flatten)

(* Pervasives.(@) is not tail recursive, so we roll our own append *)
let app_tlrec xs ys =
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (x :: acc) xs
  in aux ys (aux [] xs)

(* Re-use the list structure of forest to manage the stack *)
let to_list_naive_tlrec t =
  let rec aux acc ts =
    match ts with
    | [] -> acc
    | (Rose (root, forest)) :: ts -> aux (root :: acc) (app_tlrec forest ts)
  in aux [] [t] |> List.rev

(* Explicit stack management *)
let to_list_tlrec (t : 'a rosetree) =
  let rec aux (acc : 'a list) (stack : 'a rosetree list list) =
    match stack with
    | [] -> acc
    | ts :: stack ->
       (match ts with
        | [] -> aux acc stack
        | (Rose (root, forest)) :: ts ->
           aux (root :: acc) (forest :: ts :: stack))
  in aux [] [[t]] |> List.rev
