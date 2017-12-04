(* ================= *)
(* Part 1: functions *)
(* ================= *)

(* Write a function that takes a pair of and swaps its components. *)
let swap p = ()

(* Write a function that takes a value x and a pair p, and replaces the second
   component of p by x. *)
let pair_with = ()

(* Write a function takes a list of pairs and replaces each second component by 42. *)
(* let with_42_list ... *)

(* Write a safe-to-use head function, that returns the first element of a
   list, if the list is non-empty. Use the option type. *)


(* Write a function that takes a function f : 'a -> 'b, some x : 'a and an
   integer n and applies f n-times to x:   f (f ... (f x)) *)


(* ============================== *)
(* Part 2: data types & recursion *)
(* ============================== *)

(* Rose trees are a tree data structure with variable number of branches per
   node. We can represent them as a parametric type 'a rosetree with a single
   constructor. This constructor takes a value of type 'a (the "root") and a
   list of 'a rosetrees (the "forest") as arguments. *)
type 'a rosetree = TODO

(* Define these rose trees:

   t = 1,  t' =  2   ,      t'' =  3
                / \               /| \
               t  t              / |  \
                               -1  t'  0

 *)

let t = ()
let t' = ()
let t'' = ()

(* Define a projection function that returns the forest of a rose tree. *)
let forest = ()

(* Write a function that prints all the elements of an integer tree, one per line. *)


(* Write a function that computes the depth of a rose tree, i.e. the longest
   path from the root to a leaf. *)


(* Write a function deep_tree n that generates trees with a depth of n. *)


(* Write a function that takes a function (f : 'b -> 'a -> 'b) and a starting
   value (acc : 'b) and folds f through a tree (t : 'a rosetree).

   For example,
   fold (fun acc x -> x + acc) 0 t = 1
   fold (fun acc x -> x * acc) 3 t' = 6
   fold (fun acc x -> acc - x) 0 t'' = -6

   For full credit, your function should be tail recursive.

   Remark: Like we observed in the tutorials on the example of List.map,
   functions from the List module are not always tail recursive, so better
   avoid them if you want your function to be tail recursive. *)
