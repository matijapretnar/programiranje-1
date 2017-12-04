(* ================= *)
(* Part 1: functions *)
(* ================= *)

(* Write a function that takes three numbers and multiplies them. *)
let mult3 x y z = ()

(* Write a function f(x) that computes x^3 + k. *)
let affine_cube = ()


(* Write a function that adds computes x^3 + 2 for all elements x of a list. *)
(* let cube_plus_two_lst ... *)

(* Write a function that safely returns the last element of a list, if the list
   is non-empty. Use the option type. *)


(* Write a function f(n) that computes the n-th Fibonacci number  *)


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

(* Define a function that tests if a tree is a leaf, i.e. it has no subtrees. *)
let leaf_p = ()

(* Write a function that tests if a rose tree of integers contains only
   positive integers. *)


(* Write a function that computes the maximal width of a rose tree t, i.e. the
   maximal length of the a forest occurring in t. *)


(* Write a function deep_tree n that generates trees with a depth of n. *)


(* Write a function to_list that transforms a rose tree into a list.

   For example,
   to_list t = [1],  to_list t' = [2; 1; 1]  (or some permutation of [2;1;1]),
   to_list t'' = [3; -1; 2; 1; 1; 0]

   For full credit, your function should be tail recursive.

   Remark: Like we observed in the tutorials on the example of List.map,
   functions from the List module are not always tail recursive, so better
   avoid them if you want your function to be tail recursive. *)
