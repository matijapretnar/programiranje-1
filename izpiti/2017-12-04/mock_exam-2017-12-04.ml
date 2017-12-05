(* ================= *)
(* Part 1: functions *)
(* ================= *)

(* Write a function that takes two numbers and returns their sum. *)
let add x y = ()

(* Write a function f that adds 3 to its argument. *)
let add3 = ()

(* Write a function that adds 5 to all elements of a list. *)
(* let add5_list ... *)

(* Write a function that takes a triple of values and returns the third, e.g.
   given (1, "horse", [None]), return [None]. *)


(* Write a function that takes two functions g, f, and returns their composite
   g o f*)



(* ============================== *)
(* Part 2: data types & recursion *)
(* ============================== *)

(* Rose trees are a tree data structure with variable number of branches per
   node. We can represent them as a parametric type 'a rosetree with a single
   constructor. This constructor takes a value of type 'a (the "root") and a
   list of 'a rosetrees (the "forest") as arguments. *)
type 'a rosetree = TODO

(* Define a projection function that returns the root of a rose tree. *)
let root t = ()

(* Write a function that tests if a rose tree of integers contains a negative
   integer. *)
let rec neg_p t = ()



(* Write a function wide_tree n that generates trees with a forest containing
   n trees. You can choose an arbitrary value for the root of the k-th tree.
   Hint: write a helper function that generates lists of a given length. *)



(* Write a function that computes the size of a tree, i.e. the number of
   nodes.
   For full credit, your function should be tail recursive.

   Remark: Like we observed in the tutorials on the example of List.map,
   functions from the List module are not always tail recursive, so better
   avoid them if you want your function to be tail recursive. *)
