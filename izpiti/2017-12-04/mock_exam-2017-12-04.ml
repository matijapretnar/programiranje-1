(* ================= *)
(* Part 1: functions *)
(* ================= *)

(* Write a function that takes two numbers and returns their sum. *)
let add x y = x + y

(* Write a function f that adds 3 to its argument. *)
let add3 = add 3

(* Write a function that adds 5 to all elements of a list. *)
let add5_lst = List.map (add 5)

(* Write a function that takes a triple of values and returns the third, e.g.
   given (1, "horse", [None]), return [None]. *)
let third (_,_,x) = x

(* Write a function that takes two functions g, f, and returns their composite
   g o f*)
let comp g f : 'a -> 'c = fun x -> g (f x)


(* ============================== *)
(* Part 2: data types & recursion *)
(* ============================== *)

(* Rose trees are a tree data structure with variable number of branches per
   node. We can represent them as a parametric type 'a rosetree with a single
   constructor. This constructor takes a value of type 'a (the "root") and a
   list of 'a rosetrees (the "forest") as arguments. *)
type 'a rosetree = Rose of 'a * 'a rosetree list

(* Define a projection function that returns the root of a rose tree. *)
let root (Rose (root, _)) = root

(* Write a function that tests if a rose tree of integers contains a negative
   integer. *)
let rec neg_p (Rose (root, forest)) =
  root < 0 || List.exists neg_p forest

(* Write a function wide_tree n that generates trees with a forest containing
   n trees. You can choose an arbitrary value for the root of the k-th tree.
   Hint: write a helper function that generates lists of a given length. *)
let wide_tree n =
  let rec fold_nat f acc n =
    if n = 0 then acc else let acc = f acc n in fold_nat f acc (n-1)
  in
  Rose (n+1, fold_nat (fun acc n -> (Rose (n, [])) :: acc) [] n)


(* Write a function that computes the size of a tree, i.e. the number of
   nodes.
   For full credit, your function should be tail recursive.

   Remark: Like we observed in the tutorials on the example of List.map,
   functions from the List module are not always tail recursive, so better
   avoid them if you want your function to be tail recursive. *)

let size_naive t =
  let rec aux_size n = function
    | Rose (_, forest) -> List.fold_left (fun n t -> aux_size (n+1) t) n forest
  in aux_size 1 t


let size_tlrec t =
  let rec aux acc rests =
    match rests with
    | [] -> acc
    | [] :: rests -> aux acc rests
    | ((Rose (_, forest)) :: ts) :: rests -> aux (acc+1) (forest :: ts :: rests)
  in aux 0 [[t]]
