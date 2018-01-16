(* Generate a list of length len with random values up to max. Example:
utop[1]> let l = randlist 10 10 ;;
val l : int list = [0; 1; 0; 4; 0; 9; 1; 2; 5; 4]

Can be used to test sorting functions as in
let l = (randlist 100 100) in selection_imperative_list l = List.sort compare l;;

 *)
let rec randlist len max =
  if len <= 0 then []
  else Random.int max :: (randlist (len-1) max)

(* The function insert y xs inserts y into the already sorted list xs and
   returns a sorted list.

   For example:

utop[75]> insert 9 [0; 2];;
- : int list = [0; 2; 9]
utop[76]> insert 1 [0; 2];;
- : int list = [0; 1; 2]
utop[79]> insert 1 [];;
- : int list = [1]
 *)
let rec insert y = function
  | [] -> [y]
  | x::xs -> if y > x
             then x :: (insert y xs)
             else y :: x :: xs

(* The empty list is sorted. We can sort a list by consecutively inserting all
   of its elements into the empty list. Use this idea to write insertion_sort
   using List.fold_left and insert. *)
let ins_sort l = List.fold_left (fun acc x -> insert x acc) [] l

(* Write a recursive function that takes a list l and if l is non-empty,
   returns a pair Some (z, l_without_z) such that z is the smallest element in
   l and l_without_z is l with the first occurance of z removed. *)
let min_and_rest = function
  | [] -> None
  | (x :: xs) as l ->
     let rec remove_one z = function
       | [] -> failwith "not found"
       | x :: xs -> if x = z then xs else z :: remove_one z xs in
     let rec aux y = function
       | [] -> y
       | x::xs -> aux (min x y) xs
     in
     let z = aux x xs in
     Some (z, remove_one z l)

(* Slightly more complicated recursion, but easier to reason with. *)
let min_and_rest l =
  let rec min_and_rest mx = function
  | [] -> mx
  | x::xs ->
     (match mx with
      | None -> min_and_rest (Some (x, [])) xs
      | Some (y, xs_so_far_without_y) ->
         if x < y
         then min_and_rest (Some (x, y::xs_so_far_without_y)) xs
         else min_and_rest (Some (y, x::xs_so_far_without_y)) xs)
  in min_and_rest None l

(* Selection sort works by keeping a list l partitioned into a sublist that is
   already sorted and a sublist of left-overs that still needs treatment, and
   consecutively moving the smallest element of the left-overs to the sorted
   part. *)

(* Use min_and_rest to implement selection sort as a recursive function. *)
let selection_sort l =
  let rec aux sorted xs =
    match min_and_rest xs with
    | None -> List.rev sorted
    | Some (x,xs) -> aux (x::sorted) xs
  in aux [] l


(* When working with arrays instead of lists, selection sort can work
   "in-place", i.e. without creating intermediate copies of (parts of) the
   input. it still works by partitioning the input into a sorted part and a
   to-do part. The sorted part is always an initial segment of the input array,
   delimited by an index boundary_sorted. To make progress, we don't extract
   the smallest element of the left-overs, but locate its index, and then swap
   it with the element located at the boundary. When the boundary reaches the
   end of the array, the input is sorted. *)

(* Write a function swap a i j that exchanges a.(i) and a.(j) *)
let swap a i j =
  let t = a.(i) in
  a.(i) <- a.(j); a.(j) <- t

(* Write a function index_min a lower upper that computes the index of the
   smallest element in a between indices lower and upper. Example:
   index_min [|0; 2; 9; 3; 6|] 2 4 = 4 *)
let index_min a lower upper =
    let index_min = ref lower in
    for i = lower to upper do
      if a.(i) < a.(!index_min) then
        index_min := i
    done;
    !index_min

(* Implement in-place selection sort. *)
let selection_imperative a =
  let index_end = Array.length a - 1 in
  for boundary_sorted = 0 to index_end do
    let i = index_min a boundary_sorted index_end in
    swap a i boundary_sorted
  done

(* To test your array-function, you can turn it into a function that sorts
   lists by using Array.of_list and Array.to_list. *)
let selection_imperative_list l =
  let a = Array.of_list l in
  selection_imperative a;
  Array.to_list a
