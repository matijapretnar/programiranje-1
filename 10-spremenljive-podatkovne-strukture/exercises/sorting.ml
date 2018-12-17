(* ========== Exercise 5: Sorting  ========== *)


(*----------------------------------------------------------------------------*]
 The function [randlist len max] generates a list of length [len] with random
 integer values from 0 up to [max].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let l = randlist 10 10 ;;
 val l : int list = [0; 1; 0; 4; 0; 9; 1; 2; 5; 4]
[*----------------------------------------------------------------------------*)

let rec randlist len max =
  if len <= 0 then
    []
  else
    Random.int max :: (randlist (len-1) max)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 We can now use [randlist] to test our sorting functions (named [our_sort] in
 the below example) with the sorting functions implemented in the module [List].
 We can also just sort a smaller generated list and check what goes wrong.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 let test = (randlist 100 100) in (our_sort test = List.sort compare test);;
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)


(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*]
 Insert Sort
[*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

(*----------------------------------------------------------------------------*]
 The function [insert y xs] inserts [y] into the already sorted list [xs] and
 returns a sorted list.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 9 [0; 2];;
 - : int list = [0; 2; 9]
 # insert 1 [4; 5];;
 - : int list = [1; 4; 5]
 # insert 7 [];;
 - : int list = [7]
[*----------------------------------------------------------------------------*)

let rec insert y = function
  | [] -> [y]
  | x :: xs when y > x -> x :: (insert y xs)
  | x :: xs -> y :: x :: xs

(*----------------------------------------------------------------------------*]
 The empty list is sorted. The function [insertion_sort] sorts a list by
 consecutively inserting all of its elements into the empty list.
[*----------------------------------------------------------------------------*)

let ins_sort list = List.fold_left (fun acc x -> insert x acc) [] list


(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*]
 Selection Sort
[*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

(*----------------------------------------------------------------------------*]
 The function [min_and_rest list] returns a pair [Some (z, list')] such that [z]
 is the smallest element in [list] and [list'] is equal to [list] with the
 first occurance of [z] removed. If the list is empty it returns [None].
[*----------------------------------------------------------------------------*)

let min_and_rest list =
  let rec remove_one z = function
    | [] -> failwith "not found"
    | x :: xs -> if x = z then xs else x :: remove_one z xs
  in
  let rec find_min current_min = function
    | [] -> current_min
    | x :: xs -> find_min (min x current_min) xs
  in
  match list with
  | [] -> None
  | x :: xs ->
     let z = find_min x xs in
     Some (z, remove_one z (x :: xs))

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Selection sort works by keeping a list partitioned into two sublists where the
 first one is already sorted and the second which contains all the elements
 that still need to be sorted. It then consecutively moves the smallest element
 of the unsorted sublist to the sorted one.

 If we start with an empty sublist as the sorted one, we know that at any step
 all the elements of our unsorted sublist are greater or equal to the elements
 of our sorted sublist (as we always move the smallest one). We then know that
 the element must be inserted at the end of our sorted sublist.
 (It is faster to reverse in the end than to use the [@] operator.)
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 The function [selection_sort] implements the above algorithm.
 Hint: Use [min_and_rest] from the previous exercise.
[*----------------------------------------------------------------------------*)

let selection_sort list =
  let rec aux sorted unsorted =
    match min_and_rest unsorted with
    | None -> List.rev sorted
    | Some (x, unsorted') -> aux (x::sorted) unsorted'
  in aux [] list


(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*]
 Selection Sort with Arrays
[*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 When working with arrays instead of lists, selection sort can work "in-place",
 i.e. without creating intermediate copies of (parts of) the input. The
 algorithm still works in the same way, however now the sorted part is always
 the beginning segment of the input array. We keep track on how far the arrays
 is sorted with an index [boundary_sorted]. To make progress, we don't extract
 the smallest element of the unsorted part, but locate its index, and then swap
 it with the element located at the boundary (the first unsorted element). When
 the boundary reaches the end of the array, the input is sorted.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 The function [swap a i j] exchanges elements [a.(i)] and [a.(j)]. The swap is
 made "in-place" and the function returns the unit.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let test = [|0; 1; 2; 3; 4|];;
 val test : int array = [|0; 1; 2; 3; 4|]
 # swap test 1 4;;
 - : unit = ()
 # test;;
 - : int array = [|0; 4; 2; 3; 1|]
[*----------------------------------------------------------------------------*)

let swap a i j =
  let v = a.(i) in
  a.(i) <- a.(j);
  a.(j) <- v

(*----------------------------------------------------------------------------*]
 The function [index_min a lower upper] computes the index of the smallest
 element in [a] between indices [lower] and [upper] (both included).
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 index_min [|0; 2; 9; 3; 6|] 2 4 = 3
[*----------------------------------------------------------------------------*)

let index_min a lower upper =
  let index_min = ref lower in
  for i = lower to upper do
    if a.(i) < a.(!index_min) then
      index_min := i
  done;
  !index_min

(*----------------------------------------------------------------------------*]
 The function [selection_sort_array] implements in-place selection sort.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 Hint: To test your function, you can use the functions [Array.of_list] and
 [Array.to_list] combined with [randlist].
[*----------------------------------------------------------------------------*)

let selection_sort_array a =
  let index_end = Array.length a - 1 in
  (* Every step moves boundary_sorted one place to the right. *)
  for boundary_sorted = 0 to index_end do
    let i = index_min a boundary_sorted index_end in
    swap a i boundary_sorted
  done

let selection_sort_list list =
  (* For testing purposes. *)
  let a = Array.of_list list in
  selection_sort_array a;
  Array.to_list a
