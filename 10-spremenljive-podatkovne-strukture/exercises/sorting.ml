(* ========== Exercise 5: Sorting  ========== *)


(*----------------------------------------------------------------------------*]
 The function [randlist len max] generates a list of length [len] with random
 integer values from 0 up to [max].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let l = randlist 10 10 ;;
 val l : int list = [0; 1; 0; 4; 0; 9; 1; 2; 5; 4]
[*----------------------------------------------------------------------------*)


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


(*----------------------------------------------------------------------------*]
 The empty list is sorted. The function [insertion_sort] sorts a list by
 consecutively inserting all of its elements into the empty list.
[*----------------------------------------------------------------------------*)



(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*]
 Selection Sort
[*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

(*----------------------------------------------------------------------------*]
 The function [min_and_rest list] returns a pair [Some (z, list')] such that [z]
 is the smallest element in [list] and [list'] is equal to [list] with the
 first occurance of [z] removed. If the list is empty it returns [None].
[*----------------------------------------------------------------------------*)


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


(*----------------------------------------------------------------------------*]
 The function [index_min a lower upper] computes the index of the smallest
 element in [a] between indices [lower] and [upper] (both included).
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 index_min [|0; 2; 9; 3; 6|] 2 4 = 4
[*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*]
 The function [selection_sort_array] implements in-place selection sort.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 Hint: To test your function, you can use the functions [Array.of_list] and
 [Array.to_list] combined with [randlist].
[*----------------------------------------------------------------------------*)

