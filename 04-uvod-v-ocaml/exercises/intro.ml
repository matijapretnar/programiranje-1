
(* ========== Exercise 1: Introduction to OCaml  ========== *)


(*----------------------------------------------------------------------------*]
 The function [penultimate_element] returns the second-to-last element of a
 list. If the list is too short it raises an error.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # penultimate_element [1; 2; 3; 4];;
 - : int = 3
[*----------------------------------------------------------------------------*)

let rec penultimate_element = ()

(*----------------------------------------------------------------------------*]
 The function [get k list] returns the [k]-th element in the list [list].
 Numbering (as usual) starts with 0. If [k] is negative, the function returns
 the first element.
 If the list is too short it raises an error.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # get 2 [0; 0; 1; 0; 0; 0];;
 - : int = 1
[*----------------------------------------------------------------------------*)

let rec get = ()

(*----------------------------------------------------------------------------*]
 The function [double list] doubles the occurences of elements in the list.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # double [1; 2; 3];;
 - : int list = [1; 1; 2; 2; 3; 3]
[*----------------------------------------------------------------------------*)

let rec double = ()

(*----------------------------------------------------------------------------*]
 The function [divide k list] divides the list into a pair of lists. The first
 list contains the first [k] elements of the list and the second contains the
 rest.
 When [k] is outside the bounds of [list], the appropriate list should be empty.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # divide 2 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2], [3; 4; 5])
 # divide 7 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2; 3; 4; 5], [])
[*----------------------------------------------------------------------------*)

let rec divide = ()

(*----------------------------------------------------------------------------*]
 The function [delete k list] removes the [k]-th element of the list.
 If the list is too short it raises an error.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # delete 3 [0; 0; 0; 1; 0; 0];;
 - : int list = [0; 0; 0; 0; 0]
[*----------------------------------------------------------------------------*)

let rec delete = ()

(*----------------------------------------------------------------------------*]
 The function [slice i k list] returns the sub-list of [list] from the [i]-th
 up to (excluding) the [k]-th element. Suppose that [i] and [k] are fitting.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # slice 3 6 [0; 0; 0; 1; 2; 3; 0; 0];;
 - : int list = [1; 2; 3]
[*----------------------------------------------------------------------------*)

let rec slice = ()

(*----------------------------------------------------------------------------*]
 The function [insert x k list] inserts (not replaces) [x] into the list at the
 index [k].
 If [k] is outside of bounds of [list], insert the element at the beggining or
 the end instead.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 1 3 [0; 0; 0; 0; 0];;
 - : int list = [0; 0; 0; 1; 0; 0]
 # insert 1 (-2) [0; 0; 0; 0; 0];;
 - : int list = [1; 0; 0; 0; 0; 0]
[*----------------------------------------------------------------------------*)

let rec insert = ()

(*----------------------------------------------------------------------------*]
 The function [rotate n list] rotates the list to the left by [n] places.
 Suppose that [n] is within the bounds of [list].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # rotate 2 [1; 2; 3; 4; 5];;
 - : int list = [3; 4; 5; 1; 2]
[*----------------------------------------------------------------------------*)

let rec rotate = ()

(*----------------------------------------------------------------------------*]
 The function [remove x list] removes all occurrences of [x] in the list.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # remove 1 [1; 1; 2; 3; 1; 2; 3; 1; 1];;
 - : int list = [2; 3; 2; 3]
[*----------------------------------------------------------------------------*)

let rec remove = ()

(*----------------------------------------------------------------------------*]
 The function [is_palindrome] checks if a list is a palindrome.
 Hint: Use an auxiliary function that reverses a list.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # is_palindrome [1; 2; 3; 2; 1];;
 - : bool = true
 # is_palindrome [0; 0; 1; 0];;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec is_palindrome = ()

(*----------------------------------------------------------------------------*]
 The function [max_on_components] returns a list with the maximum element
 of the two given lists at the each index.
 The lenght of the returned list should be equal to the shorter of the lists.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_on_components [5; 4; 3; 2; 1] [0; 1; 2; 3; 4; 5; 6];;
 - : int list = [5; 4; 3; 3; 4]
[*----------------------------------------------------------------------------*)

let rec max_on_components = ()

(*----------------------------------------------------------------------------*]
 The function [second_largest] returns the second largest value in the list.
 Multiple occurrences of the same element count as one value.
 Suppose the list contains at least two distinct values.
 Hint: Use an auxiliary function that finds the maximum element of a list.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # second_largest [1; 10; 11; 11; 5; 4; 10];;
 - : int = 10
[*----------------------------------------------------------------------------*)

let rec second_largest = ()
