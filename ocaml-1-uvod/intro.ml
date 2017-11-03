
(* ===== Exercise 1: Introduction to OCaml  ===== *)


(* The function "penultimate_element l" returns the second-to-last element of the list l. 
 If the list is too short it raises an error.
 ----------
 # penultimate_element [1; 2; 3; 4];;
 - : int = 3
 ---------- *)

let penultimate_element l = ()

(* The function "get k l" returns the k-th element in the list l.
 Numbering (as usual) starts with 0.
 Suppose that k is non-negative.
 If the list is too short it raises an error.
 ----------
 # get 2 [0; 0; 1; 0; 0; 0];;
 - : int = 1
 ---------- *)

let get k l = ()

(* The function "double l" doubles the occurences of elements in the list l.
 ----------
 # double [1; 2; 3];;
 - : int list = [1; 1; 2; 2; 3; 3]
 ---------- *)

let double l = ()

(* The function "divide k l" divides the list l into a pair of lists. The first list
 contains the first k elements of the list l and the second list contains the rest.
 When k is outside the bounds of l, the appropriate list should be [])
 ----------
 # divide 2 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2], [3; 4; 5])
 # divide 7 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2; 3; 4; 5], [])
 ---------- *)
 
let divide k l = ()

(* The function "delete k l" removes the k-th element of the list l.
 If the list is too short it raises an error.
 ----------
 # delete 3 [0; 0; 0; 1; 0; 0];;
 - : int list = [0; 0; 0; 0; 0]
 ---------- *)
 
let delete k l = ()

(* The function "slice i k l" returns the sub-list of l from the i-th up
 to (excluding) the k-th element.
 Suppose that i and k are fitting.
 ----------
 # slice 3 6 [0; 0; 0; 1; 2; 3; 0; 0];;
 - : int list = [1; 2; 3]
 ---------- *)
 
let slice i k l = ()

(* The function "insert x k l" inserts (not replaces) x into the list l at index k.
 If k is outside of bounds of l insert at the beggining or the end instead.
 ----------
 # insert 1 3 [0; 0; 0; 0; 0];;
 - : int list = [0; 0; 0; 1; 0; 0]
 # insert 1 (-2) [0; 0; 0; 0; 0];;
 - : int list = [1; 0; 0; 0; 0; 0]
 ---------- *)

let insert x k l = ()

(* The function "rotate n l" rotates the list l to the left by n places.
 Suppose that n is within the bounds of l.
 ----------
 # rotate 2 [1; 2; 3; 4; 5];;
 - : int list = [3; 4; 5; 1; 2]
 ---------- *)

let rotate n l = ()
 
(* The function "remove x l" removes all occurrences of x in the list l.
 ----------
 # remove 1 [1; 1; 2; 3; 1; 2; 3; 1; 1];;
 - : int list = [2; 3; 2; 3]
 ---------- *)

let remove x l = ()

(* The function "is_palindrome l" checks if a list is a palindrome.
 Hint: Use an auxiliary function that reverses a list. 
 ----------
 # is_palindrome [1; 2; 3; 2; 1];;
 - : bool = true
 # is_palindrome [0; 0; 1; 0];;
 - : bool = false
 ---------- *)
 
let is_palindrome l = ()
  
(* The function "max_on_components l1 l2" returns a list with the maximum element
 of lists l1 and l2 at the each index.
 The lenght of the returned list should be equal to the shorter of l1 and l2. 
 ----------
 # max_on_components [5; 4; 3; 2; 1] [0; 1; 2; 3; 4; 5; 6];;
 - : int list = [5; 4; 3; 3; 4]
 ---------- *)
let max_on_components l1 l2 = ()
  
(* The function "second_largest l" returns the second largest value in the list l.
 Multiple occurrences of the same element count as one value.
 Suppose the list contains at least two distinct values.
 Hint: Use an auxiliary function that finds the maximum element of a list. 
 ----------
 # second_largest [1; 10; 11; 11; 5; 4; 10];;
 - : int = 10
 ---------- *)
 
let second_largest l = ()