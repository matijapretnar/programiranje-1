
(* ===== Exercise 1: Introduction to OCaml  ===== *)


(* The function "penultimate_element l" returns the second-to-last element of the list l. 
 If the list is too short it raises an error.
 ----------
 # penultimate_element [1; 2; 3; 4];;
 - : int = 3
 ---------- *)

let rec penultimate_element = function
  | hd :: _ :: [] -> hd
  | _ :: tl -> penultimate_element tl
  | [] -> failwith "List too short."

(* The function "get k l" returns the k-th element in the list l.
 Numbering (as usual) starts with 0.
 Suppose that k is non-negative.
 If the list is too short it raises an error.
 ----------
 # get 2 [0; 0; 1; 0; 0; 0];;
 - : int = 1
 ---------- *)

let rec get k = function
  | [] -> failwith "List too short."
  | hd :: tl -> if k=0 then hd else get (k-1) tl

(* The function "double l" doubles the occurences of elements in the list l.
 ----------
 # double [1; 2; 3];;
 - : int list = [1; 1; 2; 2; 3; 3]
 ---------- *)

let rec double = function
| hd :: tl -> hd :: hd :: double tl
| [] -> []

(* The function "divide k l" divides the list l into a pair of lists. The first list
 contains the first k elements of the list l and the second list contains the rest.
 When k is outside the bounds of l, the appropriate list should be [])
 ----------
 # divide 2 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2], [3; 4; 5])
 # divide 7 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2; 3; 4; 5], [])
 ---------- *)
 
let rec divide k l =
  match (k, l) with
  | (_, []) -> ([], [])
  | (0, l) -> ([], l)
  | (k, hd::tl) -> 
    let (l1, l2) = divide (k-1) tl in
	(hd::l1, l2)

(* The function "delete k l" removes the k-th element of the list l.
 If the list is too short it raises an error.
 ----------
 # delete 3 [0; 0; 0; 1; 0; 0];;
 - : int list = [0; 0; 0; 0; 0]
 ---------- *)
 
let rec delete k = function
  | [] -> failwith "List too short."
  | hd :: tl ->
  if k=0 then tl else hd :: delete (k-1) tl

(* The function "slice i k l" returns the sub-list of l from the i-th up
 to (excluding) the k-th element.
 Suppose that i and k are fitting.
 ----------
 # slice 3 6 [0; 0; 0; 1; 2; 3; 0; 0];;
 - : int list = [1; 2; 3]
 ---------- *)
 
let slice i k l = 
  let (_, slice1) = divide i l in
  let (slice2, _) = divide (k-i) slice1 in
  slice2

(* The function "insert x k l" inserts (not replaces) x into the list l at index k.
 If k is outside of bounds of l insert at the beggining or the end instead.
 ----------
 # insert 1 3 [0; 0; 0; 0; 0];;
 - : int list = [0; 0; 0; 1; 0; 0]
 # insert 1 (-2) [0; 0; 0; 0; 0];;
 - : int list = [1; 0; 0; 0; 0; 0]
 ---------- *)

let rec insert x k l = 
  let (l1, l2) = divide k l in
  l1 @ [x] @ l2

(* The function "rotate n l" rotates the list l to the left by n places.
 Suppose that n is within the bounds of l.
 ----------
 # rotate 2 [1; 2; 3; 4; 5];;
 - : int list = [3; 4; 5; 1; 2]
 ---------- *)

let rec rotate n l = 
  let (l1, l2) = divide n l in
  l2@l1
 
(* The function "remove x l" removes all occurrences of x in the list l.
 ----------
 # remove 1 [1; 1; 2; 3; 1; 2; 3; 1; 1];;
 - : int list = [2; 3; 2; 3]
 ---------- *)

let rec remove x = function
  | hd :: tl -> if hd=x then remove x tl else hd :: remove x tl
  | [] -> []

(* The function "is_palindrome l" checks if a list is a palindrome.
 Hint: Use an auxiliary function that reverses a list. 
 ----------
 # is_palindrome [1; 2; 3; 2; 1];;
 - : bool = true
 # is_palindrome [0; 0; 1; 0];;
 - : bool = false
 ---------- *)
 
let is_palindrome l = 
  let rec reverse = function
    | hd :: tl -> reverse tl @ [hd]
    | [] -> []
  in
  l = reverse l
  
(* The function "max_on_components l1 l2" returns a list with the maximum element
 of lists l1 and l2 at the each index.
 The lenght of the returned list should be equal to the shorter of l1 and l2. 
 ----------
 # max_on_components [5; 4; 3; 2; 1] [0; 1; 2; 3; 4; 5; 6];;
 - : int list = [5; 4; 3; 3; 4]
 ---------- *)
let rec max_on_components l1 l2 = 
  match (l1,l2) with
  | (hd1::tl1, hd2::tl2) -> max hd1 hd2 :: max_on_components tl1 tl2
  | ([], _) -> []
  | (_, []) -> []
  
(* The function "second_largest l" returns the second largest value in the list l.
 Multiple occurrences of the same element count as one value.
 Suppose the list contains at least two distinct values.
 Hint: Use an auxiliary function that finds the maximum element of a list. 
 ----------
 # second_largest [1; 10; 11; 11; 5; 4; 10];;
 - : int = 10
 ---------- *)
 
let second_largest l =
  let rec largest = function
    | [] -> failwith "List too short."
	| hd :: [] -> hd
	| hd :: tl -> max hd (largest tl)
  in
  largest (remove (largest l) l)