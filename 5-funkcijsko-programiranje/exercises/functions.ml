(* ========== Exercise 2: Functional Programming  ========== *)

(*----------------------------------------------------------------------------*]
 Hint: Write a function for reversing lists.
[*----------------------------------------------------------------------------*)

let reverse list =
  let rec reverse_aux acc = function
    | [] -> acc
    | x :: xs -> reverse_aux (x :: acc) xs
  in reverse_aux list []

(*----------------------------------------------------------------------------*]
 The function [repeat x n] returns a list with [n] repetitions of [x]. For
 unsuitable values of [n] it returns an empty list.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # repeat "A" 5;;
 - : string list = ["A"; "A"; "A"; "A"; "A"]
 # repeat "A" (-2);;
 - : string list = []
[*----------------------------------------------------------------------------*)

let rec repeat x n = if n <= 0 then [] else x :: (repeat x (n-1))

(*----------------------------------------------------------------------------*]
 The function [range] accepts an integer and returns a list of all non-negative
 integers up to (including) the given number. For unsuitable inputs it returns
 an empty list.
 The function is tail recursive.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # range 10;;
 - : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
[*----------------------------------------------------------------------------*)

let range_not_tailrec n =
  let rec range_from m =
    if m > n
    then []
    else m :: (range_from (m + 1))
  in range_from 0

let range n =
  let rec range_aux n acc =
    if n < 0 then acc else range_aux (n - 1) (n :: acc)
  in
  range_aux n []

(*----------------------------------------------------------------------------*]
  The function [map f list] accepts a list [list] of form [x0; x1; x2; ...] and a
  function [f] and returns a list of mapped values, [f(x0); f(x1); f(x2); ...].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let plus_two = (+)2 in
   map plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
[*----------------------------------------------------------------------------*)

let rec map f = function
  | [] -> []
  | x :: xs -> f x :: (map f xs)

(*----------------------------------------------------------------------------*]
  The function [map_tlrec] is the tail recursive version of map.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let plus_two = (fun x -> x+2) in
   map_tlrec plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
[*----------------------------------------------------------------------------*)

let map_tlrec f list =
  let rec map_aux list acc =
    match list with
    | [] -> reverse acc
    | x :: xs -> map_aux xs (f x :: acc)
  in
  map_aux list []

(*----------------------------------------------------------------------------*]
 The function [mapi f list] accepts a two argument function and returns a list
 of the mapped values of [list], where the second argument is the index of the
 element in [list].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # mapi (+) [0; 0; 0; 2; 2; 2];;
 - : int list = [0; 1; 2; 5; 6; 7]
[*----------------------------------------------------------------------------*)

let mapi f list =
  let rec mapi_aux list i =
    match list with
    | [] -> []
    | x :: xs -> (f i x) :: (mapi_aux xs (i + 1))
  in
  mapi_aux list 0

(*----------------------------------------------------------------------------*]
 The function [zip list1 list2] accepts two lists and returns a list of pairs of
 same index elements of the given lists. If the lists are of different lengths,
 it fails.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # zip [1; 1; 1; 1] [0; 1; 2; 3];;
 - : (int * int) list = [(1, 0); (1, 1); (1, 2); (1, 3)]
 # zip [1; 1; 1; 1] [1; 2; 3; 4; 5];;
 Exception: Failure "Different lengths of input lists.".
[*----------------------------------------------------------------------------*)

let rec zip list1 list2 =
  match list1, list2 with
  | [], [] -> []
  | _, [] | [], _ -> failwith "Different lengths of input lists."
  | x :: xs, y :: ys -> (x, y) :: (zip xs ys)

(*----------------------------------------------------------------------------*]
 The function [zip_enum_tlrec] accepts lists [x_0; x_1; ...] and [y_0; y_1; ...]
 and returns the list [(0, x_0, y_0); (1, x_1, y_1); ...]. The function is tail
 recursive. If the lists are of different lengths, it fails.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # zip_enum_tlrec ["a"; "b"; "c"] [7; 3; 4];;
 - : (int * string * int) list = [(0, "a", 7); (1, "b", 3); (2, "c", 4)]
[*----------------------------------------------------------------------------*)

let zip_enum_tlrec list1 list2 =
  let rec zipe_aux list1 list2 i acc =
    match list1, list2 with
    | [], [] -> reverse acc
    | _, [] | [], _ -> failwith "Different lengths of input lists."
    | x :: xs, y :: ys ->
      let element = (i, x, y) in
      zipe_aux xs ys (i + 1) (element :: acc)
   in
   zipe_aux list1 list2 0 []

(*----------------------------------------------------------------------------*]
 The function [unzip] is the inverse of [zip]. It accepts a list of pairs
 [(x0, y0); (x1, y1); ...] and returns the pair ([x0; x1; ...], [y0; y1; ...]).
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # unzip [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
[*----------------------------------------------------------------------------*)

let rec unzip = function
  | [] -> ([], [])
  | (x, y) :: tl -> let (list1, list2) = unzip tl in (x :: list1, y :: list2)

(*----------------------------------------------------------------------------*]
 The function [unzip_tlrec] is the tail recursive version of [unzip].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # unzip_tlrec [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
[*----------------------------------------------------------------------------*)

let unzip_tlrec list =
  let rec unzip_aux list acc1 acc2 =
    match list with
    | [] -> (reverse acc1, reverse acc2)
    | (x, y) :: tl -> unzip_aux tl (x :: acc1) (y :: acc2)
  in
  unzip_aux list [] []

(*----------------------------------------------------------------------------*]
 The function [fold_left_no_acc f list] accepts a list [x0; x1; ...; xn] and a
 two argument function [f] and returns the value of the computation
 f(... (f (f x0 x1) x2) ... xn).
 If the list has less than two elements it fails.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # fold_left_no_acc (^) ["F"; "I"; "C"; "U"; "S"];;
 - : string = "FICUS"
[*----------------------------------------------------------------------------*)

let rec fold_left_no_acc f = function
  | [] | _ :: [] -> failwith "List too short."
  | x :: y :: [] -> f x y
  | x :: y :: tl -> fold_left_no_acc f ((f x y) :: tl)

(*----------------------------------------------------------------------------*]
 The function [apply_sequence f x n] returns the list of repeated applications
 of the function [f] on the value [x] up until the [n]th repeated application,
 [x; f x; f (f x); ...; (f applied n times on x)].
 The function is tail recursive.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # apply_sequence (fun x -> x * x) 2 5;;
 - : int list = [2; 4; 16; 256; 65536; 4294967296]
 # apply_sequence (fun x -> x * x) 2 (-5);;
 - : int list = []
[*----------------------------------------------------------------------------*)

let apply_sequence f x n =
  let rec apply_aux f x n acc =
    if n < 0 then
      reverse acc
    else
      apply_aux f (f x) (n - 1) (x :: acc)
  in
  apply_aux f x n []

(*----------------------------------------------------------------------------*]
 The function [filter f list] returns a list of elements of [list] for which
 the function [f] returns [true].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # filter ((<)3) [0; 1; 2; 3; 4; 5];;
 - : int list = [4; 5]
[*----------------------------------------------------------------------------*)

let rec filter f = function
  | [] -> []
  | x :: xs -> if f x then x :: (filter f xs) else filter f xs

(*----------------------------------------------------------------------------*]
 The function [exists] accepts a list and a function and returns [true] if there
 exists an element of the list for which the function returns [true], otherwise
 it returns [false].
 The function is tail recursive.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # exists ((<)3) [0; 1; 2; 3; 4; 5];;
 - : bool = true
 # exists ((<)8) [0; 1; 2; 3; 4; 5];;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec exists f = function
  | [] -> false
  | x :: xs -> if f x then true else exists f xs

(*----------------------------------------------------------------------------*]
 The function [first f default list] returns the first element of the list for
 which [f] returns [true]. If such an element does not exist it returns
 [default].
 The function is tail recursive.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # first ((<)3) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 5
 # first ((<)8) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 0
[*----------------------------------------------------------------------------*)

let rec first f default = function
  | [] -> default
  | x :: xs -> if f x then x else first f default xs
