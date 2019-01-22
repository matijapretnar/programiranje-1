(* Write a function that doubles the sum of two elements.*)
let doublesum x y = 2 * (x + y)

(* Write a function that returns wheter or not all three components of a 3-tuple
are larger than the components of another 3-tuple. *)
let truly_larger (a, b, c) (x, y, z) = a > x && b > y && c > z

(* Write a function option_apply that gets a function and an option and applies
the fuction to the option. *)
let option_apply f = function
  | None -> None
  | Some x -> Some (f x)

(* Write a function the_other_map that takes a list of functions and an element
and returns a list of results when applying functions to the element.*)
let the_other_map f_lst x = List.map (fun f -> f x) f_lst

(* Write a function that flattens a list of options. *)
let rec flatten_opt = function
  | [] -> []
  | None :: xs -> flatten_opt xs
  | Some x :: xs -> x :: flatten_opt xs

(* Write a function that checks wheter an element appears at least twice in a list. *)
let rec appears_twice x = function
  | [] -> false
  | y :: ys -> if x = y then List.mem x ys else appears_twice x ys

(* Write a function increase_array that takes an array and increases all its
elements by 6. *)
let increase_array a = 
  for i = 0 to Array.length a - 1 do
    a.(i) <- a.(i) + 6
  done

(* Write a function that returns x^(pow) and is tail-recursive. *)
let rec exp x pow = 
  if pow <= 0 then 1 else x * (exp x (pow - 1))

let rec tlrec_exp x pow = 
  let rec aux_exp x pow acc =
    if pow <= 0 then acc else aux_exp x (pow - 1) (x * acc)
  in
  aux_exp x pow 1

(* Write a function that moves all odd elements to the front of the list. *)
let odds_first lst = 
  let even, odd = List.partition (fun x -> x mod 2 = 0) lst in
  odd @ even