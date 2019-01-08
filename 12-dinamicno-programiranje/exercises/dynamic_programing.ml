(* ========== Exercises 6: Dynamic programing  ========== *)


(*----------------------------------------------------------------------------*]
 The gluttonous mouse is located in the left upper corner of a matrix. It can
 only move one field down or one field right and at the end it must arrive at
 the lower right corner. On every square of the field there is a given
 (non-negative) amount of cheese. The mouse wants to eat as much as possible
 and its trying to figure out the optimal way.

 Write the function [max_cheese cheese_matrix] that given a matrix of cheese
 amounts calculates the overall amount of cheese that the mouse will eat if it
 follows the optimal way.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_cheese test_matrix;;
 - : int = 13
[*----------------------------------------------------------------------------*)

let test_matrix = 
  [| [| 1 ; 2 ; 0 |];
     [| 2 ; 4 ; 5 |];
     [| 7 ; 0 ; 1 |] |]

let max_cheese cheese_matrix =
  let dimx = Array.length cheese_matrix in
  let dimy = Array.length cheese_matrix.(0) in
  let rec best_path x y =
    let current_cheese = cheese_matrix.(x).(y) in
    let best_right = if (x+1 = dimx) then 0 else best_path (x+1) y in
    let best_down = if (y+1 = dimy) then 0 else best_path x (y+1) in
    current_cheese + max best_right best_down
  in
  best_path 0 0

(*----------------------------------------------------------------------------*]
 We are solving the problem of alternatingly colored towers. There are four
 different types of building blocks, two of them blue and two red. The blue
 blocks have heights 2 and 3 and the red ones 1 and 2.

 Write the function [alternating_towers] for a given height calculates the
 number of different towers of given height that we can build using alternatingly
 colored blocks (red on blue, blue on red etc.). We may start with any color.

 Hint: Use two mutually recursive auxilary functions using the keyword "and".
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # alternating_towers 10;;
 - : int = 35
[*----------------------------------------------------------------------------*)

let alternating_towers height =
  let rec redtop height =
    if height <= 0 then 
      0
    else if height <= 2 then 
      1
    else
      bluetop (height-1) + bluetop (height-2)
  and bluetop height =
    if height <= 0 then 
      0
    else if height = 2 then 
      1
    else if height = 3 then 
      2
    else
      redtop (height-2) + redtop (height-3)
  in
  redtop height + bluetop height

(*----------------------------------------------------------------------------*]
 You have won a coupon for Mercator, allowing you to purchase any articles in
 the shop, whose total weight does not exceed [max_w] kilograms. Write a
 function [best_value articles max_w], which computes the largest price sum,
 that we can get for the coupon, where we can take as many articles of the
 same kind as we want. Then write a function [best_value_uniques articles max_w]
 where only one article of the same kind may be pruchased.

 Hint: The module [Array] offers many functions such as [map], [fold_left],
 [copy] and similar, which can be used as an alternative to loops.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # best_value articles 1.;;
 - : float = 10.95
 # best_value_unique articles 1.;;
- : float = 7.66
[*----------------------------------------------------------------------------*)

(* Articles are of form (name, price, weight) *)
let articles = [|
	("yoghurt", 0.39, 0.18);
	("milk", 0.89, 1.03);
  ("coffee", 2.19, 0.2);
  ("butter", 1.49, 0.25);
  ("yeast", 0.22, 0.042);
  ("eggs", 2.39, 0.69);
  ("sausage", 3.76, 0.50);
  ("bread", 2.99, 1.0);
  ("Nutella", 4.99, 0.75);
  ("juice", 1.15, 2.0)
|]

let best_value articles max_w =
  (* Choose the item if you can and recursively search further. *)
  let rec get_item acc_w acc_p (_, p, w) =
    if acc_w +. w > max_w then
      (* Item is not suitable, return what we got so far.*)
      acc_p
    else
      (* Find best value after choosing the item. *)
      shopper (acc_w +. w) (acc_p +. p)
  (* Choose every item in the list and return the value of the best choice. *)
  and shopper w p =
    let choices = Array.map (get_item w p) articles in
    Array.fold_left max 0. choices
  in
  shopper 0. 0.

let best_value_unique articles max_w =
  (* Store which items have already been chose in the array [taken]. *)
  (* Choose the item if you can and recursively search further. *)
  let rec get_item taken acc_w acc_p i (_, p, w) =
    if acc_w +. w > max_w || taken.(i) then
      (* Item is not suitable, return what we got so far.*)
      acc_p
    else
      (* Find best value after choosing the item, mark choice in [taken]. *)
      let new_taken = Array.copy taken in
      (new_taken.(i) <- true; shopper new_taken (acc_w +. w) (acc_p +. p))
  (* Choose every item in the list and return the value of the best choice. *)  
  and shopper taken w p =
    let choices = Array.mapi (get_item taken w p) articles in
    Array.fold_left max 0. choices
  in
  let taken = Array.map (fun _ -> false) articles in
  shopper taken 0. 0.
