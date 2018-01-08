
(* ===== Exercise: Dynamic programing  ===== *)

(* The gluttonous mouse is located in the left upper corner of a matrix.
   It can only move one field down or one field right and at the end it must
   arrive at the lower right corner. On every square of the field there is
   a given (non-negative) amount of cheese. The mouse wants to eat as much
   as possible and its trying to figure out the optimal way.

   Write the function "max_cheese cheese_matrix" that given a matrix of
   cheese amounts calculates the overall amount of cheese that the mouse
   will eat if it follows the optimal way.

   ----------
   # max_cheese test_matrix;;
   - : int = 13
   ----------*)

let test_matrix = [| [| 1 ; 2 ; 0 |];
                     [| 2 ; 4 ; 5 |];
                     [| 7 ; 0 ; 1 |]  |]

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


(* We are solving the problem of towers that it was introduced in the lectures.
   There are four different types of building blocks, two of them blue and two
   red. The blue blocks have heights 2 and 3 and the red ones 1 and 2.

   Write the function "alternating_towers height" that calculates the amount
   of different towers of given height that we can build using alternatingly
   colored blocks.

   Hint: Use two auxilary functions. For mutual recursion use the keyword "and".
   
   ----------
   # alternating_towers 10;;
   - : int = 35
   ---------- *)

let alternating_towers height =
  let rec redtop height =
    if height <= 0 then 0
    else if height <= 2 then 1
    else
      bluetop (height-1) + bluetop (height-2)
  and bluetop height =
    if height <= 0 then 0
    else if height = 2 then 1
    else if height = 3 then 2
    else
      redtop (height-2) + redtop (height-3)
  in
  redtop height + bluetop height
