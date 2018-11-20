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
