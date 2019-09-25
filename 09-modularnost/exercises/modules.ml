(* ========== Exercise 8: Modules  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 "Once upon a time, there was a university with a peculiar tenure policy. All
 faculty were tenured, and could only be dismissed for moral turpitude. What
 was peculiar was the definition of moral turpitude: making a false statement
 in class. Needless to say, the university did not teach computer science.
 However, it had a renowned department of mathematics.

 One Semester, there was such a large enrollment in complex variables that two
 sections were scheduled. In one section, Professor Descartes announced that a
 complex number was an ordered pair of reals, and that two complex numbers were
 equal when their corresponding components were equal. He went on to explain
 how to convert reals into complex numbers, what "i" was, how to add, multiply,
 and conjugate complex numbers, and how to find their magnitude.

 In the other section, Professor Bessel announced that a complex number was an
 ordered pair of reals the first of which was nonnegative, and that two complex
 numbers were equal if their first components were equal and either the first
 components were zero or the second components differed by a multiple of 2π. He
 then told an entirely different story about converting reals, "i", addition,
 multiplication, conjugation, and magnitude.

 Then, after their first classes, an unfortunate mistake in the registrar's
 office caused the two sections to be interchanged. Despite this, neither
 Descartes nor Bessel ever committed moral turpitude, even though each was
 judged by the other's definitions. The reason was that they both had an
 intuitive understanding of type. Having defined complex numbers and the
 primitive operations upon them, thereafter they spoke at a level of
 abstraction that encompassed both of their definitions.

 The moral of this fable is that:
   Type structure is a syntactic discipline for enforcing levels of
   abstraction."

 from:
 John C. Reynolds, "Types, Abstraction, and Parametric Polymorphism", IFIP83
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)


(*----------------------------------------------------------------------------*]
 Define a module type [NAT] that specifies the structure of a "natural numbers 
 structure". It should have a carrier type, a function that tests for equality, 
 a zero and a one element, addition, subtraction, and multiplication operations 
 and conversion functions from and to OCaml's [int] type.

 Note: Conversion functions are usually named [to_int] and [of_int], so that
 when used, the function name [NAT.of_int] tells you that you a natural number
 from an integer.
[*----------------------------------------------------------------------------*)

module type NAT = sig
  type t

  val eq   : t -> t -> bool
  val zero : t
  (* Add what's missing here! *)
  (* val to_int : t -> int *)
  (* val of_int : int -> t *)
end

(*----------------------------------------------------------------------------*]
 Write a module that implements the [NAT] signature, using OCaml's [int] type 
 as the carrier.

 Trick: until you're done implementing [Nat_int], it won't have the required
 signature. You can add stubs with [failwith "later"] to make the compiler 
 happy and leave a note for yourself, however it does not work with constants. 
[*----------------------------------------------------------------------------*)

module Nat_int : NAT = struct

  type t = int
  let eq x y = failwith "later"
  let zero = 0
  (* Add what's missing here! *)

end

(*----------------------------------------------------------------------------*]
 Write another implementation of [NAT], taking inspiration from the Peano
 axioms: https://en.wikipedia.org/wiki/Peano_axioms

 First define the carrier type with two constructors, one for zero and one for
 the successor of another natural number.
 Most of the functions are defined using recursion, for instance the equality 
 of [k] and [l] is decided by recursion on both [k] and [l] where the base case
 is that [Zero = Zero].
[*----------------------------------------------------------------------------*)

module Nat_peano : NAT = struct

  type t = unit (* This needs to be changed! *)
  let eq x y = failwith "later"
  let zero = () (* This needs to be changed! *)
  (* Add what's missing here! *)

end

(*----------------------------------------------------------------------------*]
 OCaml modules are first class and can be passed to functions as arguments by
 using the keyword [module]. The function definition is then

 # let f (module M : M_sig) = ...

 and passing a module as an argument is done by

 # f (module M_implementation);;

 The function [sum_nat_100] accepts a module of type [NAT] and using the module
 sums the first 100 natural numbers. Because the function cant return something
 of the type [NAT.t] (because we don't know what module it belongs to, it could
 be an [int] or a variant type) it returns an [int] that we get with the method
 [to_int].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # sum_nat_100 (module Nat_int);;
 - : int = 4950
 # sum_nat_100 (module Nat_peano);;
 - : int = 4950
[*----------------------------------------------------------------------------*)

let sum_nat_100 (module Nat : NAT) = ()

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Now we follow the fable told by John Reynolds in the introduction.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Define the signature of a module of complex numbers.
 We will need a carrier type, a test for equality, zero, one, i, negation and
 conjugation, addition, and multiplication.
[*----------------------------------------------------------------------------*)

module type COMPLEX = sig
  type t
  val eq : t -> t -> bool
  (* Add what's missing here! *)
end

(*----------------------------------------------------------------------------*]
 Write an implementation of Professor Descartes's complex numbers. This should 
 be the cartesian representation.
[*----------------------------------------------------------------------------*)

module Cartesian : COMPLEX = struct

  type t = {re : float; im : float}

  let eq x y = failwith "later"
  (* Add what's missing here! *)

end

(*----------------------------------------------------------------------------*]
 Now implement Professor Bessel's complex numbers. The carrier this time
 will be a polar representation, with a magnitude and an argument for each
 complex number.

 Recommendation: Implement addition at the end, as it gets very messy (might
 as well be the end of the century).
[*----------------------------------------------------------------------------*)


module Polar : COMPLEX = struct

  type t = {magn : float; arg : float}

  (* Auxiliary functions to make life easier. *)
  let pi = 2. *. acos 0.
  let rad_of_deg deg = (deg /. 180.) *. pi
  let deg_of_rad rad = (rad /. pi) *. 180.

  let eq x y = failwith "later"
  (* Add what's missing here! *)

end


(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 DICTIONARIES
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 In the tree exercises we defined a type of dictionaries [('key, 'value) dict],
 that also had functions [dict_get], [dict_insert] and [print_dict]. Write a
 fitting signature for dictionaries [DICT] and construct it's implementation
 in the same way as in the tree exercises.

 The module should include an [empty] dictionary and functions [get], [insert]
 and [print] (where print should again work only on [(string, int) t)].
[*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*]
 The function [count (module Dict) list] counts how often certain elements
 appear in [list] using the chosen dictionary module and prints it.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # count (module Tree_dict) ["b"; "a"; "n"; "a"; "n"; "a"];;
 a : 3
 b : 1
 n : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

let count (module Dict : DICT) list = ()