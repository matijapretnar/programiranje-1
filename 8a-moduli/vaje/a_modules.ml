(* "Once upon a time, there was a university with a peculiar tenure policy. All
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

 (optional) homework: read the rest of the introduction of the paper at
 https://people.mpi-sws.org/~dreyer/tor/papers/reynolds.pdf
 *)


(* Complex numbers are more complicated than natural numbers.
   Let's start with Nat! *)


(* Define a module type "NAT" that specifies the structure of a "natural
 numbers structure". It should have a carrier type, a function that tests for
 equality, a zero and a one element, addition, subtraction, and multiplication
 operations and conversion functions from and to OCaml's "int" type. *)
module type NAT = sig
    type t
    val eq   : t -> t -> bool
    val zero : t
    (* add what's missing here! *)
  end

(* Write a module that implements the NAT signature, using OCaml's "int" type
   as carrier.

   Trick : until you're done implementing Nat_int, it won't have the required
   signature. You can add stubs with `failwith "later"' to make the compiler
   happy and leave a note for yourself. *)
(*
module Nat_int : NAT = struct
  type t =
  let eq =
  ...
end
 *)

(* Write another implementation of NAT, taking inspiration from the Peano
 axioms: https://en.wikipedia.org/wiki/Peano_axioms
 - The carrier type is given by a variant type that says that a natural number
   is either zero or the successor of another natural number.
 - Equality of k and l is decided by recursion on both k and l. The base case
   is that Zero = Zero.
 - All the other functions are also defined by structural recursion (c.f.
   Wikipedia).
 *)
(*
module ... = struct

  type t = ...
  let rec eq x y = ..

end
 *)


(* For those wishing to reenact the glory of 17th century mathematics:
   Follow the fable told by John Reynolds in the introduction. *)

(* Define the signature of a module of complex numbers.
   We will need a carrier type, a test for equality, zero, one, i, negation and
   conjugation, addition, multiplication, division, and taking the inverse.
 *)
(* module type COMPLEX = sig
    type t
    val eq : t -> t -> bool
    ...
  end
 *)

(* Write an implementation of Professor Descartes's complex numbers. Reminder:
 this should be the cartesian representation (latin_of_french "Descartes" =
 "Cartesius").

  Recommendation: implement a few basic parts of the module but leave division
  for later. It's relatively messy.
 *)
(*
module Cartesian : COMPLEX = struct

  type t = {re : float; im : float}

  let eq x y = x.re = y.re && ...

  ...

end
 *)


(* Now implement Professor Bessel's complex numbers. The carrier this time
   will be a polar representation, with a magnitude and an argument for each
   complex number.

   Recommendation: First implement equality, the constants, negation, and
   multiplication. Then the rest except for addition. So far, so pleasant.
   Finally implement addition. Now form an opinion on why nobody likes polar
   coordinates. *)
(*
module Polar : COMPLEX = struct

  type t = {magn : float; arg : float}

  let pi = 2. *. acos 0.
  let rad deg = (deg /. 180.) *. pi
  let deg rad = (rad /. pi) *. 180.

  let eq x y =

  ...

end
 *)
