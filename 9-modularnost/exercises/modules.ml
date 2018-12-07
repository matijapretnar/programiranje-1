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
  val one  : t
  val add  : t -> t -> t
  val sub  : t -> t -> t
  val mul  : t -> t -> t
  val from_int : int -> t
  val to_int   : t -> int
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
  let eq = (=)
  let zero = 0
  let one = 1
  let add = (+)
  let sub x y = max 0 (x - y)
  let mul = ( * )
  let from_int n = n
  let to_int i = i

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

  type t = Zero | S of t

  let rec eq x y =
    match (x, y) with
    | (Zero, Zero) -> true
    | (S x, S y) -> eq x y
    | _ -> false
  
  let zero = Zero
  
  let one = S Zero
  
  let rec add x = function
    | Zero -> x
    | S y -> S (add x y)
  
  let rec sub x y =
    match (x, y) with
    | (_, Zero) -> x
    | (Zero, _) -> Zero
    | (S x, S y) -> sub x y
  
  let rec mul x y =
    match x with
    | Zero -> Zero
    | S x -> add y (mul x y)
  
  let rec from_int i =
    if i <= 0
    then Zero
    else S (from_int (i-1))
  
  let rec to_int = function
    | Zero -> 0
    | S n -> 1 + (to_int n)

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

let sum_nat_100 (module Nat : NAT) =
  let hundred = Nat.from_int 100 in
  let rec sum current acc =
    if Nat.eq current hundred then
      acc
    else
      sum (Nat.add current Nat.one) (Nat.add acc current)
  in
  sum Nat.zero Nat.zero |> Nat.to_int

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
  val zero : t
  val one : t
  val i : t
  val neg : t -> t
  val conj : t -> t
  val add : t -> t -> t
  val mul : t -> t -> t
end

(*----------------------------------------------------------------------------*]
 Write an implementation of Professor Descartes's complex numbers. This should 
 be the cartesian representation.
[*----------------------------------------------------------------------------*)

module Cartesian : COMPLEX = struct

  type t = {re : float; im : float}

  let eq x y = x.re = y.re && x.im = y.im

  let zero = {re = 0.; im = 0.}
  let one =  {re = 1.; im = 0.}
  let i   =  {re = 0.; im = 1.}

  let neg {re; im} = {re = -. re; im = -. im}
  let conj {re; im} = {re; im = -. im}

  let add x y = {re = x.re +. y.re; im = x.im +. y.im}
  let mul x y = 
    let re = x.re *. y.re -. x.im *. y.im in
    let im = x.im *. y.re +. x.re *. y.im in
    {re; im}

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

  let pi = 2. *. acos 0.
  let rad deg = (deg /. 180.) *. pi
  let deg rad = (rad /. pi) *. 180.

  let eq x y =
    x.magn = y.magn &&
      (x.magn = 0. ||
         (mod_float x.arg 360.
          = mod_float y.arg 360.))

  let zero = {magn = 0.; arg = 0.}
  let one = {magn = 1.; arg = 0.}
  let i = {magn = 1.; arg = 90.}

  let neg {magn; arg} = {magn; arg = arg +. 180.}
  let conj {magn; arg} = {magn; arg = (mod_float (arg +. 180.) 360.)}

  let mul x y = {magn = x.magn *. y.magn ; arg = x.arg +. y.arg}

  (* All of this for addition... *)
  let re {magn; arg} = magn *. cos (rad arg)
  let im {magn; arg} = magn *. sin (rad arg)

  let arg re im =
    let rad =
      if re > 0. then atan (im /. re)
      else if re < 0. && im >= 0. then atan (im /. re) +. pi
      else if re < 0. && im < 0. then  atan (im /. re) -. pi
      else if re = 0. && im > 0. then pi /. 2.
      else if re = 0. && im < 0. then -.(pi /. 2.)
      else 0.
    in deg rad

  let magn re im = sqrt (re *. re +. im *. im)

  let add x y =
    let square x = x *. x in
    let magn = sqrt (square x.magn +. square y.magn +. 2. *. x.magn *. y.magn *. cos (y.arg -. x.arg))
    and arg = x.arg +.
                atan2 (y.magn *. sin (y.arg -. x.arg))
                      (x.magn +. y.magn *. cos (y.arg -. x.arg)) in
    {magn; arg}

  let add' x y =
    let z_re, z_im = re x +. re y, im x +. im y in
    let arg = arg z_re z_im
    and magn = magn z_re z_im
    in {arg; magn}

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

module type DICT = sig
  type ('key, 'value) t

  val empty : ('key, 'value) t

  val get : 'key -> ('key, 'value) t -> 'value option
  val insert : 'key -> 'value -> ('key, 'value) t -> ('key, 'value) t
  val print : (string, int) t -> unit
end

module Tree_dict : DICT = struct
  type ('key, 'value) t =
    | D_Empty
    | D_Node of ('key, 'value) t * 'key * 'value * ('key, 'value) t

  let empty = D_Empty

  let d_leaf key value = D_Node (D_Empty, key, value, D_Empty)

  let rec get key = function
    | D_Empty -> None
    | D_Node (d_l, k, value, d_r) ->
      if k = key then
        Some value
      else if key < k then
        get key d_l
      else
      get key d_r

  let rec insert key value = function
    | D_Empty -> d_leaf key value
    | D_Node (d_l, k, v, d_r) ->
      if k = key then
        D_Node (d_l, k, value, d_r)
      else if key < k then
        D_Node (insert key value d_l, k, v, d_r)
      else
        D_Node (d_l, k, v, insert key value d_r)

  let rec print = function
    | D_Empty -> ()
    | D_Node (d_l, k, v, d_r) -> (
      print d_l;
      print_string (k ^ " : "); print_int v; print_string "\n";
      print d_r)

end

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

let count (module Dict : DICT) list =
  let rec counter dict = function
    | [] -> Dict.print dict
    | x :: xs -> 
      let new_count = 
        match Dict.get x dict with
        | Some x -> x + 1 
        | None -> 1
      in
      let new_dict = Dict.insert x new_count dict in
      counter new_dict xs
  in
  counter Dict.empty list
