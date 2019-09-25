(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 A priority queue (PQ) is a data structure that stores elements in order of
 "priority". Elements with high priority are available before elements with low
 priority.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type comparison = LT | EQ | GT

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 To talk about priorities, we define a type of comparisons, and a signature of
 types with comparison. Note that we do not want to hide the implementation of
 the type [t] here because we want to use the compare function on values of type
 [t] *outside the module*. The reason that we leave it abstract here is because
 it is simply unknown at this time. 
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

module type Comparable = sig
    type t
    val compare : t -> t -> comparison
  end

(*----------------------------------------------------------------------------*]
 Implement a module that can compare integers. Try comparing some integers with
 the module.
[*----------------------------------------------------------------------------*)

module Cmp_Int = struct
  type t = int
  let compare x y = failwith "to do"
end

(*----------------------------------------------------------------------------*]
 When we prescribe the signature of a module we have to be careful which types
 we make abstract. If a type is abstract in the signature, we can still export
 explicitly via the [with type t = int] clause (because the implementation of
 int is known outside the module). Check that this is necessary!
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 Cmp_Int_prescribed.compare (-9000) 42;;
[*----------------------------------------------------------------------------*)

 module Cmp_Int_prescribed = (Cmp_Int : Comparable with type t = int)

(*----------------------------------------------------------------------------*]
Now let's implement a module that compares strings. To write your compare
function, use the [compare] function from the [Pervasives] module of built-in
OCaml functions. It compares strings [s] and [t] lexicographically, yielding 
-1 if s < t, 0 if s = t and 1 otherwise.
[*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*]
 A functor is simply a function on the module level. We can define a functor
 that takes a [Comparable] module as an argument and returns another
 [Comparable] module on the same carrier type but with inverted order relation.
[*----------------------------------------------------------------------------*)

(*
module Cmp_inv (Cmp : Comparable) : Comparable with type t = Cmp.t  = struct
  type t = ...
  let compare x y = ...
end
 *)


(*----------------------------------------------------------------------------*]
 To use a functor, like other functions, we have to apply it. One difference
 with other OCaml functions is that we need parenthesis around the arguments.
[*----------------------------------------------------------------------------*)

(* 
module Cmp_Int_inv = Cmp_inv (Cmp_Int)
let _ = Cmp_Int.compare (-9000) 42;;
let _ = Cmp_Int_inv.compare (-9000) 42;;
 *)


(*----------------------------------------------------------------------------*]
 Given two modules A and B that implement the [Comparable] signature, we can
 form the order that corresponds to taking their product. Mathematically, the
 product order is defined as follows. Given two pairs (a1,b1) and (a2,b2) in
 A × B, one sets (a1,b1) < (a2,b2) when a1 < a2 and b1 < b2. This only
 induces a *partial* order however. For example, the pairs (0,1) and (1,0) ∈
 ℕ × ℕ are not comparable.

 A different order we can impose on A × B is the lexicographical order:
 Define (a1,b1) < (a2,b2) when a1 < a2, or a1 = a2 and b1 < b2. This order
 is total if the orders on A and B are and is thus suitable for implementing
 a [Comparable] module. 

 Define a functor that takes two modules A, B : Comparable and produces a
 module [Cmp_lex : Comparable with type t = A.t * B.t]
[*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*]
 Finally, here is the signature of a priority queue. We have a type of priority
 queues [h], a type of elements [el], an [empty] priority queue, and operations
 to [push] onto and safely [pop] elements off the priority queue. [Pop] returns
 the new priority queue and the highest-priority element if the priority queue
 is non-empty.
[*----------------------------------------------------------------------------*)

(*
module type Priority_Queue = sig
    type h
    type el
    val empty : h
    val pop : h -> (h * el) option
    val push : ...
  end
 *)


(*----------------------------------------------------------------------------*]
 We can implement a priority queue as a sorted list. Write a functor that takes
 a [Comparable] module as an argument and implements a priority queue with
 [Cmp.t] lists as carrier. Be careful about which types you want to hide.
[*----------------------------------------------------------------------------*)

(*
module Sorted_List_Priority_Queue ... = struct

  ...

end
*)

(*----------------------------------------------------------------------------*]
 Apply your functor to build a priority queue of integers, and a priority queue
 of strings. Write some examples using push and pop!
[*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*]
 Write a functor [To_List] that takes an implementation of [Priority_Queue] as
 an argument and returns a module with a [to_list] function, that takes a
 priority queue and yields all of its elements as a list.
[*----------------------------------------------------------------------------*)


(* module To_List ... *)


(*----------------------------------------------------------------------------*]
 Let's test your [To_List] functor! Like any other values, modules can also be
 used locally. We just have to distinguish the let binding with an additional
 [module] keyword. Short variable and module names at toplevel are a mistake
 but are acceptable locally when we don't have to remember what they mean for
 too long.
[*----------------------------------------------------------------------------*)

(*
let _ =
  let h = List.fold_left IntH.push IntH.empty [1; 0; 9; 2] in
  let module TL = To_List(IntH) in
  TL.to_list h

let _ =
  let module H = Sorted_List_Priority_Queue (Cmp_inv(Cmp_Int)) in
  let module L = To_List(H) in
  let h = List.fold_left H.push H.empty [1; 0; 9; 2] in
  L.to_list h
 *)
