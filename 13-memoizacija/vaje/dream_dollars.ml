(* Exercise largely taken from Jeff Erickson's lecture notes. *)

(* In a previous life, you worked as a cashier in the lost Antarctican colony of Nadira, spending
   the better part of your day giving change to your customers. Because paper is a very rare
   and valuable resource in Antarctica, cashiers were required by law to use the fewest bills
   possible whenever they gave change. Thanks to the numerological predilections of one of
   its founders, the currency of Nadira, called Dream Dollars, was available in the following
   denominations: $1, $7, $10, $28.
*)

let denominations = [ 1; 7; 10; 28 ]


(* 0.i) Formulate the problem precisely in natural language. *)

(*
   Given an amount n, ...
*)


(* 0.ii) Describe the problem recursively. *)

(*
   Given an amount n, ...
*)


(* 1. The greedy change algorithm repeatedly takes the largest bill that does
   not exceed the target amount. For example, to make $122 using the greedy
   algorithm, we first take a $91 bill, then a $28 bill, and finally three $1
   bills.

   Give an example where this greedy algorithm uses more Dream Dollar bills
   than the minimum possible.

   Hint: this is tricky. If you can't find a solution, you can implement the
   greedy algorithm and test it against your dynamic programming solutions
   later.
*)

let rec bills_greedy n = failwith "todo"

(* 2.i) Describe and analyze a recursive algorithm that computes, given an
   integer k, the shortest list of bills needed to make k Dream Dollars. (Don’t
   worry about making your algorithm fast; just make sure it’s correct.)
*)

let rec bills_rec n = failwith "todo"

(* Use the generic memozation function below to write a memoized recursive
   algorithm. *)
let memoiziraj_rec odviti_f =
   let rezultati = Hashtbl.create 512 in
   let rec mem_f x =
     match Hashtbl.find_opt rezultati x with
     | None ->
         let y = odviti_f mem_f x in
         Hashtbl.add rezultati x y;
         y
     | Some y ->
         y
   in
   mem_f

let rec bills_mem n = failwith "todo"

(* 2.ii) Draw the call tree of your recursive definition for n = 5 and identify
   which subproblems are repeated. Can you find an evaluation order that will
   allow you to compute your solutions bottom-up? *)

(*
   MAKE A DRAWING
*)


(* 2.iii) Describe a dynamic programming algorithm that computes, given an integer
   k, the shortest list of bills needed to make k Dream Dollars. (This one needs
   to be fast.)
*)

let bills_iter n = failwith "todo"
