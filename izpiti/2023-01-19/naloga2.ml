type 'a list_tree = Leaf of 'a | Node of 'a list_tree list

(* 1. a) *)

(* 1. b) *)

(* 1. c) *)

(* 1. d) *)

(* 1. e) *)

let t1 = Node [ Node [ Leaf (fun x -> x) ]; Leaf (fun x -> x * 2) ]
let t2 = Node [ Leaf 1; Leaf 2 ]
let t3 = Node [ Node []; Leaf 2; Leaf 4 ]
let t4 = Node [ Node [ Leaf (fun x -> x) ]; Leaf (fun x -> x * 2) ]
let t5 = Node [ Node []; Leaf 2; Leaf 4 ]
