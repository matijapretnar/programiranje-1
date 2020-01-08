type 'a improved_list = Empty | Node of ('a Array.t * ('a improved_list))

let test = Node ([| 1;2;20 |], Node ([| 17;19;20;30 |], Node([| 100 |], Empty)))

let len =
  let rec len_aux acc = function
    | Empty -> acc
    | Node (arr, rest) -> len_aux (acc + Array.length arr) rest
  in len_aux 0

let rec index i_list ind = 
  match i_list with 
    | Empty -> None
    | Node (arr, rest) -> 
      if ind >= Array.length arr then 
        index rest (ind - Array.length arr) 
      else
         if ind < 0 then None else Some arr.(ind)

let is_smaller cmp = function
  | None -> fun _ -> true
  | Some x -> cmp x

let is_a_sorted last cmp arr =
  let l = Array.length arr in
  if l = 0 then (last, true) else
  let rec aux ind =
    if ind + 2 >= l
    then true else 
    cmp arr.(ind) arr.(ind + 1) && aux (ind + 1)
  in 
    (Some arr.(l-1), is_smaller cmp last arr.(0) &&  aux 0)

let is_sorted cmp = 
  let rec is_sorted_aux last = function
    | Empty -> true
    | Node (arr, rest) -> 
        let (last, result) = is_a_sorted last cmp arr in
        result && is_sorted_aux last rest
  in 
  is_sorted_aux None

let rec update i_list ind x = 
  match i_list with 
    | Empty -> Empty
    | Node (arr, rest) -> 
      if ind >= Array.length arr then 
        Node (arr, update rest (ind - Array.length arr) x)
      else
        (let arr = Array.copy arr in
          arr.(ind) <- x;
          Node (arr, rest)
        )

let rec update_mutable i_list ind x = 
  match i_list with 
    | Empty -> ()
    | Node (arr, rest) -> 
      if ind >= Array.length arr then 
        update_mutable rest (ind - Array.length arr) x
      else
        arr.(ind) <- x
        
(* Better solutions *)

let beq_op bound x =
  match bound with
  | None -> true
  | Some b -> b <= x
let is_a_sorted bound arr =
  let l = Array.length arr in
  if l = 0 then 
    (bound, true) 
  else
    let rec checker bound i =
      if i >= l - 1 then true 
      else beq_op bound arr.(i) && checker (Some arr.(i)) (i + 1)
    in 
    (Some arr.(l-1), checker bound 0)
let is_sorted cmp = 
  let rec checker bound = function
    | Empty -> true
    | Node (a, rest) -> 
        let (bound', this_sorted) = is_a_sorted bound a in
        this_sorted && checker bound' rest
  in 
  checker None
  
let beq_op bound x =
  match bound with
  | None -> true
  | Some b -> b <= x
let is_a_sorted arr l =
  let rec aux i =
    if i + 2 >= l then true 
    else arr.(i) <= arr.(i + 1) && aux (i + 1)
  in
  aux 0
let is_sorted cmp = 
  let rec checker bound = function
    | Empty -> true
    | Node (a, rest) ->
        let l = Array.length a in
        if l = 0 then 
          checker bound rest
        else
          let first, last = a.(0), a.(l-1) in
          beq_op bound first && is_a_sorted a l && checker (Some last) rest 
  in
  checker None