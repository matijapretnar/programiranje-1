type 'a improved_list = 
  | Empty 
  | Node of 'a array * 'a improved_list

(* a *)
let test = Node ([|1;2;20|], Node ([|17;19;20;30|], Node([|100|], Empty)))

(* b *)
let rec ilist_len = function
  | Empty -> 0
  | Node (arr, rest) -> Array.length arr + ilist_len rest

(* c *)
let rec get_el i = function 
  | Empty -> None
  | Node (arr, rest) ->
      if i < 0 then 
        None 
      else if i <= Array.length arr - 1 then
        Some arr.(i)
      else 
        get_el (i - Array.length arr) rest

(* d *)
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

let is_sorted ilist = 
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
  checker None ilist

(* e *)
let rec update ilist i x = 
  match ilist with 
  | Empty -> Empty
  | Node (arr, rest) -> 
      if i >= Array.length arr then 
        Node (arr, update rest (i - Array.length arr) x)
      else
        let arr' = Array.copy arr in
        let _ = arr'.(i) <- x in
        Node (arr', rest)