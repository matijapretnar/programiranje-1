(* 1 *)
let sum_list lst =
  let rec sum_list_aux acc = function
    | [] -> acc
    | h :: t -> sum_list_aux (acc + h) t
  in
  sum_list_aux 0 lst

let short = List.init 20 (fun x -> x)
let long = List.init 1000000 (fun x -> x)

(* 2 *)
let rec is_sorted = function
  | [] | [_] -> true
  | x :: y :: t -> x < y && is_sorted (y :: t)

(* 3 *)
let rec insert x lst =
  match lst with
  | [] -> [x]
  | y :: t -> if x <= y then x :: lst else y :: (insert x t)

let rec sort = function
  | [] -> []
  | h :: t -> insert h (sort t)

(* 4 *)
let rec better_insert cmp x lst =
  match lst with
  | [] -> [x]
  | y :: t -> if cmp x y then x :: lst else y :: (better_insert cmp x t)

let rec sort_by cmp = function
  | [] -> []
  | h :: t -> better_insert cmp h (sort_by cmp t)


(* 5 pt *)
type priority = Top | Group of int
type status = Staff | Passenger of priority

(* Given *)
type flyer = { status : status ; name : string }

let flyers = [ {status = Staff; name = "Quinn"}
             ; {status = Passenger (Group 0); name = "Xiao"}
             ; {status = Passenger Top; name = "Jaina"}
             ; {status = Passenger (Group 1000); name = "Aleks"}
             ; {status = Passenger (Group 1000); name = "Robin"}
             ; {status = Staff; name = "Alan"}
             ]

(* 6 *)
let cmp_flyer x y =
  match x.status, y.status with
  | Staff, _ -> true
  | _, Staff -> false
  | Passenger Top, _ -> true
  | Passenger _, Passenger Top -> false
  | Passenger (Group j), Passenger (Group k) -> j > k

let boarding_sequence lst = sort_by cmp_flyer lst


(* 7 *)
let boarding_blocks lst =
  let seq = boarding_sequence lst in
  let rec aux blocks block stat = function
    | [] -> block :: blocks
    | f :: fs ->
      if stat = f.status then
         aux blocks (f::block) stat fs
      else
         aux (block :: blocks) [f] f.status fs
  in
  match seq with
  | [] -> [[]]
  | f :: fs ->
    let blocks = aux [] [f] f.status fs in
    List.rev blocks
