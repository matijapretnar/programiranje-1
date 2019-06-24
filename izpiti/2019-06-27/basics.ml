type complex = { re : float ; im : float }

let complex_add x y = { re = x.re +. y.re ; im = x.im +. y.im }

let complex_conjugate {re ; im} = {re ; im = -. im}

let list_apply_either pred f g l = List.map (fun x -> if pred x then f x else g x) l

let rec take l n =
  match l, n with
  | [], _
  | _, 0 -> []
  | h :: t, n -> h :: (take t (n-1))

let rec get_range l min max =
  match l, min with
  | _, 0 -> take l max
  | h::t, _ -> get_range t (min - 1) (max - 1)
  | [], _ -> []

let power x n =
  let rec aux z n = if n <= 0 then z else aux (z * x) (n - 1) in
  aux 1 n

let eval_poly coefs x =
  let rec aux coefs n =
    match coefs with
    | [] -> 0
    | a :: coefs -> a * (power x n) + aux coefs (n + 1)
  in
  aux coefs 0

let eval_poly_tlrec coefs x =
  let rec aux res coefs n =
    match coefs with
    | [] -> res
    | a :: coefs -> aux (res + a * (power x n)) coefs (n + 1)
  in
  aux 0 coefs 0
