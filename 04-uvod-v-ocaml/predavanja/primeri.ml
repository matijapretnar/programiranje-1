let pozdravi = function
  | "Matija" -> "Dober dan, gospod predavatelj!"
  | "Filip" | "Žiga" -> "Oj!"
  | "" -> "Živjo!"
  | "*" -> "Dober dan!"

let rec fakulteta = function
  | 0 -> 1
  | n -> n * fakulteta (n - 1)

(* 0, 1, 1, 2, 3, 5, 8,  *)

let hitri_fib n =
  let rec aux n a b =
    if n = 0 then a else aux (n - 1) b (a + b)
  in aux n 0 1

let moj_nabor velikost =
  if velikost = 2 then (2, true) else (1, false)

let razdalja (x1, y1) (x2, y2) =
  sqrt ((x1 -. x2) ** 2. +. (y1 -. y2) ** 2.)

let rec dolzina xs =
  match xs with
  | [] -> 0
  | x :: xs' -> 1 + dolzina xs'

let rec vsota xs =
  match xs with
  | [] -> 0
  | x :: xs' -> x + vsota xs'

let rec skalarni_produkt xs ys =
  match (xs, ys) with
  | ([], []) -> 0.
  | (x :: xs', y :: ys') -> x *. y +. skalarni_produkt xs' ys'