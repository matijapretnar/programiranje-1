(* 1. a) *)

let permutations a b c =
  [ (a, b, c); (a, c, b); (b, a, c); (b, c, a); (c, a, b); (c, b, a) ]

(* 1. b) *)

let rec zip_with_option l1 l2 =
  match (l1, l2) with
  | [], [] -> []
  | [], x :: xs -> (None, Some x) :: zip_with_option l1 xs
  | x :: xs, [] -> (Some x, None) :: zip_with_option xs l2
  | x :: xs, y :: ys -> (Some x, Some y) :: zip_with_option xs ys

(* 1. c) *)
let rec zip_with_default l1 l2 d1 d2 =
  match (l1, l2) with
  | [], [] -> []
  | [], x :: xs -> (d1, x) :: zip_with_default l1 xs d1 d2
  | x :: xs, [] -> (x, d2) :: zip_with_default xs l2 d1 d2
  | x :: xs, y :: ys -> (x, y) :: zip_with_default xs ys d1 d2

(* 1. d) *)

type response = Left | Middle | Right

let distribute f lst =
  let rec helper acc r m l =
    match lst with
    | [] -> (List.rev acc, List.rev r, List.rev m)
    | h::t ->
        match f h with
        | Right -> helper (h::acc) (h::r) m t
        | Middle -> helper (h::acc) r (h::m) t
        | Left -> helper (h::acc) r m t
  in helper [] [] [] lst

let distribute_example = distribute (fun x -> if x < 0 then Left else if x = 0 then Middle else Right) [1; 2; 3; 0; -1; 0; -2; 0; 4; 5; 6]

(* 1. e) *)

type ('a, 'b) vsota = Left of 'a | Right of 'b

let iso1 f = (fun x -> f (Left x)), (fun x -> f (Right x))

let iso2 (f, g) = function
  | Left x -> f x
  | Right x -> g x
