(* a *)

let dot_prod (x1,y1,z1) (x2,y2,z2) = 
  x1 *. x2 +. y1 *. y2 +. z1 *. z2 

(* b *)

let curry_second f b a = f a b

(* c *)

let combine_and_filter f xs ys =
  let rec combinator acc xs ys =
    match xs, ys with
    | [], [] | [] , _ | _ , [] -> List.rev acc
    | x :: xs, y :: ys -> 
        match f x y with
        | Some a -> combinator (a::acc) xs ys
        | None -> combinator acc xs ys
  in
  combinator [] xs ys

(* d *)

let rec conditional_print cond = function
  | [] -> ()
  | [x] -> if cond x then print_string x else () 
  | x :: xs -> 
      (if cond x then (print_string x; print_string ", ") else ());
      conditional_print cond xs
      