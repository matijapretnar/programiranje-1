type complex = { re : float ; im : float }

let complex_add x y = { re = x.re +. y.re ; im = x.im +. y.im }

let complex_conjugate {re ; im} = {re ; im = -. im}

let list_apply_either pred f g l =
  List.map (fun x -> if pred x then f x else g x) l

let eval_poly coefs x =
  let rec aux acc coefs x_n =
    match coefs with
    | [] -> acc
    | a :: coefs -> aux (acc + a * x_n) coefs (x * x_n)
  in
  aux 0 coefs 1
