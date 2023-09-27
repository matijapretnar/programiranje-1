let rec vsota =
  function
  | [] -> 0
  | x :: xs -> x + vsota xs

let vsota' xs =
  let rec aux acc =
    function
    | [] -> acc
    | x :: xs -> aux (acc + x) xs
  in
  aux 0 xs

let rec preslikaj f =
  function
  | [] -> []
  | x :: xs -> f x :: preslikaj f xs

let obrni xs =
  let rec aux acc =
    function
    | [] -> acc
    | x :: xs -> aux (x :: acc) xs
  in 
  aux [] xs

let preslikaj' f xs =
  let rec aux acc =
    function
    | [] -> obrni acc
    | x :: xs -> aux (f x :: acc) xs
  in
  aux [] xs
