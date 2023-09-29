```ocaml
let rec obrni = function
  | [] -> []
  | x :: xs -> obrni xs @ [x]

let obrni' xs =
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (x :: acc) xs
  in
  aux [] xs
```

obrni xs = obrni' xs
- - - - - - - - - - -

obrni [] = []
obrni' [] = aux [] [] = []

obrni (x :: xs)
= obrni xs @ [x]
= obrni' xs @ [x]
= aux [] xs @ [x]

obrni' (x :: xs)
= aux [] (x :: xs)
= aux (x :: []) xs
= aux [x] xs


obrni xs @ acc = aux acc xs
