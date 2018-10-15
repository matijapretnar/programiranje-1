let razlika_kvadratov x y = (x + y) * (x + y) - x * x - y * y

let rec uporabi_na_paru f (x, y) = (f x, f y)

let rec ponovi_seznam n sez =
  if n <= 0 then [] else sez @ ponovi_seznam (n - 1) sez

let rec razdeli sez =
  let rec razdeli_aux acc_lt acc_geq = function
    | [] -> (List.rev acc_lt, List.rev acc_geq)
    | x :: xs when x < 0 -> razdeli_aux (x :: acc_lt) acc_geq xs
    | x :: xs -> razdeli_aux acc_lt (x :: acc_geq) xs
  in
  razdeli_aux [] [] sez
