let razlika_kvadratov x y = (x + y) * (x + y) - x * x - y * y

let rec uporabi_na_paru f (x, y) = (f x, f y)

let rec ponovi_seznam n sez =
  if n <= 0 then [] else sez @ ponovi_seznam (n - 1) sez

let rec razdeli x sez =
  let rec razdeli_aux acc_lt acc_geq = function
    | [] -> (List.rev acc_lt, List.rev acc_geq)
    | y :: ys when y < x -> razdeli_aux (y :: acc_lt) acc_geq ys
    | y :: ys -> razdeli_aux acc_lt (y :: acc_geq) ys
  in
  razdeli_aux [] [] sez
