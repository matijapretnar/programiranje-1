let difference_of_squares x y = (x + y) * (x + y) - x * x - y * y

let rec pair_apply (f, g) (x, y) = (f x, g y)

let rec multiply_list n lst =
  if n <= 0 then [] else lst @ multiply_list (n - 1) lst

let rec separate x lst =
  let rec separate_aux acc_lt acc_geq = function
    | [] -> (List.rev acc_lt, List.rev acc_geq)
    | y :: ys when y < x -> separate_aux (y :: acc_lt) acc_geq ys
    | y :: ys -> separate_aux acc_lt (y :: acc_geq) ys
  in
  separate_aux [] [] lst

let transfer_option = function
  | (Some x, Some y, Some z) -> Some (x, y, z)
  | _ -> None
