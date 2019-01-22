let razdeli_na_pol xs =
  let rec aux xs1 xs2 = function
  | [] -> (xs1, xs2)
  | x :: xs -> aux xs2 (x :: xs1) xs
  in
  aux [] [] xs

let rec zlij xs ys =
  match (xs, ys) with
  | ([], _) -> ys
  | (_, []) -> xs
  | (x :: xs', y :: ys') ->
      if x <= y then x :: zlij xs' ys else y :: zlij xs ys'

let rec uredi_z_zlivanjem xs =
  match xs with
  | [] | [_] -> xs
  | _ :: _ :: _ ->
    let (xs1, xs2) = razdeli_na_pol xs in
    let (xs1', xs2') = (uredi_z_zlivanjem xs1, uredi_z_zlivanjem xs2) in
    zlij xs1' xs2'

let pivotiraj p xs =
  let rec aux manjsi vecji = function
  | [] -> (manjsi, vecji)
  | x :: xs ->
      if x <= p then aux (x :: manjsi) vecji xs else aux manjsi (x :: vecji) xs
  in
  aux [] [] xs

let rec hitro_uredi xs =
  match xs with
  | [] | [_] -> xs
  | p :: xs' ->
    let (xs1, xs2) = pivotiraj p xs' in
    let (xs1', xs2') = (hitro_uredi xs1, hitro_uredi xs2) in
    xs1' @ p :: xs2'


let nakljucni_seznam m n = List.init n (fun _ -> Random.int m)

let preveri_urejanje urejanje =
  let test = (nakljucni_seznam 100 100) in
  (urejanje test = List.sort compare test)