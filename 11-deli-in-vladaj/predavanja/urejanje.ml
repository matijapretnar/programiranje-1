let ne_uredi sez = sez

let rec razdeli_na_pol sez =
  match sez with
  | [] -> ([], [])
  | glava :: rep ->
      let (sodi_v_repu, lihi_v_repu) = razdeli_na_pol rep in
      (glava :: lihi_v_repu, sodi_v_repu)

let rec zlij sez1 sez2 =
  match (sez1, sez2) with
  | ([], []) -> []
  | ([], sez2) -> sez2
  | (sez1, []) -> sez1
  | (glava1 :: rep1, glava2 :: rep2) ->
      if glava1 < glava2 then
        glava1 :: zlij rep1 sez2
      else
        glava2 :: zlij sez1 rep2

let rec uredi_z_zlivanjem =
  function
  | [] -> []
  | [x] -> [x]
  | sez ->
      let (sez1, sez2) = razdeli_na_pol sez in
      let sez1' = uredi_z_zlivanjem sez1
      and sez2' = uredi_z_zlivanjem sez2 in
      zlij sez1' sez2'

let rec pivotiraj pivot =
  function
  | [] -> ([], [])
  | glava :: rep ->
      let (manjsi, vecji) = pivotiraj pivot rep in
      if glava < pivot then
        (glava :: manjsi, vecji)
      else
        (manjsi, glava :: vecji)


let rec hitro_uredi =
  function
  | [] -> []
  | glava :: rep ->
     let (sez1, sez2) = pivotiraj glava rep in
      let sez1' = hitro_uredi sez1
      and sez2' = hitro_uredi sez2 in
      sez1' @ glava :: sez2'

let nakljucni_seznam m n =
  let rec aux acc = function
  | 0 -> acc
  | n -> aux (Random.int m :: acc) (n - 1)
  in
  aux [] n

let preveri_urejanje urejanje =
  let test = (nakljucni_seznam 100 100) in
  (urejanje test = List.sort compare test)

;;

print_endline (string_of_bool (preveri_urejanje ne_uredi));
print_endline (string_of_bool (preveri_urejanje uredi_z_zlivanjem));
print_endline (string_of_bool (preveri_urejanje hitro_uredi))