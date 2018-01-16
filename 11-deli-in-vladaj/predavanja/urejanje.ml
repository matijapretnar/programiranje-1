let rec razdeli_na_pol = function
  | [] -> [], []
  | x :: xs ->
      let sez1, sez2 = razdeli_na_pol xs in
      x :: sez2, sez1

let rec zlij xs ys =
  match xs, ys with
  | [], _ -> ys
  | _, [] -> xs
  | (x :: xs'), (y :: ys') ->
    if x <= y then x :: zlij xs' ys else y :: zlij xs ys'

let rec uredi_z_zlivanjem sez =
  if List.length sez <= 1 then
    sez
  else
    let sez1, sez2 = razdeli_na_pol sez in
    let urejeni1 = uredi_z_zlivanjem sez1
    and urejeni2 = uredi_z_zlivanjem sez2 in
    zlij urejeni1 urejeni2

let pivotiraj pivot sez =
  let rec aux manjsi vecji = function
  | [] -> manjsi, vecji
  | y :: ys ->
    if y <= pivot then
      aux (y :: manjsi) vecji ys
    else 
      aux manjsi (y :: vecji) ys
  in
  aux [] [] sez

let rec hitro_uredi = function
  | [] -> []
  | [x] -> [x]
  | pivot :: ostali ->
      let manjsi, vecji = pivotiraj pivot ostali in
      let urejeni1 = hitro_uredi manjsi
      and urejeni2 = hitro_uredi vecji in
      urejeni1 @ pivot :: urejeni2

let zamenjaj tabela i j =
  let t = tabela.(i) in
  tabela.(i) <- tabela.(j);
  tabela.(j) <- t

let pivotiraj_na_mestu tabela i0 j0 =
  let pivot = tabela.(i0) in
  let i = ref (i0 + 1)
  and j = ref j0 in
  while !i < !j do
    while !i < !j && tabela.(!i) <= pivot do
      incr i
    done;
    while !i < !j && tabela.(!j) > pivot do
      decr j
    done;
    zamenjaj tabela !i !j
  done;
  let p = if tabela.(!i) <= pivot then !i else !i - 1 in
  zamenjaj tabela i0 p;
  p

let hitro_uredi_na_mestu tabela =
  let rec uredi_med_indeksoma i j =
    if i < j then
      let p = pivotiraj_na_mestu tabela i j in
      uredi_med_indeksoma i (p - 1);
      uredi_med_indeksoma (p + 1) j
  in
  uredi_med_indeksoma 0 (Array.length tabela - 1)
