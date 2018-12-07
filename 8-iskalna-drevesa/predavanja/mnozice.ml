let stevilo_razlicnih_s_seznami xs =
  let rec aux ze_videni = function
    | [] -> List.length ze_videni
    | x :: xs when List.mem x ze_videni ->
        aux ze_videni xs
    | x :: xs -> aux (x :: ze_videni) xs
  in
  aux [] xs


type 'a drevo =
    | Prazno
    | Sestavljeno of 'a drevo * 'a * 'a drevo

let rec vsebuje x = function
  | Prazno -> false
  | Sestavljeno (l, y, d) ->
      if x < y then
        vsebuje x l
      else if y < x then
        vsebuje x d
      else
        x = y

let rec dodaj x = function
  | Prazno -> Sestavljeno (Prazno, x, Prazno)
  | Sestavljeno (l, y, d) as drevo ->
      if x < y then
        Sestavljeno (dodaj x l, y, d)
      else if y < x then
        Sestavljeno (l, y, dodaj x d)
      else
        drevo

let rec velikost = function
  | Prazno -> 0
  | Sestavljeno (l, _, d) -> 1 + velikost l + velikost d

let prazna = Prazno

let stevilo_razlicnih_z_mnozicami xs =
  let rec aux ze_videni = function
    | [] -> velikost ze_videni
    | x :: xs when vsebuje x ze_videni ->
        aux ze_videni xs
    | x :: xs -> aux (dodaj x ze_videni) xs
  in
  aux prazna xs

let rec nakljucni_seznam a b = function
  | 0 -> []
  | n -> a + Random.int (b - a + 1) :: nakljucni_seznam a b (n - 1)
