type 'a drevo =
  | Prazno
  | Sestavljeno of 'a drevo * 'a * 'a drevo

let prazna_vreca = Prazno

let rec velikost = function
  | Prazno -> 0
  | Sestavljeno (l, _, d) -> 1 + velikost l + velikost d

let rec vsebuje x = function
  | Prazno -> false
  | Sestavljeno (l, y, d) ->
      if x = y then
        true
      else if x < y then
        vsebuje x l
      else
        vsebuje x d

let rec dodaj x = function
  | Prazno -> Sestavljeno (Prazno, x, Prazno)
  | Sestavljeno (l, y, d) ->
      if x = y then
        Sestavljeno (l, y, d)
      else if x < y then
        Sestavljeno (dodaj x l, y, d)
      else
        Sestavljeno (l, y, dodaj x d)
(* ------------------------------------------------------------------------- *)

let stevilo_razlicnih xs =
  let rec aux ze_videni = function
    | [] -> velikost ze_videni
    | x :: xs -> aux (dodaj x ze_videni) xs
  in
  aux prazna_vreca xs

let seznam_zaporednih n = List.init n (fun i -> i)
let nakljucni_seznam m n = List.init n (fun _ -> Random.int m)

let stopaj f x =
  let zacetek = Sys.time () in
  let y = f x in
  let konec = Sys.time () in
  print_endline ("Porabljen čas: " ^ string_of_float (1000. *. (konec -. zacetek)) ^ "ms");
  y

let _ = Random.self_init ()

(* let primer = nakljucni_seznam 100000 100000 *)
let primer = seznam_zaporednih 10000

let n = stopaj stevilo_razlicnih primer

let _ = print_endline ("Število različnih: " ^ string_of_int n)
