type 'a drevo =
  | Prazno
  | Sestavljeno of 'a drevo * 'a * 'a drevo

let prazna_mnozica = Prazno

let rec velikost drevo =
  match drevo with
  | Prazno -> 0
  | Sestavljeno (l, _, d) ->
      1 + velikost l + velikost d

let rec visina drevo =
  match drevo with
  | Prazno -> 0
  | Sestavljeno (l, _, d) -> 1 + max (visina l) (visina d)

let zavrti_levo = function
| Sestavljeno (l, x, Sestavljeno (dl, y, dd)) ->
    Sestavljeno (Sestavljeno (l, x, dl), y, dd)
| _ -> failwith "Tega drevesa ne morem zavrteti"

let zavrti_desno = function
| Sestavljeno (Sestavljeno (ll, y, ld), x, d) ->
    Sestavljeno (ll, y, Sestavljeno (ld, x, d))
| _ -> failwith "Tega drevesa ne morem zavrteti"

let razlika = function
    | Prazno -> 0
    | Sestavljeno (l, _, d) -> visina l - visina d

let uravnotezi drevo =
match drevo with
| Sestavljeno (l, x, d) when razlika drevo = 2 && razlika l = 1 ->
    zavrti_desno drevo
| Sestavljeno (l, x, d) when razlika drevo = 2 ->
    Sestavljeno (zavrti_levo l, x, d) |> zavrti_desno
| Sestavljeno (l, x, d) when razlika drevo = -2 && razlika d = -1 ->
    zavrti_levo drevo
| Sestavljeno (l, x, d) when razlika drevo = -2 ->
    Sestavljeno (l, x, zavrti_desno d) |> zavrti_levo
| _ -> drevo
    
let rec isci x drevo =
  match drevo with
  | Prazno -> false
  | Sestavljeno (l, vrednost, d) ->
      if x < vrednost then
        isci x l
      else if x > vrednost then
        isci x d
      else
        true
      
let rec dodaj x drevo =
  match drevo with
  | Prazno -> Sestavljeno (Prazno, x, Prazno)
  | Sestavljeno (l, vrednost, d) ->
      if x < vrednost then
        Sestavljeno (dodaj x l, vrednost, d) |> uravnotezi
      else if x > vrednost then
        Sestavljeno (l, vrednost, dodaj x d) |> uravnotezi
      else
        drevo

(* ------------------------------------------------------------------------- *)

let stevilo_razlicnih xs =
  let rec aux ze_videni = function
    | [] -> velikost ze_videni
    | x :: xs -> aux (dodaj x ze_videni) xs
  in
  aux prazna_mnozica xs

let nakljucni_seznam m n = List.init n (fun _ -> Random.int m)

let seznam_zaporednih n = List.init n (fun i -> i)

let stopaj f x =
  let zacetek = Sys.time () in
  let y = f x in
  let konec = Sys.time () in
  Printf.printf "Porabljen čas: %f ms\n" (1000. *. (konec -. zacetek));
  y

let _ = Random.self_init ()

(* let primer = nakljucni_seznam 20000 20000 *)

let primer = seznam_zaporednih 10000

let n = stopaj stevilo_razlicnih primer

let _ = Printf.printf "Število različnih: %d\n" n
