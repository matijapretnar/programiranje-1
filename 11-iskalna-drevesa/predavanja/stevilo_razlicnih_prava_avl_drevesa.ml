type 'a drevo =
  | Prazno
  | Sestavljeno of int * 'a drevo * 'a * 'a drevo

let prazna_mnozica = Prazno

let rec velikost drevo =
  match drevo with
  | Prazno -> 0
  | Sestavljeno (_, l, _, d) ->
      1 + velikost l + velikost d

let visina drevo =
  match drevo with
  | Prazno -> 0
  | Sestavljeno (h, _, _, _) -> h

let sestavi levi x desni =
  Sestavljeno (1 + max (visina levi) (visina desni), levi, x, desni)

let sestavljeno (levi, x, desni) =
  Sestavljeno (1 + max (visina levi) (visina desni), levi, x, desni)
  
let zavrti_levo = function
| Sestavljeno (h, l, x, Sestavljeno (hd, dl, y, dd)) ->
    let novi_levi = sestavi l x dl in
    sestavi novi_levi y dd
| _ -> failwith "Tega drevesa ne morem zavrteti"

let zavrti_desno = function
| Sestavljeno (h, Sestavljeno (hl, ll, y, ld), x, d) ->
    sestavljeno (ll, y, sestavljeno (ld, x, d))
| _ -> failwith "Tega drevesa ne morem zavrteti"

let razlika = function
    | Prazno -> 0
    | Sestavljeno (_, l, _, d) -> visina l - visina d

let uravnotezi drevo =
match drevo with
| Sestavljeno (_, l, x, d) when razlika drevo = 2 && razlika l = 1 ->
    zavrti_desno drevo
| Sestavljeno (_, l, x, d) when razlika drevo = 2 ->
    sestavljeno (zavrti_levo l, x, d) |> zavrti_desno
| Sestavljeno (_, l, x, d) when razlika drevo = -2 && razlika d = -1 ->
    zavrti_levo drevo
| Sestavljeno (_, l, x, d) when razlika drevo = -2 ->
    sestavljeno (l, x, zavrti_desno d) |> zavrti_levo
| _ -> drevo
    
let rec isci x drevo =
  match drevo with
  | Prazno -> false
  | Sestavljeno (_, l, vrednost, d) ->
      if x < vrednost then
        isci x l
      else if x > vrednost then
        isci x d
      else
        true
      
let rec dodaj x drevo =
  match drevo with
  | Prazno -> Sestavljeno (1, Prazno, x, Prazno)
  | Sestavljeno (_, l, vrednost, d) ->
      if x < vrednost then
        sestavljeno (dodaj x l, vrednost, d) |> uravnotezi
      else if x > vrednost then
        sestavljeno (l, vrednost, dodaj x d) |> uravnotezi
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

(* let primer = nakljucni_seznam 10000 10000 *)

let primer = seznam_zaporednih 100000

let n = stopaj stevilo_razlicnih primer

let _ = Printf.printf "Število različnih: %d\n" n
