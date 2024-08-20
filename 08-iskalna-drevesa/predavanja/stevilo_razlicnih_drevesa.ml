type 'a drevo =
  | Prazno
  | Sestavljeno of 'a drevo * 'a * 'a drevo

let prazna_mnozica = Prazno

let rec velikost drevo =
  match drevo with
  | Prazno -> 0
  | Sestavljeno (levi, _, desni) ->
      1 + velikost levi + velikost desni

let rec isci x drevo =
  match drevo with
  | Prazno -> false
  | Sestavljeno (levi, vrednost, desni) ->
      if x < vrednost then
        isci x levi
      else if x > vrednost then
        isci x desni
      else
        true
      
let rec dodaj x drevo =
  match drevo with
  | Prazno -> Sestavljeno (Prazno, x, Prazno)
  | Sestavljeno (levi, vrednost, desni) ->
      if x < vrednost then
        Sestavljeno (dodaj x levi, vrednost, desni)
      else if x > vrednost then
        Sestavljeno (levi, vrednost, dodaj x desni)
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

(* let primer = nakljucni_seznam 80000 80000 *)

let primer = seznam_zaporednih 10000

let n = stopaj stevilo_razlicnih primer

let _ = Printf.printf "Število različnih: %d\n" n
