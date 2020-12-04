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

let zavrti_levo = function
    | Sestavljeno (l, x, Sestavljeno (dl, y, dd)) ->
        Sestavljeno (Sestavljeno (l, x, dl), y, dd)
    | _ -> failwith "Tega drevesa ne morem zavrteti"

let zavrti_desno = function
    | Sestavljeno (Sestavljeno (ll, y, ld), x, d) ->
        Sestavljeno (ll, y, Sestavljeno (ld, x, d))
    | _ -> failwith "Tega drevesa ne morem zavrteti"


let rec visina = function
    | Prazno -> 0
    | Sestavljeno (l, _, d) -> 1 + max (visina l) (visina d)

let razlika = function
    | Prazno -> 0
    | Sestavljeno (l, _, d) -> visina l - visina d

let uravnotezi drevo =
    match drevo with
    | Prazno -> Prazno
    | Sestavljeno (l, x, d)
        when razlika drevo = 2 && razlika l = 1 ->
        zavrti_desno drevo
    | Sestavljeno (l, x, d)
        when razlika drevo = 2  ->
        Sestavljeno (zavrti_levo l, x, d) |> zavrti_desno
    | Sestavljeno (l, x, d)
        when razlika drevo = -2 && razlika d = -1 ->
        zavrti_levo drevo
    | Sestavljeno (l, x, d)
        when razlika drevo = -2 ->
        Sestavljeno (l, x, zavrti_desno d) |> zavrti_levo
    | _ -> drevo

let rec dodaj x = function
  | Prazno -> Sestavljeno (Prazno, x, Prazno)
  | Sestavljeno (l, y, d) ->
      if x = y then
        Sestavljeno (l, y, d)
      else if x < y then
        Sestavljeno (dodaj x l, y, d) |> uravnotezi
      else
        Sestavljeno (l, y, dodaj x d) |> uravnotezi
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
let primer = seznam_zaporednih 1000

let n = stopaj stevilo_razlicnih primer

let _ = print_endline ("Število različnih: " ^ string_of_int n)
