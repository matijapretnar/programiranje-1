type 'a drevo =
  | Prazno
  | Sestavljeno of int * 'a drevo * 'a * 'a drevo

let prazna_vreca = Prazno

let rec velikost = function
  | Prazno -> 0
  | Sestavljeno (_, l, _, d) -> 1 + velikost l + velikost d

let rec vsebuje x = function
  | Prazno -> false
  | Sestavljeno (_, l, y, d) ->
      if x = y then
        true
      else if x < y then
        vsebuje x l
      else
        vsebuje x d

let rec visina = function
  | Prazno -> 0
  | Sestavljeno (h, _, _, _) -> h

let sestavljeno (l, x, d) =
  let h = 1 + max (visina l) (visina d) in
  Sestavljeno (h, l, x, d)
    
let zavrti_levo = function
    | Sestavljeno (h, l, x, Sestavljeno (dh, dl, y, dd)) ->
        sestavljeno (sestavljeno (l, x, dl), y, dd)
    | _ -> failwith "Tega drevesa ne morem zavrteti"

let zavrti_desno = function
    | Sestavljeno (_, Sestavljeno (_, ll, y, ld), x, d) ->
        sestavljeno (ll, y, sestavljeno (ld, x, d))
    | _ -> failwith "Tega drevesa ne morem zavrteti"


let razlika = function
    | Prazno -> 0
    | Sestavljeno (_, l, _, d) -> visina l - visina d

let uravnotezi drevo =
    match drevo with
    | Prazno -> Prazno
    | Sestavljeno (_, l, x, d)
        when razlika drevo = 2 && razlika l = 1 ->
        zavrti_desno drevo
    | Sestavljeno (_, l, x, d)
        when razlika drevo = 2  ->
        sestavljeno (zavrti_levo l, x, d) |> zavrti_desno
    | Sestavljeno (_, l, x, d)
        when razlika drevo = -2 && razlika d = -1 ->
        zavrti_levo drevo
    | Sestavljeno (_, l, x, d)
        when razlika drevo = -2 ->
        sestavljeno (l, x, zavrti_desno d) |> zavrti_levo
    | _ -> drevo

let rec dodaj x = function
  | Prazno -> sestavljeno (Prazno, x, Prazno)
  | Sestavljeno (_, l, y, d) ->
      if x = y then
        sestavljeno (l, y, d)
      else if x < y then
        sestavljeno (dodaj x l, y, d) |> uravnotezi
      else
        sestavljeno (l, y, dodaj x d) |> uravnotezi
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
let primer = seznam_zaporednih 100000

let n = stopaj stevilo_razlicnih primer

let _ = print_endline ("Število različnih: " ^ string_of_int n)
