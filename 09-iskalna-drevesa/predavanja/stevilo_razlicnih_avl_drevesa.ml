type 'a drevo =
    | Prazno
    | Sestavljeno of 'a drevo * 'a * 'a drevo

let prazna = Prazno

let rec velikost = function
  | Prazno -> 0
  | Sestavljeno (l, _, d) -> 1 + velikost l + velikost d

let rec vsebuje x = function
  | Prazno -> false
  | Sestavljeno (l, y, d) when x < y -> vsebuje x l
  | Sestavljeno (l, y, d) when x > y -> vsebuje x d
  | drevo -> true

let zavrti_levo = function
    | Sestavljeno (l, x, Sestavljeno (dl, y, dd)) ->
        Sestavljeno (Sestavljeno (l, x, dl), y, dd)
    | _ -> failwith "Tega drevesa ne morem zavrteti"

let zavrti_desno = function
    | Sestavljeno (Sestavljeno (ll, y, dl), x, d) ->
        Sestavljeno (ll, y, Sestavljeno (dl, x, d))
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
  | Sestavljeno (l, y, d) when x < y -> Sestavljeno (dodaj x l, y, d) |> uravnotezi
  | Sestavljeno (l, y, d) when x > y -> Sestavljeno (l, y, dodaj x d) |> uravnotezi
  | drevo -> drevo

(* ------------------------------------------------------------------------- *)

let stevilo_razlicnih xs =
  let rec aux ze_videni = function
    | [] -> velikost ze_videni
    | x :: xs -> aux (dodaj x ze_videni) xs
  in
  aux Prazno xs

let nakljucni_seznam m n = List.init n (fun _ -> Random.int m)
let seznam_zaporednih n = List.init n (fun i -> i)

let stopaj f x =
  let zacetek = Sys.time () in
  let y = f x in
  let konec = Sys.time () in
  print_endline ("Porabljen čas: " ^ string_of_float (1000. *. (konec -. zacetek)) ^ "ms");
  y

(* let primer = nakljucni_seznam 10000 10000 *)
let primer = seznam_zaporednih 10000

let n = stopaj stevilo_razlicnih primer

;;

Random.self_init ();
print_endline ("Število različnih: " ^ string_of_int n)
