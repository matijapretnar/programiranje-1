module type MNOZICA = sig
  type 'a t

  val vsebuje : 'a t -> 'a -> bool
  val prazna : 'a t
  val velikost : 'a t -> int
  val dodaj : 'a -> 'a t -> 'a t
end

module MnozicaPrekSeznamov : MNOZICA = struct
  type 'a t = 'a list

  let vsebuje mn x = List.mem x mn
  let prazna = []
  let velikost = List.length
  let dodaj x mn = if vsebuje mn x then mn else x :: mn
end

module MnozicaPrekIskalnihDreves : MNOZICA = struct
  type 'a t = Prazno | Sestavljeno of 'a t * 'a * 'a t

  (* type comparison = Equal | Less | Greater *)

  let rec vsebuje mn x =
    match mn with
    | Prazno -> false
    | Sestavljeno (l, y, d) when x = y -> true
    | Sestavljeno (l, y, d) when x < y -> vsebuje l x
    | Sestavljeno (l, y, d) when x > y -> vsebuje d x
    | _ -> assert false

  let prazna = Prazno

  let rec velikost = function
    | Prazno -> 0
    | Sestavljeno (l, _, d) -> 1 + velikost l + velikost d

  let rec podvoji = function
    | Prazno -> Prazno
    | Sestavljeno (l, x, d) -> Sestavljeno (podvoji l, x, podvoji d)

  let rec dodaj x mn =
    match mn with
    | Prazno -> Sestavljeno (Prazno, x, Prazno)
    | Sestavljeno (l, y, d) when x = y -> mn
    | Sestavljeno (l, y, d) when x < y -> Sestavljeno (dodaj x l, y, d)
    | Sestavljeno (l, y, d) when x > y -> Sestavljeno (l, y, dodaj x d)
    | _ -> assert false
end

module MnozicaPrekAVLDreves : MNOZICA = struct
  type 'a t = Prazno | Sestavljeno of 'a t * 'a * 'a t

  let rec vsebuje mn x =
    match mn with
    | Prazno -> false
    | Sestavljeno (l, y, d) when x = y -> true
    | Sestavljeno (l, y, d) when x < y -> vsebuje l x
    | Sestavljeno (l, y, d) when x > y -> vsebuje d x
    | _ -> assert false

  let prazna = Prazno

  let rec velikost = function
    | Prazno -> 0
    | Sestavljeno (l, _, d) -> 1 + velikost l + velikost d

  let rec podvoji = function
    | Prazno -> Prazno
    | Sestavljeno (l, x, d) -> Sestavljeno (podvoji l, x, podvoji d)

  let zavrti_levo = function
    | Sestavljeno (l, x, Sestavljeno (dl, y, dd)) ->
        Sestavljeno (Sestavljeno (l, x, dl), y, dd)
    | _ -> failwith "Tega drevesa ne morem zavrteti"

  let zavrti_desno = function
    | Sestavljeno (Sestavljeno (ll, y, ld), x, d) ->
        Sestavljeno (ll, y, Sestavljeno (ld, x, d))
    | _ -> failwith "Tega drevesa ne morem zavrteti"

  let rec visina drevo =
    match drevo with
    | Prazno -> 0
    | Sestavljeno (l, _, d) -> 1 + max (visina l) (visina d)

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
        if x < vrednost then isci x l
        else if x > vrednost then isci x d
        else true

  let rec dodaj x drevo =
    match drevo with
    | Prazno -> Sestavljeno (Prazno, x, Prazno)
    | Sestavljeno (l, vrednost, d) ->
        if x < vrednost then Sestavljeno (dodaj x l, vrednost, d) |> uravnotezi
        else if x > vrednost then
          Sestavljeno (l, vrednost, dodaj x d) |> uravnotezi
        else drevo
end

module MnozicaPrekPravilnoImplementiranihAVLDreves : MNOZICA = struct
  type 'a t = Prazno | Sestavljeno of int * 'a t * 'a * 'a t

  let rec vsebuje mn x =
    match mn with
    | Prazno -> false
    | Sestavljeno (_, l, y, d) when x = y -> true
    | Sestavljeno (_, l, y, d) when x < y -> vsebuje l x
    | Sestavljeno (_, l, y, d) when x > y -> vsebuje d x
    | _ -> assert false

  let prazna = Prazno

  let rec velikost = function
    | Prazno -> 0
    | Sestavljeno (_, l, _, d) -> 1 + velikost l + velikost d

  let visina drevo =
    match drevo with
    | Prazno -> 0
    | Sestavljeno (h, _, _, _) -> h

  let sestavljeno (l, x, d) =
    Sestavljeno (1 + max (visina l) (visina d), l, x, d)

  let zavrti_levo = function
    | Sestavljeno (_, l, x, Sestavljeno (_, dl, y, dd)) ->
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
        if x < vrednost then isci x l
        else if x > vrednost then isci x d
        else true

  let rec dodaj x drevo =
    match drevo with
    | Prazno -> Sestavljeno (1, Prazno, x, Prazno)
    | Sestavljeno (h, l, vrednost, d) ->
        if x < vrednost then sestavljeno (dodaj x l, vrednost, d) |> uravnotezi
        else if x > vrednost then
          sestavljeno (l, vrednost, dodaj x d) |> uravnotezi
        else drevo
end

module Mnozica = MnozicaPrekPravilnoImplementiranihAVLDreves

let stevilo_razlicnih xs =
  let rec aux ze_videni = function
    | [] -> Mnozica.velikost ze_videni
    | x :: xs -> aux (Mnozica.dodaj x ze_videni) xs
  in
  aux Mnozica.prazna xs

let seznam_nakljucnih m n = List.init n (fun _ -> Random.int m)
let seznam_zaporednih n = List.init n (fun i -> i)

let stopaj f x =
  let zacetek = Sys.time () in
  let y = f x in
  let konec = Sys.time () in
  let izpis =
    Printf.sprintf "Porabljen čas: %f ms" (1000. *. (konec -. zacetek))
  in
  print_endline izpis;
  y

let rezultati =
  [ 1000; 2000; 4000; 8000; 16000 ]
  |> List.map seznam_zaporednih
  |> List.map (stopaj stevilo_razlicnih)
  |> List.map print_int
