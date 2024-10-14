module type Mnozica = sig
  type 'a mn
  val prazna : 'a mn
  val dodaj : 'a -> 'a mn -> 'a mn
  val vsebuje : 'a -> 'a mn -> bool
  val velikost : 'a mn -> int
end

module MnozicaSeznami : Mnozica = struct
  type 'a mn = 'a list

  let prazna = []

  let velikost m = List.length m

  let vsebuje x m = List.mem x m

  let dodaj x m = if vsebuje x m then m else x :: m
end

module MnozicaDrevesa : Mnozica = struct
  type 'a drevo =
    | Prazno
    | Sestavljeno of 'a drevo * 'a * 'a drevo
  
  type 'a mn = 'a drevo

  let prazna = Prazno

  let rec velikost = function
    | Prazno -> 0
    | Sestavljeno (l, _, d) -> 1 + velikost l + velikost d

  let rec vsebuje x = function
    | Prazno -> false
    | Sestavljeno (l, y, d) when x < y -> vsebuje x l
    | Sestavljeno (l, y, d) when x > y -> vsebuje x d
    | drevo -> true

  let rec dodaj x = function
    | Prazno -> Sestavljeno (Prazno, x, Prazno)
    | Sestavljeno (l, y, d) when x < y -> Sestavljeno (dodaj x l, y, d)
    | Sestavljeno (l, y, d) when x > y -> Sestavljeno (l, y, dodaj x d)
    | drevo -> drevo
end

module MnozicaNeumnaAVLDrevesa : Mnozica = struct
    type 'a drevo =
        | Prazno
        | Sestavljeno of 'a drevo * 'a * 'a drevo

    type 'a mn = 'a drevo

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

end


module MnozicaAVLDrevesa : Mnozica = struct
     type 'a drevo =
        | Prazno
        | Sestavljeno of 'a drevo * 'a * 'a drevo * int

    type 'a mn = 'a drevo

    let prazna = Prazno

    let rec velikost = function
      | Prazno -> 0
      | Sestavljeno (l, _, d, _) -> 1 + velikost l + velikost d

    let rec vsebuje x = function
      | Prazno -> false
      | Sestavljeno (l, y, d, _) when x < y -> vsebuje x l
      | Sestavljeno (l, y, d, _) when x > y -> vsebuje x d
      | drevo -> true

    let rec visina = function
        | Prazno -> 0
        | Sestavljeno (_, _, _, h) -> h

    let sestavljeno (l, x, d) =
        let h = 1 + max (visina l) (visina d) in
        Sestavljeno (l, x, d, h)

    let zavrti_levo = function
        | Sestavljeno (l, x, Sestavljeno (dl, y, dd, _), _) ->
            sestavljeno (sestavljeno (l, x, dl), y, dd)
        | _ -> failwith "Tega drevesa ne morem zavrteti"

    let zavrti_desno = function
        | Sestavljeno (Sestavljeno (ll, y, dl, _), x, d, _) ->
            sestavljeno (ll, y, sestavljeno (dl, x, d))
        | _ -> failwith "Tega drevesa ne morem zavrteti"

    let rec visina = function
        | Prazno -> 0
        | Sestavljeno (_, _, _, h) -> h

    let razlika = function
        | Prazno -> 0
        | Sestavljeno (l, _, d, _) -> visina l - visina d

    let uravnotezi drevo =
        match drevo with
        | Prazno -> Prazno
        | Sestavljeno (l, x, d, _)
            when razlika drevo = 2 && razlika l = 1 ->
            zavrti_desno drevo
        | Sestavljeno (l, x, d, _)
            when razlika drevo = 2  ->
            sestavljeno (zavrti_levo l, x, d) |> zavrti_desno
        | Sestavljeno (l, x, d, _)
            when razlika drevo = -2 && razlika d = -1 ->
            zavrti_levo drevo
        | Sestavljeno (l, x, d, _)
            when razlika drevo = -2 ->
            sestavljeno (l, x, zavrti_desno d) |> zavrti_levo
        | _ -> drevo

    let rec dodaj x = function
      | Prazno -> sestavljeno (Prazno, x, Prazno)
      | Sestavljeno (l, y, d, _) when x < y -> sestavljeno (dodaj x l, y, d) |> uravnotezi
      | Sestavljeno (l, y, d, _) when x > y -> sestavljeno (l, y, dodaj x d) |> uravnotezi
      | drevo -> drevo
end

(* module M = MnozicaSeznami *)
(* module M = MnozicaDrevesa *)
(* module M = MnozicaNeumnaAVLDrevesa *)
module M = MnozicaAVLDrevesa

let stevilo_razlicnih xs =
  let rec aux ze_videni = function
    | [] -> M.velikost ze_videni
    | x :: xs -> aux (M.dodaj x ze_videni) xs
  in
  aux M.prazna xs

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
nakljucni_seznam 5000 5000
(* seznam_zaporednih 5000 *)
|> stopaj stevilo_razlicnih
|> string_of_int
|> print_endline
