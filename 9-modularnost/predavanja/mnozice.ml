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

  let vsebuje = List.mem
  
  let dodaj x mn = if vsebuje x mn then mn else x :: mn

  let velikost = List.length
end

module MnozicaDrevesa : Mnozica = struct
    type 'a mn =
        | Prazno
        | Sestavljeno of 'a mn * 'a * 'a mn

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
end

module MnozicaNeumnaAVLDrevesa : Mnozica = struct
    type 'a mn =
        | Prazno
        | Sestavljeno of 'a mn * 'a * 'a mn

    let prazna = Prazno

    let rec vsebuje x = function
      | Prazno -> false
      | Sestavljeno (l, y, d) ->
          if x < y then
            vsebuje x l
          else if y < x then
            vsebuje x d
          else
            x = y

    let rec velikost = function
      | Prazno -> 0
      | Sestavljeno (l, _, d) ->
            1 + velikost l + velikost d

    let zavrti_levo = function
      | Sestavljeno (l, x, Sestavljeno (dl, y, dd)) ->
          Sestavljeno (Sestavljeno (l, x, dl), y, dd)
      | _ -> assert false

    let zavrti_desno = function
      | Sestavljeno (Sestavljeno (ll, y, dl), x, d) ->
          Sestavljeno (ll, y, Sestavljeno (dl, x, d))
      | _ -> assert false

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
      | Sestavljeno (l, y, d) as drevo ->
          if x < y then
            Sestavljeno (dodaj x l, y, d) |> uravnotezi
          else if y < x then
            Sestavljeno (l, y, dodaj x d) |> uravnotezi
          else
            drevo
end


module MnozicaAVLDrevesa : Mnozica = struct
    type 'a mn =
        | Prazno
        | Sestavljeno of 'a mn * 'a * 'a mn * int
    
    let rec visina = function
      | Prazno -> 0
      | Sestavljeno (l, _, d, h) -> h

    let sestavljeno (l, x, d) =
      let h = 1 + max (visina l) (visina d) in
      Sestavljeno (l, x, d, h)

    let prazna = Prazno

    let rec vsebuje x = function
      | Prazno -> false
      | Sestavljeno (l, y, d, _) ->
          if x < y then
            vsebuje x l
          else if y < x then
            vsebuje x d
          else
            x = y

    let rec velikost = function
      | Prazno -> 0
      | Sestavljeno (l, _, d, _) ->
            1 + velikost l + velikost d

    let zavrti_levo = function
      | Sestavljeno (l, x, Sestavljeno (dl, y, dd, _), _) ->
          sestavljeno (sestavljeno (l, x, dl), y, dd)
      | _ -> assert false

    let zavrti_desno = function
      | Sestavljeno (Sestavljeno (ll, y, dl, _), x, d, _) ->
          sestavljeno (ll, y, sestavljeno (dl, x, d))
      | _ -> assert false

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
      | Prazno -> Sestavljeno (Prazno, x, Prazno, 0)
      | Sestavljeno (l, y, d, h) as drevo ->
          if x < y then
            sestavljeno (dodaj x l, y, d) |> uravnotezi
          else if y < x then
            sestavljeno (l, y, dodaj x d) |> uravnotezi
          else
            drevo
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
