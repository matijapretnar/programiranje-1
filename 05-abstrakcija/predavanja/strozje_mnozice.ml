type primerjava = Manjsi | Enak | Vecji
module type Urejenost =
sig
    type t
    val primerjaj : t -> t -> primerjava
end

module CelaStevila : Urejenost with type t = int =
struct
    type t = int
    let primerjaj x y =
        if x < y then
            Manjsi
        else if x > y then
            Vecji
        else
            Enak
end

module type Mnozica = sig
  type elt
  type mn
  val prazna : mn
  val dodaj : elt -> mn -> mn
  val vsebuje : elt -> mn -> bool
  val velikost : mn -> int
end

module MnozicaSeznami (U : Urejenost) : Mnozica = struct
  type elt = U.t
  type mn = elt list

  let prazna = []

  let velikost m = List.length m

  let vsebuje x m = List.exists (fun y -> U.primerjaj x y = Enak) m

  let dodaj x m = if vsebuje x m then m else x :: m
end

module MnozicaDrevesa (U : Urejenost) : Mnozica = struct
  type 'a drevo =
    | Prazno
    | Sestavljeno of 'a drevo * 'a * 'a drevo
  
  type elt = U.t
  type mn = elt drevo

  let prazna = Prazno

  let rec velikost = function
    | Prazno -> 0
    | Sestavljeno (l, _, d) -> 1 + velikost l + velikost d

  let rec vsebuje x = function
    | Prazno -> false
    | Sestavljeno (l, y, d) ->
        begin match U.primerjaj x y with
        | Manjsi -> vsebuje x l
        | Vecji -> vsebuje x d
        | Enak -> true
        end

  let rec dodaj x = function
    | Prazno -> Sestavljeno (Prazno, x, Prazno)
    | Sestavljeno (l, y, d) as drevo ->
        begin match U.primerjaj x y with
        | Manjsi -> Sestavljeno (dodaj x l, y, d)
        | Vecji -> Sestavljeno (l, y, dodaj x d)
        | Enak -> drevo
        end
end

module MnozicaNeumnaAVLDrevesa (U : Urejenost) : Mnozica = struct
    type 'a drevo =
        | Prazno
        | Sestavljeno of 'a drevo * 'a * 'a drevo

    type elt = U.t
    type mn = elt drevo

    let prazna = Prazno

    let rec velikost = function
      | Prazno -> 0
      | Sestavljeno (l, _, d) -> 1 + velikost l + velikost d

    let rec vsebuje x = function
        | Prazno -> false
        | Sestavljeno (l, y, d) ->
            begin match U.primerjaj x y with
            | Manjsi -> vsebuje x l
            | Vecji -> vsebuje x d
            | Enak -> true
            end

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
    | Sestavljeno (l, y, d) as drevo ->
        begin match U.primerjaj x y with
        | Manjsi -> Sestavljeno (dodaj x l, y, d) |> uravnotezi
        | Vecji -> Sestavljeno (l, y, dodaj x d) |> uravnotezi
        | Enak -> drevo
        end

end


module MnozicaAVLDrevesa (U : Urejenost) : Mnozica = struct
     type 'a drevo =
        | Prazno
        | Sestavljeno of 'a drevo * 'a * 'a drevo * int

    type elt = U.t
    type mn = elt drevo

    let prazna = Prazno

    let rec velikost = function
      | Prazno -> 0
      | Sestavljeno (l, _, d, _) -> 1 + velikost l + velikost d

    let rec vsebuje x = function
        | Prazno -> false
        | Sestavljeno (l, y, d, _) ->
            begin match U.primerjaj x y with
            | Manjsi -> vsebuje x l
            | Vecji -> vsebuje x d
            | Enak -> true
            end

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
        | Sestavljeno (l, y, d, _) as drevo ->
            begin match U.primerjaj x y with
            | Manjsi -> sestavljeno (dodaj x l, y, d) |> uravnotezi
            | Vecji -> sestavljeno (l, y, dodaj x d) |> uravnotezi
            | Enak -> drevo
            end
end

(* module M = MnozicaSeznami(CelaStevila) *)
(* module M = MnozicaDrevesa(CelaStevila) *)
(* module M = MnozicaNeumnaAVLDrevesa(CelaStevila) *)
module M = MnozicaAVLDrevesa(CelaStevila)

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
