module type StrogaMnozica = sig
  type mn
  type elt
  val prazna : mn
  val dodaj : elt -> mn -> mn
  val vsebuje : elt -> mn -> bool
  val velikost : mn -> int
end

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

let sta_enaka x y =
   match CelaStevila.primerjaj x y with
   | Enak -> true
   | _ -> false

let oceni_kakovost x =
  CelaStevila.primerjaj x 42

module MnozicaAVLDrevesa (U : Urejenost) : StrogaMnozica with type elt = U.t = struct
    type elt = U.t

    type mn =
        | Prazno
        | Sestavljeno of mn * elt * mn * int
    
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
          match U.primerjaj x y with
          | Manjsi -> vsebuje x l
          | Vecji -> vsebuje x d
          | Enak -> true

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
          match U.primerjaj x y with
          | Manjsi -> sestavljeno (dodaj x l, y, d) |> uravnotezi
          | Vecji -> sestavljeno (l, y, dodaj x d) |> uravnotezi
          | Enak -> drevo

end

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
