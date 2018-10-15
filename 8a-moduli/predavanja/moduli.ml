module type Mnozica =
sig
    type 'a t

    val prazna : 'a t
    val dodaj : 'a -> 'a t -> 'a t
    val vsebuje : 'a -> 'a t -> bool
    val velikost : 'a t -> int
end

module Mnozica_s_seznami : Mnozica =
struct
    type 'a t = 'a list

    let prazna = []

    let dodaj x xs = x :: xs

    let vsebuje = List.mem

    let velikost = List.length
end

module Mnozica_z_iskalnimi_drevesi : Mnozica =
struct
    type 'a t =
      | Prazno
      | Sestavljeno of 'a t * 'a * 'a t

    let prazna = Prazno

    let rec dodaj x = function
      | Prazno -> Sestavljeno (Prazno, x, Prazno)
      | Sestavljeno (l, y, d) as drevo ->
          if x < y then
            Sestavljeno (dodaj x l, y, d)
          else if y < x then
            Sestavljeno (l, y, dodaj x d)
          else
            drevo

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
      | Sestavljeno (l, _, d) -> 1 + velikost l + velikost d
end

module Mnozica_z_narobe_narejenimi_AVL_drevesi : Mnozica =
struct
    type 'a t =
        | Prazno
        | Sestavljeno of 'a t * 'a * 'a t

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

module Mnozica_z_AVL_drevesi : Mnozica =
struct
    type 'a t =
        | Prazno
        | Sestavljeno of int * 'a t * 'a * 'a t

    let prazna = Prazno

    let rec vsebuje x = function
      | Prazno -> false
      | Sestavljeno (_, l, y, d) ->
          if x < y then
            vsebuje x l
          else if y < x then
            vsebuje x d
          else
            x = y

    let rec velikost = function
      | Prazno -> 0
      | Sestavljeno (_, l, _, d) ->
            1 + velikost l + velikost d

    let rec visina = function
      | Prazno -> 0
      | Sestavljeno (h, _, _, _) -> h

    let sestavljeno (l, x, d) =
      let h = 1 + max (visina l) (visina d) in
      Sestavljeno (h, l, x, d)

    let zavrti_levo = function
      | Sestavljeno (_, l, x, Sestavljeno (_, dl, y, dd)) ->
          sestavljeno (sestavljeno (l, x, dl), y, dd)
      | _ -> assert false

    let zavrti_desno = function
      | Sestavljeno (_, Sestavljeno (_, ll, y, ld), x, d) ->
          sestavljeno (ll, y, sestavljeno (ld, x, d))
      | _ -> assert false

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
      | Sestavljeno (_, l, y, d) as drevo ->
          if x < y then
            sestavljeno (dodaj x l, y, d) |> uravnotezi
          else if y < x then
            sestavljeno (l, y, dodaj x d) |> uravnotezi
          else
            drevo
end

module M = Mnozica_z_AVL_drevesi

let stevilo_razlicnih xs =
  let rec aux ze_videni = function
    | [] -> M.velikost ze_videni
    | x :: xs when M.vsebuje x ze_videni ->
        aux ze_videni xs
    | x :: xs -> aux (M.dodaj x ze_videni) xs
  in
  aux M.prazna xs

let rec nakljucni_seznam a b = function
  | 0 -> []
  | n -> a + Random.int (b - a + 1) :: nakljucni_seznam a b (n - 1)

let rec urejeni_seznam a = function
  | 0 -> []
  | n -> a :: urejeni_seznam (a + 1) (n - 1)
