module Seznam = List

let obrni_in_povecaj sez =
  sez |> Seznam.rev |> Seznam.map succ

module type Mnozica =
sig
    type 'a t

    val prazna : 'a t
    val dodaj : 'a -> 'a t -> 'a t
    val vsebuje : 'a -> 'a t -> bool
    val velikost : 'a t -> int
end


module Mnozice_s_seznami : Mnozica =
struct
    type 'a t = 'a list

    let prazna = []

    let dodaj x xs = x :: xs

    let vsebuje x xs = List.mem x xs

    let velikost xs = List.length xs
end


module Mnozice_s_drevesi : Mnozica =
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
      | Sestavljeno (l, _, d) ->
            1 + velikost l + velikost d
end


module Primer (M : Mnozica) =
struct
  let stevilo_razlicnih xs =
    let rec aux ze_videni = function
      | [] -> M.velikost ze_videni
      | x :: xs when M.vsebuje x ze_videni ->
          aux ze_videni xs
      | x :: xs -> aux (M.dodaj x ze_videni) xs
    in
    aux M.prazna xs
end

