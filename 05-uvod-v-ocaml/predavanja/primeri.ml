let odgovor = min 8 7 * 6
let se_boljsi_odgovor = odgovor + 1

let pi = 4. *. atan 1.

let tau = 2. *. pi

let izboljsaj odgovor = let ena = 1 in odgovor + 1

let trikrat_na_nic f = let nic = 0 in f (f (f nic));;

let pozdravi = function
    | "Matija" -> "Dober dan, gospod predavatelj!"
    | "Filip" | "Katja" -> "Oj!"
    | "" -> "Živjo, kdor koli žes i?"
    | "*" -> "Živjo zvezdica!"
    | "**" -> "Živjo dve zvezdici!"
    | "***" -> "Živjo tri zvezdice!"
    | ime -> "Zdravo " ^ ime ^ "!"

let rec fakulteta = function
    | 0 -> 1
    | n -> n * fakulteta (n - 1)

let razdalja (x1, y1) (x2, y2) =
    let dx = x1 -. x2
    and dy = y1 -. y2
    in
    sqrt (dx ** 2. +. dy ** 2.)

let citiraj_knjigo avtorji naslov =
    match avtorji with
    | [] -> naslov
    | [avtor] -> avtor ^ ": " ^ naslov
    | prvi :: _ -> prvi ^ " in ostali: " ^ naslov

let rec skalarni_produkt xs ys =
    match (xs, ys) with
    | ([], []) -> 0.
    | (x :: xs', y :: ys') -> x *. y +. skalarni_produkt xs' ys'
    | _ -> invalid_arg "skalarni_produkt"