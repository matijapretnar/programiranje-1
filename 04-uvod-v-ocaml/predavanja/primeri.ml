let odgovor = min 8 7 * 6
let se_boljsi_odgovor = odgovor + 1

let pozdravi ime =
  if ime = "Matija" then
    "Dober dan, gospod predavatelj!"
  else if ime = "Phillip" || ime = "Žiga" then
    "Oj!"
  else
    "Dober dan, " ^ ime ^ "!"

let pozdravi ime =
  match ime with
  | "Matija" -> "Dober dan, gospod predavatelj!"
  | "Phillip" | "Žiga" -> "Oj!"
  | _ -> "Dober dan, " ^ ime ^ "!"

let pozdravi pozdrav_za_asistente = function
  | "Matija" -> "Dober dan, gospod predavatelj!"
  | "Phillip" | "Žiga" -> pozdrav_za_asistente
  | "" -> "Pozdravljen, nihče!"
  | "*" -> "Živijo, zvezdica!"

let rec fakulteta = function
  | 0 -> 1
  | n -> n * fakulteta (n - 1)

let rec pocasni_fib = function
  | 0 -> 0
  | 1 -> 1
  | n -> pocasni_fib (n - 1) + pocasni_fib (n - 2)

let rec je_liho = function
  | 0 -> false
  | n -> je_sodo (n - 1)

and je_sodo = function
  | 0 -> true
  | n -> je_liho (n - 1)

let skalarni_produkt_3 (x1, y1, z1) (x2, y2, z2) =
  x1 *. x2 +. y1 *. y2 +. z1 *. z2

let citiraj_knjigo avtorji naslov =
  match avtorji with
  | [] -> naslov
  | [avtor] -> avtor ^ ": " ^ naslov
  (* | pomembni @ _ -> String.concat ", " pomembni ^ ": " ^ naslov *)
  | prvi :: _ -> prvi ^ " in ostali: " ^ naslov

let za_lase_privlecena_funkcija = function
  | [] -> 0
  | [(x, _); (y, z)] -> x + y + z
  | ((_, 10) :: _) -> 40
  | ((_, 0) :: _) -> 20

let rec skalarni_produkt xs ys =
    match (xs, ys) with
    | ([], []) -> 0.
    | (x :: xs', y :: ys') -> x *. y +. skalarni_produkt xs' ys'
