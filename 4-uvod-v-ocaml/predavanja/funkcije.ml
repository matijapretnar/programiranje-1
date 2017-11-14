let se_boljsi_odgovor =
  let odgovor = min 8 7 * 6 in
  odgovor + 1

let pozdravi ime =
  if ime = "Matija" then
    "Dober dan, gospod predavatelj!"
  else if ime = "Phillip" || ime = "Žiga" then
    "Oj!"
  else
    "Dober dan, " ^ ime ^ "!"

let pozdravi = function
  | "Matija" -> "Dober dan, gospod predavatelj!"
  | "Phillip" | "Žiga" -> "Oj!"
  | "" -> "Pozdravljen, neznanec!"
  | "*" -> "Pozdravljena, zvezdica!"
 
let rec fakulteta = function
  | 0 -> 1
  | n -> n * fakulteta (n - 1)

let rec fib = function
  | 0 -> 0
  | 1 -> 1
  | n -> fib (n - 1) + fib (n - 2)

let hitri_fib n =
  let rec aux n a b =
    if n = 0 then a else aux (n - 1) b (a + b)
  in
  aux n 0 1

let rec je_sodo = function
  | 0 -> true
  | n -> je_liho (n - 1)

and je_liho = function
  | 0 -> false
  | n -> je_sodo (n - 1)

let razdalja (x1, y1) (x2, y2) =
  sqrt ((x1 -. x2) ** 2. +. (y1 -. y2) ** 2.)

let skalarni_produkt (x1, y1, z1) (x2, y2, z2) =
    x1 *. x2 +. y1 *. y2 +. z1 *. z2

let citiraj_knjigo avtorji naslov =
  match avtorji with
  | [] -> naslov
  | avtor :: [] -> avtor ^ ": " ^ naslov
  | prvi :: _ -> prvi ^ " in ostali: " ^ naslov

let rec skalarni_produkt sez1 sez2 =
  match sez1, sez2 with
  | [], [] -> 0.
  | glava1 :: rep1, glava2 :: rep2 ->
      glava1 *. glava2 +. skalarni_produkt rep1 rep2
  | _ -> failwith "Vektorja nimata enakega stevila komponent"

let rec vsota = function
  | [] -> 0
  | glava :: rep -> glava + vsota rep

let rec dolzina = function
  | [] -> 0
  | _ :: rep -> 1 + dolzina rep

let rec preslikaj f = function
  | [] -> []
  | glava :: rep -> f glava :: preslikaj f rep
