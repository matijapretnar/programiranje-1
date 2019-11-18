(* type barva = string *)

type barva =
  | Rdeca
  | Zelena
  | Mod_AOOra

let moje_barve = [Rdeca; Zelena]

type geometrijski_objekt =
  | Tocka
  | Krog of float
  | Pravokotnik of float * float

type ('a, 'b) vsota =
  | Iota1 of 'a
  | Iota2 of 'b

let moji_objekti = [Iota1 Tocka; Iota1 Tocka; Iota2 Zelena; Iota1 (Krog 3.)]

let povrsina obj =
    match obj with
    | Tocka -> 0.
    | Krog r -> 3.14 *. r *. r
    | Pravokotnik (h, w) -> h *. w

type leto = Leto of int

let delno_definirana_povrsina (Krog r) = 3.14 *. r *. r

let koliko_let_je_minilo (Leto x) = 2019 - x

type cevelj = Cevelj of float
type meter = Meter of float

let razdalja_do_marsa = Meter 10000.
let hitrost_plovila_na_sekundo = Cevelj 1.

let pretvori_v_metre (Cevelj x) = Meter (0.3048 *. x)
let cas_v_sekundah (Meter razdalja) (Meter hitrost_na_sekundo) = razdalja /. hitrost_na_sekundo
let cas_potovanja = cas_v_sekundah razdalja_do_marsa (pretvori_v_metre hitrost_plovila_na_sekundo)

let nevarna_glava (x :: _) = x

let velika_glava xs = nevarna_glava xs > 100

let glava sez =
  match sez with
  | g :: rep -> Some g
  | [] -> None

let velika_glava xs = glava xs > 100

let deli m n =
  if n = 0 then None else Some (m / n)

let f x y z = x + (y / z)

let f x y z =
    match deli y z with
    | Some k -> Some (x + k)
    | None -> None
