type barva = string

let moje_barve : barva list = ["rdeča"; "modra"; "rumena"; "stol"]

let je_svetla (b : barva) : bool =
  match b with
  | "rumena" -> true
  | "modra" -> false
  | "rdeča" -> false
  | "črna" -> false
  | "rjava" -> false
  | _ -> true

let je_noc_svetla = je_svetla "zelo temna noč"

type barva' =
  | Rdeca
  | Zelena
  | Rumena
  | Modra

let moje_barve : barva' list = [Rdeca; Modra; Rumena; Stol]


type stanje_narocila =
  | VKosarici
  | Placano
  | Pripravljeno
  | Poslano
  | Prejeto

type tocka = { x : float; y : float }

type geometrijski_objekt =
  | Tocka of tocka
  | Daljica of tocka * tocka
  | Kroznica of { sredisce : tocka; radij : float }

let moja_slika = [
  Tocka {x = 0.; y = 0.};
  Daljica ({x = 0.; y = 0.}, {x = 1.; y = 1.});
  Daljica ({x = 0.; y = 0.}, {x = -1.; y = 1.});
  Kroznica {sredisce = {x = -1.; y = 1.}; radij = 2. ** 0.5}
]
let koliko_svincnika_potrebujem obj =
  match obj with
  | Tocka _ -> 0.
  | Daljica (p, q) -> ((p.x -. q.x) ** 2. +. (p.y -. q.y) ** 2.) ** 0.5
  | Kroznica k -> 3.14 *. k.radij ** 2.

type cevlji = Cevlji of float
type metri = Metri of float

let visina_donalda = Cevlji 6.
let visina_boruta = Metri 2.

let iz_cevljev_v_metre (Cevlji ft) = Metri (0.3048 *. ft)
let kdo_je_vecji =
  if iz_cevljev_v_metre visina_donalda > visina_boruta then
    "Donald"
  else
    "Borut"

type ('k, 'v) slovar = ('k * 'v) list

type 'a morebitni =
  | Nimamo
  | Imamo of 'a

let glava_seznama =
  function
  | [] -> Nimamo
  | x :: _ -> Imamo x

let dangerous_head =
  function
  | x :: _ -> x

let safe_head =
  function
  | [] -> None
  | x :: _ -> Some x

let big_dangerous_head lst =
  dangerous_head lst > 100

let big_safe_head lst =
  match safe_head lst with
  | Some hd -> hd > 100
  | None -> false


type int_ali_float =
| Int of int
| Float of float

let vsota x y =
match x, y with
| Int m, Int n -> Int (m + n)
| Int m, Float y -> Float (float_of_int m +. y)