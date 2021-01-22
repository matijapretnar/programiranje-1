type naravno =
  | Nic
  | Nasl of naravno

let ena = Nasl Nic

let rec vsota m n =
  match m with
  | Nic -> n
  | Nasl m' -> vsota m' (Nasl n)  (* pozor, ni standardno, je pa repno-rekurzivno *)

let rec zmnozi m n =
  match m with
  | Nic -> Nic
  | Nasl m' -> vsota n (zmnozi m' n)

let dva = vsota ena ena
let stiri = zmnozi dva dva

type 'a seznam =
  | Prazen
  | Sestavljen of 'a * 'a seznam

(* type ('a, 'b) izmenjujoci_seznam =
  | Prazen
  | Sestavljen of 'a * ('b, 'a) izmenjujoci_seznam *)

let rec vsota sez =
    match sez with
    | Prazen -> 0
    | Sestavljen (glava, rep) -> glava + vsota rep

let rec dolzina sez =
    match sez with
    | Prazen -> 0
    | Sestavljen (_, rep) -> 1 + dolzina rep

(* [1; 2; 3] ... 1 :: 2 :: 3 :: [] *)
(* [1, 2, 3] ... Sestavljen (1, Sestavljen (2, Sestavljen (3, Prazen))) *)
let rec stakni sez1 sez2 =
    match sez1 with
    | [] -> sez2
    | glava1 :: rep1 -> glava1 :: stakni rep1 sez2

let rec preslikaj f sez =
    match sez with
    | [] -> []
    | glava :: rep -> f glava :: preslikaj f rep

let rec v_slovenscino =
  function
  | [] -> Prazen
  | glava :: rep -> Sestavljen (glava, v_slovenscino rep)