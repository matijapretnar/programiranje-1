type izraz =
  | Stevilo of int
  | Plus of izraz * izraz
  | Minus of izraz * izraz
  | Krat of izraz * izraz
  | Kvadriraj of izraz

let rec izracunaj izraz =
    match izraz with
    | Stevilo n -> n
    | Plus (i1, i2) -> izracunaj i1 + izracunaj i2
    | Minus (i1, i2) -> izracunaj i1 - izracunaj i2
    | Krat (i1, i2) -> izracunaj i1 * izracunaj i2
    | Kvadriraj i -> let n = izracunaj i in n * n

type boole = True | False

type primerjava = Vecje | Manjse | Enako

type predznak = Plus | Minus | Nic

type 'a seznam = Prazen | Sestavljen of 'a * 'a seznam

type naravno_stevilo = Nic | Naslednik of naravno_stevilo

let rec sestej m = function
  | Nic -> m
  | Naslednik n -> Naslednik (sestej m n)

let rec pretvori_v_int = function
  | Nic -> 0
  | Naslednik n -> pretvori_v_int n + 1

let rec pretvori_iz_int n =
  if n == 0 then Nic else Naslednik (pretvori_iz_int (n - 1))

type drevo = Prazno | Sestavljeno of drevo * int * drevo

let boljsa_glava = function
  | [] -> None
  | x :: _ -> Some x

let varno_deljenje m n =
    if n == 0 then None else Some (m / n)

let uporabi_privzeto privzeta_vrednost rezultat =
    match rezultat with
    | None -> privzeta_vrednost
    | Some vrednost -> vrednost

let mogoce_uporabi f = function
  | None -> None
  | Some x -> Some (f x)
