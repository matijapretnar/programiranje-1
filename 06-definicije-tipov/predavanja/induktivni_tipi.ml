type izraz =
  | Stevilo of int
  | Minus of izraz
  | Plus of izraz * izraz
  | Krat of izraz * izraz

let rec izracunaj = function
  | Stevilo n -> n
  | Minus izr -> -(izracunaj izr)
  | Plus (izr1, izr2) -> izracunaj izr1 + izracunaj izr2
  | Krat (izr1, izr2) -> izracunaj izr1 * izracunaj izr2

type 'a seznam =
  | Prazen
  | Sestavljen of 'a * 'a seznam

type niz =
  | Prazen
  | Sestavljen of char * niz
