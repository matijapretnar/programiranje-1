type izraz =
  | Stevilo of int
  | Plus of izraz * izraz
  | Minus of izraz
  | Krat of izraz * izraz

let moj_izraz =
  Minus (
    Krat (Stevilo 5, Plus (Stevilo 2, Stevilo 7))
  )

let rec vrednost =
  function
  | Stevilo x -> x
  | Plus (izr1, izr2) -> vrednost izr1 + vrednost izr2
  | Minus izr -> - vrednost izr
  | Krat (izr1, izr2) -> vrednost izr1 * vrednost izr2
