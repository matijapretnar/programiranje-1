type barva' = string

let moje_barve' = ["rumena"; "rdeca"; "miza"; "modra"]

type barva =
  | Rdeca
  | Rumena
  | Modra
  | Zelena

let moje_barve = [Rdeca; Rumena; Modra; Miza; Modra]

let mi_je_vsec = function
  | Rdeca -> true
  | Rumena -> false
  | Modra -> true
  | Zelena -> true
