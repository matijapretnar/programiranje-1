type t = { avtomat : Avtomat.t; trak : Trak.t; stanje : Stanje.t }

let pozeni avtomat trak =
  { avtomat; trak; stanje = Avtomat.zacetno_stanje avtomat }

let avtomat { avtomat; _ } = avtomat
let trak { trak; _ } = trak
let stanje { stanje; _ } = stanje

let korak_naprej { avtomat; trak; stanje } =
  if Trak.je_na_koncu trak then None
  else
    let stanje' =
      Avtomat.prehodna_funkcija avtomat stanje (Trak.trenutni_znak trak)
    in
    match stanje' with
    | None -> None
    | Some stanje' ->
        Some { avtomat; trak = Trak.premakni_naprej trak; stanje = stanje' }

let je_v_sprejemnem_stanju { avtomat; stanje; _ } =
  Avtomat.je_sprejemno_stanje avtomat stanje
