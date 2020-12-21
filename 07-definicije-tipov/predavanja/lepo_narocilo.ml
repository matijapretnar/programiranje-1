type naslov = {
  ulica : string;
  postna_stevilka : int
}

type izdelek = string

type papir =
| Grd
| Blescec

type tip_narocila =
| Obicajno
| BrezRacuna
| ZavitoDarilo of papir

type dostava =
| OsebniPrevzem
| ObicajnaDostava of { naslov : naslov }
| HitraDostava of { naslov : naslov; telefon : string }

type narocilo = {
  izdelki : (izdelek * int) list;
  dostava : dostava;
  tip_narocila : tip_narocila;
}















let cena_papirja = function
| Grd -> 1
| Blescec -> 10

let cena_narocila narocilo =
  let cena_izdelkov =
    narocilo.izdelki
    |> List.map snd
    |> List.fold_left (+) 0
  and cena_zavijanja =
    match narocilo.tip_narocila with
    | Obicajno | BrezRacuna -> 0
    | ZavitoDarilo papir -> cena_papirja papir
  in
  let cena_dostave =
    match narocilo.dostava with
    | OsebniPrevzem -> 0
    | ObicajnaDostava _ when cena_izdelkov < 50 -> 0
    | ObicajnaDostava _ -> 5
    | HitraDostava _ ->
        if cena_izdelkov < 100 then 10 else 0
    in
    cena_izdelkov + cena_zavijanja + cena_dostave