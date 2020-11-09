type izdelek = ..
type naslov = ..
type papir = ..
type dnk = ..

type pakiranje =
  | Obicajno
  | BrezRacuna
  | KotDarilo of papir

type dostava =
  | EkspresnoHitraDostava of dnk
  | PrevzemVTrgovini
  | HitraDostava of naslov
  | ObicajnaDostava of naslov

type narocilo = {
  naroceni_izdelki : izdelek list;
  dostava : dostava;
  pakiranje : pakiranje;
}

let ali_naj_zavijem narocilo =
  match narocilo.pakiranje with
  | Obicajno -> false
  | BrezRacuna -> false
  | KotDarilo _ -> true

let ali_naj_dodam_racun narocilo =
  match narocilo.pakiranje with
  | Obicajno -> true
  | BrezRacuna -> false
  | KotDarilo _ -> false

let vmesnik ... : string -> narocilo =
  narocilo