
type izdelek = ..
type naslov = ..
type papir = ..

type hitrost_dostave =
    | Obicajna
    | Hitra
    | Ekspresna

type dostava =
    | Poslji of {
        naslov: naslov;
        hitrost: hitrost_dostave
      }
    | PoPovzetju

type zavijanje =
    | BrezZavijanja
    | NezavitoDarilo
    | ZavitoDarilo of papir

type narocilo = {
  naroceni_izdelki : izdelek list;
  vrsta_dostave : dostava;
  vrsta_zavijanja : zavijanje;
}
