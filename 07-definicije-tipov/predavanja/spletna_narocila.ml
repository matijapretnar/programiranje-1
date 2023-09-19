type izdelek = Izdelek of string

type posta = {
  postna_stevilka : int;
  kraj : string
}

type naslov = {
  ulica : string;
  posta : posta
}

type papir =
  | Rjav
  | Krep
  | Svetlec

type vrsta_darila =
  | NiDarilo
  | BrezRacuna
  | Zavito of papir

type telefon = Telefon of string

type vrsta_dostave =
  | Prevzem
  | ObicajnaDostava of naslov
  | HitraDostava of {
      naslov : naslov;
      telefon : telefon
    }


type narocilo = {
  naroceni_izdelki : izdelek list;
  dostava : vrsta_dostave;
  naslov_placnika : naslov;
  darilo : vrsta_darila;
}
