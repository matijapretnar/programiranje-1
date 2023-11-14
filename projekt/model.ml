open Avtomat

type nacin = PrivzetNacin | VnasanjeNiza | PremikanjeVozlisca of stanje

module type TRAK = sig
  type t

  val prazen : t
  val trenutni_znak : t -> char
  val je_na_koncu : t -> bool
  val premakni_naprej : t -> t
  val iz_niza : string -> t
  val v_niz : t -> string
  val prebrani : t -> string
  val neprebrani : t -> string
end

module Trak : TRAK = struct
  type t = { niz : string; indeks_trenutnega_znaka : int }

  let trenutni_znak trak = String.get trak.niz trak.indeks_trenutnega_znaka
  let je_na_koncu trak = String.length trak.niz = trak.indeks_trenutnega_znaka

  let premakni_naprej trak =
    { trak with indeks_trenutnega_znaka = succ trak.indeks_trenutnega_znaka }

  let iz_niza niz = { niz; indeks_trenutnega_znaka = 0 }
  let prazen = iz_niza ""
  let v_niz trak = trak.niz

  let prebrani trak = String.sub trak.niz 0 trak.indeks_trenutnega_znaka

  and neprebrani trak =
    String.sub trak.niz trak.indeks_trenutnega_znaka
      (String.length trak.niz - trak.indeks_trenutnega_znaka)
end

type model = {
  avtomat : avtomat;
  polozaji : (stanje * Vektor.t) list;
  nacin : nacin;
  sirina : float;
  visina : float;
  trak : Trak.t;
  stanje_avtomata : stanje;
}

let init sirina visina avtomat =
  let polozaji =
    Vektor.koreni_enote (List.length avtomat.stanja) sirina visina
    |> List.combine avtomat.stanja
  in
  {
    avtomat;
    polozaji;
    nacin = PrivzetNacin;
    sirina;
    visina;
    trak = Trak.prazen;
    stanje_avtomata = avtomat.zacetno_stanje;
  }

type msg =
  | PreberiNaslednjiZnak
  | ZacniPremikVozlisca of stanje
  | PremakniVozlisce of Vektor.t
  | KoncajPremikVozlisca
  | ZacniVnosNiza
  | VnesiNiz of string

let polozaj_stanja model q = List.assoc q model.polozaji

let update model = function
  | PreberiNaslednjiZnak when Trak.je_na_koncu model.trak -> (
      print_endline "P";
      let znak = Trak.trenutni_znak model.trak in
      match preberi_znak model.avtomat model.stanje_avtomata znak with
      | None -> model
      | Some q' ->
          {
            model with
            stanje_avtomata = q';
            trak = Trak.premakni_naprej model.trak;
          })
  | PreberiNaslednjiZnak -> model
  | ZacniPremikVozlisca q -> { model with nacin = PremikanjeVozlisca q }
  | PremakniVozlisce position -> (
      match model.nacin with
      | PremikanjeVozlisca q ->
          let polozaji =
            List.map
              (fun (q', position') ->
                (q', if q = q' then position else position'))
              model.polozaji
          in
          { model with polozaji }
      | _ -> model)
  | KoncajPremikVozlisca -> { model with nacin = PrivzetNacin }
  | ZacniVnosNiza -> { model with nacin = VnasanjeNiza }
  | VnesiNiz vneseni_niz ->
      {
        model with
        stanje_avtomata = model.avtomat.zacetno_stanje;
        trak = Trak.iz_niza vneseni_niz;
        nacin = PrivzetNacin;
      }
