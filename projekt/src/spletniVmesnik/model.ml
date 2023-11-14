open Definicije
open Avtomat

type nacin = PrivzetNacin | VnasanjeNiza | PremikanjeVozlisca of Stanje.t

type model = {
  avtomat : t;
  polozaji : (Stanje.t * Vektor.t) list;
  nacin : nacin;
  sirina : float;
  visina : float;
  trak : Trak.t;
  stanje_avtomata : Stanje.t;
}

let init sirina visina avtomat =
  let polozaji =
    Vektor.koreni_enote (List.length (seznam_stanj avtomat)) sirina visina
    |> List.combine (seznam_stanj avtomat)
  in
  {
    avtomat;
    polozaji;
    nacin = PrivzetNacin;
    sirina;
    visina;
    trak = Trak.prazen;
    stanje_avtomata = zacetno_stanje avtomat;
  }

type msg =
  | PreberiNaslednjiZnak
  | ZacniPremikVozlisca of Stanje.t
  | PremakniVozlisce of Vektor.t
  | KoncajPremikVozlisca
  | ZacniVnosNiza
  | VnesiNiz of string

let polozaj_stanja model q = List.assoc q model.polozaji

let update model = function
  | PreberiNaslednjiZnak when Trak.je_na_koncu model.trak -> (
      print_endline "P";
      let znak = Trak.trenutni_znak model.trak in
      match prehodna_funkcija model.avtomat model.stanje_avtomata znak with
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
        stanje_avtomata = zacetno_stanje model.avtomat;
        trak = Trak.iz_niza vneseni_niz;
        nacin = PrivzetNacin;
      }
