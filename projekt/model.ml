open Avtomat

type nacin = PrivzetNacin | VnasanjeNiza | PremikanjeVozlisca of stanje

type model = {
  avtomat : avtomat;
  polozaji : (stanje * Vektor.t) list;
  nacin : nacin;
  sirina : float;
  visina : float;
  vneseni_niz : string;
  indeks_naslednjega_znaka : int;
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
    vneseni_niz = "";
    indeks_naslednjega_znaka = 0;
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
  | PreberiNaslednjiZnak
    when model.indeks_naslednjega_znaka <= String.length model.vneseni_niz -> (
      print_endline "P";
      let znak = String.get model.vneseni_niz model.indeks_naslednjega_znaka in
      match preberi_znak model.avtomat model.stanje_avtomata znak with
      | None -> model
      | Some q' ->
          {
            model with
            stanje_avtomata = q';
            indeks_naslednjega_znaka = succ model.indeks_naslednjega_znaka;
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
        vneseni_niz;
        indeks_naslednjega_znaka = 0;
        nacin = PrivzetNacin;
      }
