open Avtomat

type nacin = Normal | PremikanjeVozlisca of stanje

type model = {
  avtomat : avtomat;
  polozaji : (stanje * Vektor.t) list;
  nacin : nacin;
  sirina : float;
  visina : float;
  prebrani_znaki : char list;
  neprebrani_znaki : char list;
  stanje_avtomata : stanje;
}

let init sirina visina avtomat string =
  let neprebrani_znaki = string |> String.to_seq |> List.of_seq in
  let polozaji =
    Vektor.koreni_enote (List.length avtomat.stanja) sirina visina
    |> List.combine avtomat.stanja
  in
  {
    avtomat;
    polozaji;
    nacin = Normal;
    sirina;
    visina;
    prebrani_znaki = [];
    neprebrani_znaki;
    stanje_avtomata = avtomat.zacetno_stanje;
  }

type msg =
  | PreberiNaslednjiZnak
  | ZacniPremikVozlisca of stanje
  | PremakniVozlisce of Vektor.t
  | KoncajPremikVozlisca

let polozaj_stanja model q = List.assoc q model.polozaji

let update model = function
  | PreberiNaslednjiZnak -> (
      match model.neprebrani_znaki with
      | znak :: neprebrani_znaki -> (
          match preberi_znak model.avtomat model.stanje_avtomata znak with
          | None -> model
          | Some q' ->
              {
                model with
                stanje_avtomata = q';
                prebrani_znaki = model.prebrani_znaki @ [ znak ];
                neprebrani_znaki;
              })
      | [] -> model)
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
      | Normal -> model)
  | KoncajPremikVozlisca -> { model with nacin = Normal }
