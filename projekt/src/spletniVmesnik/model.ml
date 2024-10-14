open Definicije

type nacin = PrivzetNacin | VnasanjeNiza | PremikanjeVozlisca of Stanje.t

type model = {
  avtomat : ZagnaniAvtomat.t;
  polozaji : (Stanje.t * Vektor.t) list;
  nacin : nacin;
  sirina : float;
  visina : float;
}

let init sirina visina avtomat =
  let polozaji =
    Vektor.koreni_enote
      (List.length (Avtomat.seznam_stanj avtomat))
      sirina visina
    |> List.combine (Avtomat.seznam_stanj avtomat)
  in
  {
    avtomat = ZagnaniAvtomat.pozeni avtomat Trak.prazen;
    polozaji;
    nacin = PrivzetNacin;
    sirina;
    visina;
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
  | PreberiNaslednjiZnak -> (
      match ZagnaniAvtomat.korak_naprej model.avtomat with
      | None -> model
      | Some avtomat' -> { model with avtomat = avtomat' })
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
        avtomat =
          ZagnaniAvtomat.pozeni
            (ZagnaniAvtomat.avtomat model.avtomat)
            (Trak.iz_niza vneseni_niz);
        nacin = PrivzetNacin;
      }
