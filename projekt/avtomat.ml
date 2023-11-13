type stanje = { oznaka : string }

type avtomat = {
  stanja : stanje list;
  zacetno_stanje : stanje;
  sprejemna_stanja : stanje list;
  prehodi : (stanje * char * stanje) list;
}

let stanje oznaka = { oznaka }

let prazen_avtomat zacetno_stanje =
  {
    stanja = [ zacetno_stanje ];
    zacetno_stanje;
    sprejemna_stanja = [];
    prehodi = [];
  }

let dodaj_nesprejemno_stanje stanje avtomat =
  { avtomat with stanja = stanje :: avtomat.stanja }

let dodaj_sprejemno_stanje stanje avtomat =
  {
    avtomat with
    stanja = stanje :: avtomat.stanja;
    sprejemna_stanja = stanje :: avtomat.sprejemna_stanja;
  }

let dodaj_prehod stanje1 znak stanje2 avtomat =
  { avtomat with prehodi = (stanje1, znak, stanje2) :: avtomat.prehodi }

let preberi_znak avtomat q znak =
  match
    List.find_opt
      (fun (q1, znak', _q2) -> q1 = q && znak = znak')
      avtomat.prehodi
  with
  | None -> None
  | Some (_, _, q') -> Some q'

let preberi_niz avtomat q niz =
  let aux acc znak =
    match acc with None -> None | Some q -> preberi_znak avtomat q znak
  in
  niz |> String.to_seq |> Seq.fold_left aux (Some q)

let ali_sprejema_niz avtomat niz =
  match preberi_niz avtomat avtomat.zacetno_stanje niz with
  | None -> false
  | Some koncno_stanje -> List.mem koncno_stanje avtomat.sprejemna_stanja

let enke_1mod3 =
  let q0 = stanje "q0" and q1 = stanje "q1" and q2 = stanje "q2" in
  prazen_avtomat q0
  |> dodaj_sprejemno_stanje q1
  |> dodaj_nesprejemno_stanje q2
  |> dodaj_prehod q0 '0' q0
  |> dodaj_prehod q1 '0' q1
  |> dodaj_prehod q2 '0' q2
  |> dodaj_prehod q0 '1' q1
  |> dodaj_prehod q1 '1' q2
  |> dodaj_prehod q2 '1' q0
