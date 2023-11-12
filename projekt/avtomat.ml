type stanje = { oznaka : string }

type avtomat = {
  stanja : stanje list;
  zacetno_stanje : stanje;
  sprejemna_stanja : stanje list;
  prehodi : (stanje * char * stanje) list;
}

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

let ravno_prav_nicel =
  let q0 = { oznaka = "q0" }
  and q1 = { oznaka = "q1" }
  and q2 = { oznaka = "q2" } in
  let prehodi =
    [
      (q0, '0', q1);
      (q1, '0', q2);
      (q2, '0', q0);
      (q0, '1', q0);
      (q1, '1', q1);
      (q2, '1', q2);
    ]
  in
  {
    stanja = [ q0; q1; q2 ];
    zacetno_stanje = q0;
    sprejemna_stanja = [ q1 ];
    prehodi;
  }
