type narocilo = {
  nar_izdelki : (string * int) list;
  hitra_dost : bool;
  nas_prejemnika : string option;
  darilo : bool;
  vp : string option;
}

let cena_narocila narocilo =
  let cene_izdel =
    narocilo.nar_izdelki
    |> List.map snd
    |> List.fold_left (+) 0
  and cen_papirja =
    match narocilo.vp with
    | Some "grd" -> 1
    | Some "bleščeč" -> 10
    | _ -> 0
  in
  let cd =
    if Option.is_some narocilo.nas_prejemnika then
      if cene_izdel < 100 && narocilo.hitra_dost then
        10
      else if cene_izdel < 50 && not narocilo.hitra_dost then
        5
      else
        0
    else
      0
    in
    cene_izdel + cen_papirja + cd