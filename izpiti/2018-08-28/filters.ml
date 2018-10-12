type 'a filter = Filter of ('a -> bool) * 'a list * 'a filter | Ostalo of 'a list


let test = Filter((fun x -> x < 0), [], Filter((fun x -> x < 10), [], Ostalo []))


let rec vstavi x = function
  | Ostalo sez -> Ostalo (x :: sez)
  | Filter (f, sez, filtri) when f x -> Filter (f, x :: sez, filtri)
  | Filter (f, sez, filtri) -> Filter (f, sez, vstavi x filtri)


let rec poisci x = function
  | Ostalo sez -> List.mem x sez
  | Filter (f, sez, filtri) when f x -> List.mem x sez
  | Filter (_, _, filtri) -> poisci x filtri


let rec izprazni_filtre = function
  | Ostalo sez -> (Ostalo [], sez)
  | Filter (f, sez, filtri) ->
      let prazni, vsebina = izprazni_filtre filtri in
      (Filter (f, [], prazni), sez @ vsebina)


let rec dodaj_filter f filtri =
  let prazni, vsebina = izprazni_filtre filtri in
  let nova_veriga = Filter (f, [], prazni) in
  List.fold_right vstavi vsebina nova_veriga
