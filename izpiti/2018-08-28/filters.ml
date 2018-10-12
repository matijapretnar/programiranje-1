type 'a veriga = Filter of ('a -> bool) * 'a list * 'a veriga | Ostalo of 'a list


let test = Filter((fun x -> x < 0), [], Filter((fun x -> x < 10), [], Ostalo []))


let rec vstavi x = function
  | Ostalo sez -> Ostalo (x :: sez)
  | Filter (f, sez, veriga) when f x -> Filter (f, x :: sez, veriga)
  | Filter (f, sez, veriga) -> Filter (f, sez, vstavi x veriga)


let rec poisci x = function
  | Ostalo sez -> List.mem x sez
  | Filter (f, sez, veriga) when f x -> List.mem x sez
  | Filter (_, _, veriga) -> poisci x veriga


let rec izprazni_filtre = function
  | Ostalo sez -> (Ostalo [], sez)
  | Filter (f, sez, veriga) ->
      let prazni, vsebina = izprazni_filtre veriga in
      (Filter (f, [], prazni), sez @ vsebina)


let rec dodaj_filter f veriga =
  let prazni, vsebina = izprazni_filtre veriga in
  let nova_veriga = Filter (f, [], prazni) in
  List.fold_right vstavi vsebina nova_veriga
