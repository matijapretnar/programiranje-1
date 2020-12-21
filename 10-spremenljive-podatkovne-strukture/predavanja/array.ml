let zamenjaj tab i j =
  let tmp = tab.(i) in
  tab.(i) <- tab.(j);
  tab.(j) <- tmp

let vrni_obrnjeno tab =
  let n = Array.length tab in
  Array.init n (fun i -> tab.(n - i - 1))

let obrni_na_mestu tab =
  let n = Array.length tab in
  for i = 0 to n / 2 do
    zamenjaj tab i (n - i - 1)
  done

let primer = [| 1; 2; 3; 4; 5 |]

let fisher_yates tab =
  let n = Array.length tab in
  for i = n - 1 downto 1 do
    let j = Random.int (i + 1) in
    zamenjaj tab i j
  done

let premesaj tab =
  let tab' = Array.copy tab in
  fisher_yates tab';
  tab'

let prikazi_verjetnosti verjetnosti =
  let urejene_verjetnosti =
    List.sort (fun (_, p1) (_, p2) -> -compare p1 p2) verjetnosti
  in
  match urejene_verjetnosti with
  | [] -> ()
  | (_, max_p) :: _ ->
      let max_sirina = 50 in
      urejene_verjetnosti
      |> List.iter (fun (x, p) ->
          let sirina =
            int_of_float (float_of_int max_sirina *. p /. max_p)
          in
          let stolpec =
            String.make sirina '='
            ^ String.make (max_sirina - sirina) ' '
          in
          Format.printf "%s %s (%f%%)\n" x stolpec (100. *. p)
      )

let verjetnost_rezultatov poskus stevilo_poskusov =
  let ponovitve = Hashtbl.create 256 in
  for _ = 1 to stevilo_poskusov do
    let rezultat = poskus () in
    if Hashtbl.mem ponovitve rezultat then
      Hashtbl.replace ponovitve rezultat (Hashtbl.find ponovitve rezultat + 1)
    else
      Hashtbl.add ponovitve rezultat 1
  done;
  Hashtbl.fold (fun rezultat stevilo seznam ->
    (rezultat, float_of_int stevilo /. float_of_int stevilo_poskusov) :: seznam
  ) ponovitve []

let verjetnost_permutacij premesaj stevilo_poskusov velikost_permutacije =
  let tabela = Array.init velikost_permutacije (fun i -> string_of_int (i + 1)) in
  let poskus () =
    tabela
    |> premesaj
    |> Array.to_list
    |> String.concat ""
  in
  verjetnost_rezultatov poskus stevilo_poskusov
  |> prikazi_verjetnosti
