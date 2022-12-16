let zamenjaj tabela i j =
  let t = tabela.(i) in
  tabela.(i) <- tabela.(j);
  tabela.(j) <- t

let iz_na_mestu_v_ne_na_mestu f_na_mestu tab =
  let tab' = Array.copy tab in
  let () = f_na_mestu tab' in
  tab'

let fisher_yates tabela =
  let n = Array.length tabela in
  for i = 0 to n - 2 do
    let j = i + Random.int (n - i) in
    zamenjaj tabela i j
  done

let urejena_tabela n = Array.init n (fun i -> i + 1)

let prikazi_verjetnosti verjetnosti =
  let urejene_verjetnosti =
    List.sort (fun (x1, p1) (x2, p2) -> compare x1 x2) verjetnosti
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
               String.make sirina '=' ^ String.make (max_sirina - sirina) ' '
             in
             Format.printf "%s %s (%f%%)\n" x stolpec (100. *. p))

let verjetnost_rezultatov poskus stevilo_poskusov =
  let ponovitve = Hashtbl.create 256 in
  for _ = 1 to stevilo_poskusov do
    let rezultat = poskus () in
    if Hashtbl.mem ponovitve rezultat then
      Hashtbl.replace ponovitve rezultat (Hashtbl.find ponovitve rezultat + 1)
    else Hashtbl.add ponovitve rezultat 1
  done;
  Hashtbl.fold
    (fun rezultat stevilo seznam ->
      (rezultat, float_of_int stevilo /. float_of_int stevilo_poskusov)
      :: seznam)
    ponovitve []

let verjetnost_permutacij premesaj stevilo_poskusov velikost_permutacije =
  let tabela =
    Array.init velikost_permutacije (fun i -> string_of_int (i + 1))
  in
  let poskus () = tabela |> premesaj |> Array.to_list |> String.concat "" in
  verjetnost_rezultatov poskus stevilo_poskusov |> prikazi_verjetnosti

;;
Random.self_init ();
verjetnost_permutacij (iz_na_mestu_v_ne_na_mestu fisher_yates) 2000000 4
