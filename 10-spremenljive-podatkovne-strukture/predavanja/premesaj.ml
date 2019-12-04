let zamenjaj tabela i j =
  let t = tabela.(i) in
  tabela.(i) <- tabela.(j);
  tabela.(j) <- t

let zamenjaj_vrednosti_referenc r1 r2 =
  let t = !r1 in
  r1 := !r2;
  r2 := t

let vrni_obrnjeno tabela =
  let n = Array.length tabela in
  Array.init n (fun i -> tabela.(n - i - 1))

let obrni_na_mestu tabela =
  let n = Array.length tabela in
  for i = 0 to n / 2 - 1 do
    zamenjaj tabela i (n - i - 1)
  done

let urejena_tabela n =
  Array.init n (fun i -> i + 1)

let prikazi_verjetnosti_permutacij stevilo_poskusov velikost_permutacije =
  let zapis_permutacije permutacija = String.concat "" (Array.to_list permutacija)
  and tabela = Array.init velikost_permutacije (fun i -> string_of_int (i + 1))
  and ponovitve = Hashtbl.create stevilo_poskusov in
  for _ = 1 to stevilo_poskusov do
    let mesanje = zapis_permutacije (vrni_premesano tabela) in
    if Hashtbl.mem ponovitve mesanje then
      Hashtbl.replace ponovitve mesanje (Hashtbl.find ponovitve mesanje + 1)
    else
      Hashtbl.add ponovitve mesanje 1
  done;
  Hashtbl.fold (fun perm st sez -> (perm, 2000 * st / stevilo_poskusov) :: sez) ponovitve []
  |> List.sort compare
  |> List.map (fun (perm, st) -> perm ^ " " ^ String.make st '=')
  |> List.iter print_endline


let premesaj_na_mestu tabela =  
  let n = Array.length tabela in
  for i = 0 to (n - 2) do
    let j = i + Random.int (n - i) in
    zamenjaj tabela i j
  done

let vrni_premesano tabela =
  let kopija = Array.copy tabela in
  premesaj_na_mestu kopija;
  kopija

;;

(* Random.self_init (); *)
prikazi_verjetnosti_permutacij 2000 5