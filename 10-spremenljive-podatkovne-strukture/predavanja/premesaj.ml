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

let prikazi_urejenost tabela =
  Array.iteri (fun i n ->
    String.make n (if n = i + 1 then '=' else '#')
    |> print_endline
  ) tabela

let prikazi_odstopanja tabela =
  Array.iteri (fun i n -> print_string (if n = i + 1 then "=" else "#")) tabela;
  print_newline ()

let premesaj_na_mestu tabela =  
  let n = Array.length tabela in
  for i = 1 to (n - 1) do
    let j = Random.int (n - i + 1) in
    zamenjaj tabela i j
  done

let vrni_premesano tabela =
  let kopija = Array.copy tabela in
  premesaj_na_mestu kopija;
  kopija

;;

(* Random.self_init (); *)
let t = urejena_tabela 100 in
premesaj_na_mestu t;
prikazi_odstopanja t