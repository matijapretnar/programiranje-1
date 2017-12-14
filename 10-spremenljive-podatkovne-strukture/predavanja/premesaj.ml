let zamenjaj tabela i j =
  let t = tabela.(i) in
  tabela.(i) <- tabela.(j);
  tabela.(j) <- t

let urejena_tabela n =
  Array.init n (fun i -> i + 1)

let narisi tabela =
  Array.iteri (fun i n ->
    String.make n (if n = i + 1 then '#' else '-')
    |> print_endline
  ) tabela

let premesaj_na_mestu tabela =
  let n = Array.length tabela in
  for i = n downto 2 do
    let j = Random.int i in
    zamenjaj tabela (i - 1) j
  done

let vrni_premesano tabela =
  let kopija = Array.copy tabela in
  premesaj_na_mestu kopija;
  kopija

;;

Random.self_init ();
let t = urejena_tabela 10 in
premesaj t;
narisi t