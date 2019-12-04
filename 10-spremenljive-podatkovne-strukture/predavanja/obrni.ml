let zamenjaj tabela i j =
    let t = tabela.(i) in
    tabela.(i) <- tabela.(j);
    tabela.(j) <- t

let vrni_obrnjeno tabela =
  let n = Array.length tabela in
  Array.init n (fun i -> tabela.(n - i - 1))

let obrni_na_mestu tabela =
  let n = Array.length tabela in
  for i = 0 to n / 2 - 1 do
    zamenjaj tabela i (n - i - 1)
  done

let vrni_obrnjeno' tabela =
  let kopija = Array.copy tabela in
  obrni_na_mestu kopija;
  kopija

let stopaj f x =
  let zacetek = Sys.time () in
  let y = f x in
  let konec = Sys.time () in
  print_endline ("Porabljen čas: " ^ string_of_float (1000. *. (konec -. zacetek)) ^ "ms");
  y

let moja_tabela = Array.make 10000 0

let poskus n =
  for _ = 1 to n do
    ignore (vrni_obrnjeno moja_tabela)
  done

let poskus' n =
  for _ = 1 to n do
    ignore (vrni_obrnjeno' moja_tabela)
  done

;;

stopaj poskus 1000;
stopaj poskus' 1000;