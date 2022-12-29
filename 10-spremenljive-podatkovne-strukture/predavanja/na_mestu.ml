let iz_na_mestu_v_ne_na_mestu f_na_mestu tabela =
  let kopija = Array.copy tabela in
  let () = f_na_mestu kopija in
  kopija

let iz_ne_na_mestu_v_na_mestu f_ne_na_mestu tabela =
  let spremenjena = f_ne_na_mestu tabela in
  for i = 0 to Array.length tabela - 1 do
    tabela.(i) <- spremenjena.(i)
  done
