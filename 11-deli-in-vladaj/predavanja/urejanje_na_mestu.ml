let zamenjaj tabela i j =
  let t = tabela.(i) in
  tabela.(i) <- tabela.(j);
  tabela.(j) <- t

let pivotiraj_na_mestu tabela i0 j0 =
  let pivot = tabela.(i0) in
  let i = ref (i0 + 1)
  and j = ref j0 in
  while !i < !j do
    while !i < !j && tabela.(!i) <= pivot do
      incr i
    done;
    while !i < !j && tabela.(!j) > pivot do
      decr j
    done;
    zamenjaj tabela !i !j
  done;
  let p = if tabela.(!i) <= pivot then !i else !i - 1 in
  zamenjaj tabela i0 p;
  p

let hitro_uredi_na_mestu tabela =
  let rec uredi_med_indeksoma i j =
    if i < j then
      let p = pivotiraj_na_mestu tabela i j in
      uredi_med_indeksoma i (p - 1);
      uredi_med_indeksoma (p + 1) j
  in
  uredi_med_indeksoma 0 (Array.length tabela - 1)
