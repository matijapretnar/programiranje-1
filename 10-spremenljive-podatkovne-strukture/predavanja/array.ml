let tab = [| 1; 2; 3; 4 |]

let obrnjen tab =
  let n = Array.length tab in
  Array.init n (fun i -> tab.(n - i - 1))

let zamenjaj tab i j =
  let x = tab.(i) in
  tab.(i) <- tab.(j);
  tab.(j) <- x

let obrni tab =
  let n = Array.length tab in
  for i = 0 to (n / 2) - 1 do
    zamenjaj tab i (n - i - 1)
  done

let obrni_while tab =
  let n = Array.length tab in
  let i = ref 0 in
  while !i <= (n / 2) - 1 do
    zamenjaj tab !i (n - !i - 1);
    incr i
  done

let iz_na_mestu_v_ne_na_mestu f tab =
  let tab' = Array.copy tab in
  let () = f tab' in
  tab'

let premesaj_na_mestu tab =
  let n = Array.length tab in
  for _ = 1 to 10000 do
    let i = Random.int n and j = Random.int n in
    zamenjaj tab i j
  done

let premesan tab = iz_na_mestu_v_ne_na_mestu premesaj_na_mestu tab
