type meja = 
  | Odprta of float
  | Zaprta of float
  | Neomejena

type interval = {
  spodnja : meja;
  zgornja : meja;
}

let nad_spodnjo x =
  function
  | Neomejena -> true
  | Odprta meja -> x > meja
  | Zaprta meja -> x >= meja

let pod_zgornjo x =
  function
  | Neomejena -> true
  | Odprta meja -> x < meja
  | Zaprta meja -> x <= meja

let vsebuje x i =
  nad_spodnjo x i.spodnja && pod_zgornjo x i.zgornja

let vecja_spodnja meja1 meja2 =
  match meja2 with
  | Neomejena -> meja1
  | (Odprta x | Zaprta x) when nad_spodnjo x meja1 -> meja2
  | _ -> meja1

let manjsa_zgornja meja1 meja2 =
  if pod_zgornjo meja2.vrednost meja1 then meja2 else meja1

let presek (i1 : interval)
    (i2 : interval) : interval =
  let nova_spodnja = vecja_spodnja i1.spodnja i2.spodnja
  and nova_zgornja = manjsa_zgornja i1.zgornja i2.zgornja in
  { spodnja = nova_spodnja; zgornja = nova_zgornja }

let enotski_interval = { spodnja = {vrednost = 0.0; zaprt = true}; zgornja = {vrednost = 1.0; zaprt = true} }

let zamaknjen_enotski_interval = { spodnja = {vrednost = 1.0; zaprt = true}; zgornja = {vrednost = 2.0; zaprt = true} }
