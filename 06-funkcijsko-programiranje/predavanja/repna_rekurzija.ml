let rec vsota xs =
  match xs with
  | [] -> 0
  | x :: xs' -> x + vsota xs'

let repno_rekurzivna_vsota xs =
  let rec pomozna acc xs =
  match xs with
  | [] -> acc
  | x :: xs' -> pomozna (acc + x) xs'
  in
  pomozna 0 xs

let rec repno_rekurzivna_vsota_z_neobveznim_argumentom ?(acc=0) xs =
  match xs with
  | [] -> acc
  | x :: xs' -> repno_rekurzivna_vsota_z_neobveznim_argumentom ~acc:(acc + x) xs'

(* ('a -> 'b) -> 'a list -> 'b list *)
let rec preslikaj f xs =
  match xs with
  | [] -> []
  | x :: xs' -> f x :: preslikaj f xs'

let obrni xs =
  let rec pomozna acc xs =
    match xs with
    | [] -> acc
    | x :: xs' -> pomozna (x :: acc) xs'
  in
  pomozna [] xs

let repno_rekurzivni_preslikaj f xs =
  let rec pomozna acc xs =
    match xs with
    | [] -> obrni acc
    | x :: xs' -> pomozna (f x :: acc) xs'
  in
  pomozna [] xs

let rec f x y =
    if y = 0 then x else f (x + 1) (y - 1)

let rec f' x y =
    if y = 0 then x else 1 + f' x (y - 1)
