let rec zlozi_desno f xs z =
  match xs with
  | [] -> z
  | x :: xs -> f x (zlozi_desno f xs z)

let vsota xs = zlozi_desno (+) xs 0

let produkt xs = zlozi_desno ( * ) xs 1

let dolzina xs = zlozi_desno (fun _ acc -> succ acc) xs 0

let preslikaj f xs = zlozi_desno (fun x acc -> f x :: acc) xs []

let rec zlozi_levo f z = function
  | [] -> z
  | x :: xs -> zlozi_levo f (f z x) xs

let obrni xs = zlozi_levo (fun acc x -> x :: acc) [] xs

let obrni' xs = zlozi_desno (fun x acc -> acc @ [x]) xs []

let rec enke n = if n = 0 then [] else 1 :: enke (n - 1)

let (@@) f x = f x
