let rec zlozi_desno f xs z = match xs with
  | [] -> z
  | x :: xs -> f x (zlozi_desno f xs z)

(* vsota, produkt, dolzina, preslikaj *)

let vsota xs = zlozi_desno (fun x acc -> x + acc) xs 0
let produkt xs = zlozi_desno (fun x acc -> x * acc) xs 1
let dolzina xs = zlozi_desno (fun x acc -> acc + 1) xs 0
let preslikaj f xs = zlozi_desno (fun x acc -> f x :: acc) xs []

(* zlozi_desno : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b *)

let rec zlozi_levo f z xs = match xs with
  | [] -> z
  | x :: xs' -> zlozi_levo f (f z x) xs'