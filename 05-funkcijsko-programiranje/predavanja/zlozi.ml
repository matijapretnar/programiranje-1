(* zlozi_desno : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b *)
let rec zlozi_desno f xs z = match xs with
  | [] -> z
  | x :: xs -> f x (zlozi_desno f xs z)

let vsota xs = zlozi_desno ( + ) xs 0
let produkt xs = zlozi_desno ( * ) xs 1
let dolzina xs = zlozi_desno (fun _ acc -> succ acc) xs 0
let preslikaj f xs = zlozi_desno (fun x acc -> f x :: acc) xs []

(* zlozi_levo : ('acc -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc *)
let rec zlozi_levo f z xs = match xs with
  | [] -> z
  | x :: xs -> zlozi_levo f (f z x) xs

let rec zlozi_levo_naravna f z n = match n with
  | 0 -> z
  | n -> zlozi_levo_naravna f (f z) (n - 1)
