let rec zlozi_desno f xs z = match xs with
  | [] -> z
  | x :: xs -> f x (zlozi_desno f xs z)

let rec preslikaj g = function
  | [] -> []
  | x :: xs -> g x :: preslikaj g xs

let vsota xs = zlozi_desno (+) xs 0
let preslikaj' g xs = zlozi_desno (fun x gxs -> g x :: gxs) xs []

let map' g xs = List.fold_right (fun x gxs -> g x :: gxs) xs []

;;

zlozi_desno (+) [1; 2; 3] 0