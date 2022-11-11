let rec zlozi_desno f xs z = match xs with
  | [] -> z
  | x :: xs -> f x (zlozi_desno f xs z)

let vsota xs = zlozi_desno (+) xs 0

zlozi_desno (+) [1; 2; 3] 0