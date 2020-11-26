let rec fold_left f zac_acc sez =
  match sez with
  | [] -> zac_acc
  | glava :: rep ->
      let nov_acc = f zac_acc glava in
      fold_left f nov_acc rep

let rec fold_right f sez zac_acc =
  match sez with
  | [] -> zac_acc
  | glava :: rep ->
      let nov_acc = fold_right f rep zac_acc in
      f glava nov_acc

(* vsota [1; 2; 3]
= 1 + vsota [2; 3]
= 1 + (2 + vsota [3])
= 1 + (2 + (3 + vsota []))
= 1 + (2 + (3 + 0)) *)

let vsota sez = List.fold_right (fun el acc -> el + acc) sez 0

(* dolzina [1; 2; 3]
= 1 + dolzina [2; 3]
= 1 + (1 + dolzina [3])
= 1 + (1 + (1 + dolzina []))
= 1 + (1 + (1 + 0)) *)

let dolzina sez = List.fold_right (fun el acc -> 1 + acc) sez 0

(* produkt [1; 2; 3]
= 1 * produkt [2; 3]
= 1 * (2 * produkt [3])
= 1 * (2 * (3 * produkt []))
= 1 * (2 * (3 * 1)) *)

let produkt sez = List.fold_right (fun el acc -> el * acc) sez 1

let rec obrni = function
| [] -> []
| x :: xs -> obrni xs @ [x]

(* obrni [1; 2; 3]
= obrni [2; 3] @ [1]
= (obrni [3] @ [2]) @ [1]
= ((obrni [] @ [3]) @ [2]) @ [1]
= (([] @ [3]) @ [2]) @ [1] *)

let obrni sez = List.fold_right (fun el acc -> acc @ [el]) sez []

(* obrni [1; 2; 3]
= obrni_aux [] [1; 2; 3]
= obrni_aux (1 :: []) [2; 3]
= obrni_aux (2 :: 1 :: []) [3]
= obrni_aux (3 :: 2 :: 1 :: []) []
= 3 :: 2 :: 1 :: [] *)
let obrni' sez = List.fold_left (fun acc el -> el :: acc) sez []
