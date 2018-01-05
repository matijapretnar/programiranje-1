
(* ===== Vaja: Dinamično programiranje  ===== *)

(* Požrešna miška se nahaja v zgornjem levem kotu šahovnice. Premikati se sme
   samo za eno polje navzdol ali za eno polje na desno in na koncu prispeti v
   desni spodnji kot. Na vsakem polju šahovnice je en sirček. Ti sirčki imajo
   različne (pozitivne) mase. Miška bi se rada kar se da nažrla, zato jo zanima,
   katero pot naj ubere.

   Napišite funkcijo "max_cheese cheese_matrix", ki dobi matriko z
   masami sirčkov in vrne največjo skupno maso, ki jo bo miška požrla, če gre po
   optimalni poti. *)

let test_matrix = [| [| 1 ; 2 ; 0 |];
                     [| 2 ; 4 ; 5 |];
                     [| 7 ; 0 ; 1 |]  |]

let max_cheese cheese_matrix =
  let dimx = Array.length cheese_matrix in
  let dimy = Array.length cheese_matrix.(0) in
  let rec best_path x y =
    let current_cheese = cheese_matrix.(x).(y) in
    let best_right = if (x+1 = dimx) then 0 else best_path (x+1) y in
    let best_down = if (y+1 = dimy) then 0 else best_path x (y+1) in
    current_cheese + max best_right best_down
  in
  best_path 0 0


(* Rešujemo problem stolpov, ko smo ga spoznali na predavanjih.
   Imamo štiri različne tipe gradnikov, dva modra in dva rdeča.
   Modri gradniki so višin 2 in 3, rdeči pa višin 1 in 2.

   Napiši funkcijo "alternating_towers height", ki za podano višino "height"
   izpiše število različnih stolpov podane višine, kjer se barva gradnikov
   izmenjuje (rdeč na modrem, moder na rdečem itd.).

   Namig: Uporabi dve pomožni funkciji. Za medsebojno rekurzijo uporabi
          ukaz "and".
   ----------
   # alternating_towers 10;;
   - : int = 35
   ---------- *)

let alternating_towers height =
  let rec redtop height =
    if height <= 0 then 0
    else if height <= 2 then 1
    else
      bluetop (height-1) + bluetop (height-2)
  and bluetop height =
    if height <= 0 then 0
    else if height = 2 then 1
    else if height = 3 then 2
    else
      redtop (height-2) + redtop (height-3)
  in
  redtop height + bluetop height
