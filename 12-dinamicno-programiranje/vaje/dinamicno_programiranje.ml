(* ========== Vaje 6: Dinamično programiranje  ========== *)


(*----------------------------------------------------------------------------*]
 Požrešna miška se nahaja v zgornjem levem kotu šahovnice. Premikati se sme
 samo za eno polje navzdol ali za eno polje na desno in na koncu mora prispeti
 v desni spodnji kot. Na vsakem polju šahovnice je en sirček. Ti sirčki imajo
 različne (ne-negativne) mase. Miška bi se rada kar se da nažrla, zato jo
 zanima, katero pot naj ubere.

 Funkcija [max_cheese cheese_matrix], ki dobi matriko [cheese_matrix] z masami
 sirčkov in vrne največjo skupno maso, ki jo bo miška požrla, če gre po
 optimalni poti.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_cheese test_matrix;;
 - : int = 13
[*----------------------------------------------------------------------------*)

let test_matrix = 
  [| [| 1 ; 2 ; 0 |];
     [| 2 ; 4 ; 5 |];
     [| 7 ; 0 ; 1 |] |]

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

let max_cheese_bottom cheese_matrix = 
  let bottom = List.init (Array.length cheese_matrix.(0)) (fun _ -> 0) in
  let lst = List.rev (List.map Array.to_list (Array.to_list cheese_matrix)) in
  let rec best_path bottom current = 
    match (bottom, current) with 
      | ([b], [c]) -> [b + c]
      | (b::bs, c::cs) -> (let right = best_path bs cs in
        match right with 
          |(r::rest) -> (c + (max b r)) :: right
          | _ -> assert false
      )
      | _ -> assert false
  in
  match List.fold_left best_path bottom lst with
    | result::_ -> result
    | _ -> assert false

(*----------------------------------------------------------------------------*]
 Rešujemo problem sestavljanja alternirajoče obarvanih stolpov. Imamo štiri
 različne tipe gradnikov, dva modra in dva rdeča. Modri gradniki so višin 2 in
 3, rdeči pa višin 1 in 2.

 Funkcija [alternating_towers] za podano višino vrne število različnih stolpov
 dane višine, ki jih lahko zgradimo z našimi gradniki, kjer se barva gradnikov
 v stolpu izmenjuje (rdeč na modrem, moder na rdečem itd.). Začnemo z gradnikom
 poljubne barve.

 Namig: Uporabi medsebojno rekurzivni pomožni funkciji z ukazom [and].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # alternating_towers 10;;
 - : int = 35
[*----------------------------------------------------------------------------*)

let alternating_towers height =
  let rec redtop height =
    if height <= 0 then 
      0
    else if height <= 2 then 
      1
    else
      bluetop (height-1) + bluetop (height-2)
  and bluetop height =
    if height <= 0 then 
      0
    else if height = 2 then 
      1
    else if height = 3 then 
      2
    else
      redtop (height-2) + redtop (height-3)
  in
  redtop height + bluetop height

(* Optimal general implementation seems to be circular buffer *)
let alternating_towers_bottom level =
  let rec alternate (r,b) (r1, b1) (r2, b2) level =
    if level == 0 then (r + b) else 
      alternate (r1 + b, b1) (r2 + b, b2 + r) (0, r) (level - 1)
  in
  if level == 0 then 1 else 
  alternate (1,1) (0,0) (0,0) level

(*----------------------------------------------------------------------------*]
 Na nagradni igri ste zadeli kupon, ki vam omogoča, da v Mercatorju kupite
 poljubne izdelke, katerih skupna masa ne presega [max_w] kilogramov. Napišite
 funkcijo [best_value articles max_w], ki poišče največjo skupno ceno, ki jo
 lahko odnesemo iz trgovine, kjer lahko vsak izdelek vzamemo večkrat, nato pa
 še funkcijo [best_value_uniques articles max_w], kjer lahko vsak izdelek
 vzamemo kvečjemu enkrat.

 Namig: Modul [Array] ponuja funkcije kot so [map], [fold_left], [copy] in
 podobno, kot alternativa uporabi zank.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # best_value articles 1.;;
 - : float = 10.95
 # best_value_unique articles 1.;;
- : float = 7.66
[*----------------------------------------------------------------------------*)

(* Articles are of form (name, price, weight) *)
let articles = [|
	("yoghurt", 0.39, 0.18);
	("milk", 0.89, 1.03);
  ("coffee", 2.19, 0.2);
  ("butter", 1.49, 0.25);
  ("yeast", 0.22, 0.042);
  ("eggs", 2.39, 0.69);
  ("sausage", 3.76, 0.50);
  ("bread", 2.99, 1.0);
  ("Nutella", 4.99, 0.75);
  ("juice", 1.15, 2.0)
|]

let best_value articles max_w =
  (* Choose the item if you can and recursively search further. *)
  let rec get_item acc_w acc_p (_, p, w) =
    if acc_w +. w > max_w then
      (* Item is not suitable, return what we got so far.*)
      acc_p
    else
      (* Find best value after choosing the item. *)
      shopper (acc_w +. w) (acc_p +. p)
  (* Choose every item in the list and return the value of the best choice. *)
  and shopper w p =
    let choices = Array.map (get_item w p) articles in
    Array.fold_left max 0. choices
  in
  shopper 0. 0.

let best_value_unique articles max_w =
  (* Store which items have already been chose in the array [taken]. *)
  (* Choose the item if you can and recursively search further. *)
  let rec get_item taken acc_w acc_p i (_, p, w) =
    if acc_w +. w > max_w || taken.(i) then
      (* Item is not suitable, return what we got so far.*)
      acc_p
    else
      (* Find best value after choosing the item, mark choice in [taken]. *)
      let new_taken = Array.copy taken in
      (new_taken.(i) <- true; shopper new_taken (acc_w +. w) (acc_p +. p))
  (* Choose every item in the list and return the value of the best choice. *)  
  and shopper taken w p =
    let choices = Array.mapi (get_item taken w p) articles in
    Array.fold_left max 0. choices
  in
  let taken = Array.map (fun _ -> false) articles in
  shopper taken 0. 0.

(*----------------------------------------------------------------------------*]
 Cena sprehoda po drevesu je vsota vrednosti v vseh obiskanih vozliščih.
 Poiščite vrednost najdražjega sprehoda od korena do listov drevesa.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_path Empty ;;
 - : 'a option = None
 # max_path test_tree;;
- : int option = Some 21
[*----------------------------------------------------------------------------*)

type 'a tree
 = Empty
 | Node of ('a tree) * 'a * ('a tree)

let leaf x = Node (Empty, x, Empty)

let test_tree = Node( Node(leaf 0, 2, leaf 13), 5, Node(leaf 9, 7, leaf 4))

let max_aux a b = match (a,b) with 
  | (Some a', None) -> a'
  | (None, Some b') -> b'
  | (Some a', Some b') -> max a' b'
  | _ -> failwith "Error"

let rec max_path = function
  | Empty -> None
  | Node (Empty, x, Empty) -> Some x
  | Node (left, x, right) -> Some (x + max_aux (max_path left) (max_path right))

(*----------------------------------------------------------------------------*]
 Cena sprehoda po drevesu je vsota vrednosti v vseh obiskanih vozliščih.
 Poiščite najdražji sprehod od korena do listov drevesa: Funkcija pot vrne v 
 obliki seznama smeri, katere je potrebno izbrati za najdražji sprehod.

 Napišite tudi funkcijo, ki sprehod pretvori v elemente sprehoda
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_path_trace Empty ;;
 - : 'a list = []
 # max_path_trace test_tree;;
- : direction list = [Right, Left]
 # reconstruct test_tree (max_path_trace test_tree);;
- : int list = [5; 7; 9]
[*----------------------------------------------------------------------------*)

type direcion 
  = Left
  | Right

let rec max_path_trace = function
  | Empty -> []
  | Node (Empty, x, Empty) -> []
  | Node (left, _, right) -> 
    match (max_path left, max_path right) with
    | (Some a', None) -> Left :: (max_path_trace left)
    | (None, Some b') -> Right :: (max_path_trace right)
    | (Some a', Some b') -> if a' > b' then (Left :: (max_path_trace left)) else (Right ::(max_path_trace right))
    | _ -> failwith "Error"


let rec reconstruct tree path =
  match (tree, path) with
  | (Node (_, x, _), []) -> [x]
  | (Node (left, x, _), Left::dirs) -> x :: (reconstruct left dirs)
  | (Node (_, x, right), Right::dirs) -> x :: (reconstruct right dirs) 
  | _ -> failwith "Invalid path length"