(* 
  list_tree je drevo, ki je ali list z vrednostjo, 
  ali pa vozlišče, ki vsebuje seznam poddreves tipa list_tree 
*)

type 'a list_tree = Leaf of 'a | Node of 'a list_tree list

(* 
  Napišite funkcijo map, ki sprejme funkcijo in drevo ter 
  podano drevo preslika s podano funkcijo.
*)

let rec map f = function
  | Leaf x -> Leaf (f x)
  | Node xs -> Node (List.map (map f) xs)

(* 
  Napišite funkcijo count, ki sprejme drevo in vrne število listov v drevesu.
  Za vse točke naj bo funkcija repno rekurzivna.
*)

let count_tlrec tree = 
  let rec count' acc = function
    | Leaf _ -> acc + 1
    | Node xs -> List.fold_left count' acc xs
  in
  count' 0 tree

let rec count = function
  | Leaf _ -> 1
  | Node xs -> xs |> List.map count |> List.fold_left (+) 0

(*  
  Napišite funkcijo `apply`, tipa:
    ('a -> 'b) list_tree -> 'a list_tree -> 'b list_tree
    
  Funkcija sprejme dve drevesi in vrne novo drevo, kjer je vsak element
  rezultat aplikacije funkcije na elementu na isti poziciji v drugem drevesu.
  Predpostavite lahko, da sta drevesi popolnoma enake oblike.

*)

let rec apply t1 t2 =
  match (t1, t2) with
  | Leaf f, Leaf x -> Leaf (f x)
  | Node fs, Node xs -> Node (List.map2 apply fs xs)
  | _ -> failwith "apply: type error"

(*
Napišite funkcijo `combine`, tipa:
  ('a -> 'b) list_tree -> ('c -> 'a) list_tree -> ('c -> 'b) list_tree
Funkcija sprejme dve drevesi, katerih elementi so funkcije, in vrne novo drevo,
kjer je vsak element kompozitum enakoležnih funkcij.
Predpostavite lahko, da sta drevesi popolnoma enake oblike. 

Za vsa (smiselna) drevesa mora veljati:

map (combine t1 t2) t3 == (map t1 (map t2 t3))

*)

let rec combine t1 t2 = 
  match (t1, t2) with
  | Leaf f, Leaf g -> Leaf (fun x -> f (g x))
  | Node fs, Node xs -> Node (List.map2 combine fs xs)
  | _ -> failwith "combine: type error"

let rec apply_smart t1 t2 d1 d2 =
  match (t1, t2) with
  | Leaf f, Leaf x -> Leaf (f x)
  | Node fs, Node xs ->
      let rec process fs xs =
        match (fs, xs) with
        | [], [] -> []
        | [], x :: xs -> apply (map (fun _ -> d1) x) x :: process fs xs
        | f :: fs, [] -> apply f (map (fun _ -> d2) f) :: process fs xs
        | f :: fs, x :: xs -> apply_smart f x d1 d2 :: process fs xs
      in
      Node (process fs xs)
  | _ -> failwith "apply_smart: type error"


let t1 = Node [Node [Leaf (fun x -> x)]; Leaf (fun x -> x * 2)]

let t2 = Node [Leaf 1; Leaf 2]

let t3 = Node [Node []; Leaf 2; Leaf 4]

let t4 = apply_smart t1 t3 (fun x -> x* 100) 0

let k = apply_smart ( Node [Node [Leaf (fun x -> x)]; Leaf (fun x -> x * 2)]) (Node [Node []; Leaf 2; Leaf 4]) (fun x -> x * 110) 19