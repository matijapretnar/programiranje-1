
(* ========== Vaja 1: Uvod v OCaml  ========== *)

(*----------------------------------------------------------------------------*]
 Definirajte funkcijo [predzadnji_element], ki vrne predzadnji element danega
 seznama. V primeru prekratkega seznama naj vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # predzadnji_element [1; 2; 3; 4];;
 - : int = 3
[*----------------------------------------------------------------------------*)

let rec predzadnji_element = function
  | x :: _ :: [] -> x
  | _ :: xs -> predzadnji_element xs
  | [] -> failwith "List is too short."

(*----------------------------------------------------------------------------*]
 Definirajte funkcijo [poisci k lst], ki poišče [k]-ti element v seznamu [lst].
 Številčenje elementov seznama (kot ponavadi) pričnemo z 0. Če je k negativen,
 naj funkcija vrne ničti element.
 V primeru prekratkega seznama naj funkcija vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # poisci 2 [0; 0; 1; 0; 0; 0];;
 - : int = 1
[*----------------------------------------------------------------------------*)

let rec poisci k = function
  | [] -> failwith "List is too short."
  | x :: xs -> if k <= 0 then x else poisci (k-1) xs

(*----------------------------------------------------------------------------*]
 Definirajte funkcijo [podvoji], ki podvoji pojavitve elementov v seznamu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # podvoji [1; 2; 3];;
 - : int list = [1; 1; 2; 2; 3; 3]
[*----------------------------------------------------------------------------*)

let rec podvoji = function
  | x :: xs -> x :: x :: podvoji xs
  | [] -> []

(*----------------------------------------------------------------------------*]
 Definirajte funkcijo [razdeli k lst], ki seznam razdeli na dva seznama. Prvi
 naj vsebuje prvih [k] elementov, v drugem pa vsi ostali. Funkcija vrne par teh
 dveh seznamov.
 V primeru, ko je [k] izven mej seznama, naj bo primeren od seznamov prazen.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # razdeli 2 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2], [3; 4; 5])
 # razdeli 7 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2; 3; 4; 5], [])
[*----------------------------------------------------------------------------*)

let rec razdeli k lst =
  match (k, lst) with
  | (_, []) -> ([], [])
  | (k, lst) when k <= 0 -> ([], lst)
  | (k, x :: xs) ->
      let (lst1, lst2) = razdeli (k - 1) xs in
	    (x :: lst1, lst2)

(*----------------------------------------------------------------------------*]
 Definirajte funkcijo [zbrisi k lst], ki iz seznama izbriše [k]-ti element.
 V primeru prekratkega seznama naj funkcija vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # zbrisi 3 [0; 0; 0; 1; 0; 0];;
 - : int list = [0; 0; 0; 0; 0]
[*----------------------------------------------------------------------------*)

let rec zbrisi k = function
  | [] -> failwith "List is too short."
  | x :: xs -> if k = 0 then xs else x :: zbrisi (k-1) xs

(*----------------------------------------------------------------------------*]
 Definirajte funkcijo [rezina i k lst], ki sestavi nov seznam, ki vsebuje
 elemente seznama [lst] od vključno [i]-tega do izključno [k]-tega.
 Predpostavimo, da sta [i] in [k] primerna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # rezina 3 6 [0; 0; 0; 1; 2; 3; 0; 0];;
 - : int list = [1; 2; 3]
[*----------------------------------------------------------------------------*)

let rezina i k lst =
  let (_, rez1) = razdeli i lst in
  let (rez2, _) = razdeli (k - i) rez1 in
  rez2

(*----------------------------------------------------------------------------*]
 Definirajte funkcijo [vstavi x k lst], ki na [k]-to mesto seznama [lst] vrine
 element [x]. Če je [k] izven mej seznama, naj ga funkcija doda na začetek
 oziroma na konec.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # vstavi 1 3 [0; 0; 0; 0; 0];;
 - : int list = [0; 0; 0; 1; 0; 0]
 # vstavi 1 (-2) [0; 0; 0; 0; 0];;
 - : int list = [1; 0; 0; 0; 0; 0]
[*----------------------------------------------------------------------------*)

let rec vstavi x k = function
  | [] -> [x]
  | y :: ys -> if k <= 0 then x :: y :: ys else y :: vstavi x k ys

(*----------------------------------------------------------------------------*]
 Definirajte funkcijo [zavrti n lst], ki seznam zavrti za [n] mest v levo.
 Predpostavimo, da je [n] v mejah seznama.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # zavrti 2 [1; 2; 3; 4; 5];;
 - : int list = [3; 4; 5; 1; 2]
[*----------------------------------------------------------------------------*)

let rec zavrti n lst =
  let (zacetek, konec) = razdeli n lst in
  konec @ zacetek

(*----------------------------------------------------------------------------*]
 Definirajte funkcijo [pobrisi x lst], ki iz seznama [lst] izbriše vse pojavitve
 elementa [x].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # pobrisi 1 [1; 1; 2; 3; 1; 2; 3; 1; 1];;
 - : int list = [2; 3; 2; 3]
[*----------------------------------------------------------------------------*)

let rec pobrisi x = function
  | y :: ys -> if y = x then pobrisi x ys else y :: pobrisi x ys
  | [] -> []

(*----------------------------------------------------------------------------*]
 Definirajte funkcijo [je_palindrom], ki za dani seznam ugotovi ali predstavlja
 palindrom.
 Namig: Pomagaj si s pomožno funkcijo, ki obrne vrstni red elementov seznama.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # je_palindrom [1; 2; 3; 2; 1];;
 - : bool = true
 # je_palindrom [0; 0; 1; 0];;
 - : bool = false
[*----------------------------------------------------------------------------*)

let je_palindrom lst =
  let rec obrni = function
    | x :: xs -> obrni xs @ [x]
	  | [] -> []
  in
  lst = obrni lst

(*----------------------------------------------------------------------------*]
 Definirajte funkcijo [max_po_komponentah], ki sprejme dva seznama in vrne nov
 seznam, katerega elementi so večji od istoležnih elementov na danih seznamih.
 Skupni seznam ima dolžino krajšega od danih seznamov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_po_komponentah [5; 4; 3; 2; 1] [0; 1; 2; 3; 4; 5; 6];;
 - : int list = [5; 4; 3; 3; 4]
[*----------------------------------------------------------------------------*)

let rec max_po_komponentah lst1 lst2 =
  match (lst1, lst2) with
  | (x :: xs, y :: ys) -> max x y :: max_po_komponentah xs ys
  | _ -> []

(*----------------------------------------------------------------------------*]
 Definirajte funkcijo [drugi_najvecji], ki vrne drugo največjo vrednost v
 seznamu. Pri tem se ponovitve elementa štejejo kot ena vrednost.
 Predpostavimo, da ima seznam vsaj dva različna elementa.
 Namig: Pomagaj si s pomožno funkcijo, ki poišče največjo vrednost v seznamu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # drugi_najvecji [1; 10; 11; 11; 5; 4; 10];;
 - : int = 10
[*----------------------------------------------------------------------------*)

let drugi_najvecji lst =
  let rec najvecji = function
    | [] -> failwith "List is too short."
	  | x :: [] -> x
	  | x :: xs -> max x (najvecji xs)
  in
  najvecji (pobrisi (najvecji lst) lst)
