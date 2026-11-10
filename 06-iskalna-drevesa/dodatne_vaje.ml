(*----------------------------------------------------------------------------*
 # Iskalna drevesa: dodatne naloge
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 V dodatnih nalogah uporabljamo tip dreves in testno drevo iz vaj ter funkcije
 `map_tree`, `list_of_tree` in `insert`. Namesto spodnjih nastavkov
 prekopirajte svoje rešitve iz vaj.
[*----------------------------------------------------------------------------*)

type 'a tree = Empty | Node of 'a tree * 'a * 'a tree

let leaf x = Node (Empty, x, Empty)

let test_tree = Node (Node (leaf 0, 2, Empty), 5, Node (leaf 6, 7, leaf 11))

let map_tree _ _ = ()

let list_of_tree _ = ()

let insert _ _ = ()

(*----------------------------------------------------------------------------*
 ## Urejanje z drevesi
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `tree_sort` uredi seznam tako, da njegove elemente enega za drugim
 vstavi v prazno iskalno drevo, nato pa drevo pretvori nazaj v seznam.

 Kaj se zgodi, če se elementi v seznamu ponavljajo? Kakšna je časovna
 zahtevnost funkcije na že urejenem seznamu? Bi pomagalo, če bi namesto
 `insert` uporabili `avl_insert` iz vaj?

 Opomba: Prosim ne uporabljajte te funkcije v praksi.
[*----------------------------------------------------------------------------*)

let tree_sort _ = ()

let primer_tree_sort = tree_sort ["a"; "c"; "f"; "b"; "e"; "d"]
(* val primer_tree_sort : string list = ["a"; "b"; "c"; "d"; "e"; "f"] *)

(*----------------------------------------------------------------------------*
 ## Navodila za pot

 Pot po drevesu opišemo s seznamom navodil tipa `direction`, ki povedo, ali se
 iz vozlišča premaknemo v levo ali v desno poddrevo.
[*----------------------------------------------------------------------------*)

type direction = Left | Right

(*----------------------------------------------------------------------------*
 Funkcija `follow directions tree` tipa `direction list -> 'a tree -> 'a option`
 sprejme seznam navodil za premikanje po drevesu in vrne podatek v vozlišču, do
 katerega vodi podana pot. Ker navodila morda ne vodijo do nobenega vozlišča v
 drevesu, vrne rezultat kot `option` tip.
[*----------------------------------------------------------------------------*)

let follow _ _ = ()

let primer_follow = follow [Right; Left] test_tree
(* val primer_follow : int option = Some 6 *)

let primer_follow_2 = follow [Right; Left; Right; Right] test_tree
(* val primer_follow_2 : int option = None *)

(*----------------------------------------------------------------------------*
 Funkcija `prune directions tree` poišče vozlišče v drevesu glede na navodila
 in izbriše poddrevo, ki se začne v izbranem vozlišču. Če navodila ne vodijo do
 nobenega vozlišča, vrne `None`.

 Opozorilo: Pri uporabi `Some Node (l, x, r)` se OCaml pritoži, saj to razume
 kot `(Some Node) (l, x, r)`, zato pravilno postavite potrebne oklepaje.
[*----------------------------------------------------------------------------*)

let prune _ _ = ()

let primer_prune = prune [Right] test_tree
(* val primer_prune : int tree option =
     Some (Node (Node (Node (Empty, 0, Empty), 2, Empty), 5, Empty)) *)

let primer_prune_2 = prune [Left; Right] test_tree
(* val primer_prune_2 : int tree option = None *)

(*----------------------------------------------------------------------------*
 ## Leno brisanje

 Brisanje iz iskalnega drevesa je zapleteno, saj moramo drevo preurediti, pri
 AVL drevesih pa še uravnotežiti. Druga možnost je leno brisanje, pri katerem
 vozlišča ne odstranimo, temveč ga le označimo kot izbrisano. Tako dobimo
 fantomsko vozlišče, ki ga še vedno uporabljamo pri iskanju po drevesu, med
 elemente drevesa pa ga ne štejemo več.

 Fantomsko drevo predstavimo kot drevo parov `(x, exists)`, kjer `exists` pove,
 ali je element `x` še v drevesu. Še vedno predpostavljamo, da imajo drevesa
 glede na elemente `x` strukturo BST.
[*----------------------------------------------------------------------------*)

type 'a phantom_tree = ('a * bool) tree

(*----------------------------------------------------------------------------*
 Funkcija `phantomize` navadnemu drevesu priredi ekvivalentno fantomsko drevo,
 v katerem so vsi elementi še prisotni. Funkcija `kill x ptree` iz fantomskega
 drevesa izbriše element `x` tako, da ga označi kot izbrisanega. Če elementa ni
 v drevesu, drevo ostane nespremenjeno.

 Namig: Ali lahko uporabite katero od funkcij iz vaj?
[*----------------------------------------------------------------------------*)

let phantomize _ = ()

let kill _ _ = ()

let primer_phantomize = phantomize test_tree
(* val primer_phantomize : int phantom_tree =
     Node (Node (Node (Empty, (0, true), Empty), (2, true), Empty), (5, true),
      Node (Node (Empty, (6, true), Empty), (7, true),
       Node (Empty, (11, true), Empty))) *)

let primer_kill = test_tree |> phantomize |> kill 7 |> kill 3
(* val primer_kill : int phantom_tree =
     Node (Node (Node (Empty, (0, true), Empty), (2, true), Empty), (5, true),
      Node (Node (Empty, (6, true), Empty), (7, false),
       Node (Empty, (11, true), Empty))) *)

(*----------------------------------------------------------------------------*
 Funkcija `unphantomize` fantomskemu drevesu priredi navadno iskalno drevo, ki
 vsebuje zgolj elemente, ki še niso izbrisani. Oblika novega drevesa ni
 pomembna.

 Namig: Lahko uporabite vmesni prehod na drugo podatkovno strukturo.
[*----------------------------------------------------------------------------*)

let unphantomize _ = ()

let primer_unphantomize =
  test_tree |> phantomize |> kill 7 |> kill 0 |> kill 5 |> unphantomize
  |> list_of_tree
(* val primer_unphantomize : int list = [2; 6; 11] *)

(*----------------------------------------------------------------------------*
 Fantomska vozlišča zasedajo prostor in podaljšujejo iskanje, zato drevo
 občasno s funkcijo `unphantomize` zgradimo na novo. Kdaj se to splača?
 Kakšne oblike je drevo, ki ga vrne vaša funkcija `unphantomize`?
[*----------------------------------------------------------------------------*)
