(*----------------------------------------------------------------------------*
 # Iskalna drevesa
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Dvojiška drevesa

 OCaml omogoča enostavno delo z drevesi. Konstruiramo nov tip dreves, ki so
 bodisi prazna, bodisi pa vsebujejo podatek in imajo dve (morda prazni)
 poddrevesi. Na tej točki ne predpostavljamo ničesar drugega o obliki dreves.
[*----------------------------------------------------------------------------*)

type 'a tree = 
  | Empty 
  | Node of 'a tree * 'a * 'a tree

(*----------------------------------------------------------------------------*
 Definirajte testni primer `test_tree` za preizkušanje funkcij v nadaljevanju.
 Testni primer naj predstavlja spodaj narisano drevo, pomagate pa si lahko s
 pomožno funkcijo `leaf`, ki iz podatka zgradi list.

          5
         / \
        2   7
       /   / \
      0   6   11
[*----------------------------------------------------------------------------*)

let leaf x = Node (Empty, x, Empty)

let test_tree = Empty (* Zapišite testni primer *)

(*----------------------------------------------------------------------------*
 Funkcija `mirror` vrne prezrcaljeno drevo. Na primeru `test_tree` torej vrne

          5
         / \
        7   2
       / \   \
      11  6   0
[*----------------------------------------------------------------------------*)

let mirror _ = ()

let primer_mirror = mirror test_tree
(* val primer_mirror : int tree =
     Node (Node (Node (Empty, 11, Empty), 7, Node (Empty, 6, Empty)), 5,
      Node (Empty, 2, Node (Empty, 0, Empty))) *)

(*----------------------------------------------------------------------------*
 Funkcija `height` vrne višino oz. globino drevesa, funkcija `size` pa število
 vseh vozlišč drevesa.
[*----------------------------------------------------------------------------*)

let height _ = ()

let size _ = ()

let primer_height = height test_tree
(* val primer_height : int = 3 *)

let primer_size = size test_tree
(* val primer_size : int = 6 *)

(*----------------------------------------------------------------------------*
 Funkcija `map_tree f tree` preslika drevo v novo drevo, ki vsebuje podatke
 drevesa `tree`, preslikane s funkcijo `f`.
[*----------------------------------------------------------------------------*)

let map_tree _ _ = ()

let primer_map_tree = map_tree ((<) 3) test_tree
(* val primer_map_tree : bool tree =
     Node (Node (Node (Empty, false, Empty), false, Empty), true,
      Node (Node (Empty, true, Empty), true, Node (Empty, true, Empty))) *)

(*----------------------------------------------------------------------------*
 Funkcija `list_of_tree` pretvori drevo v seznam. Vrstni red podatkov v seznamu
 naj bo takšen, da v primeru dvojiškega iskalnega drevesa vrne urejen seznam.
[*----------------------------------------------------------------------------*)

let list_of_tree _ = ()

let primer_list_of_tree = list_of_tree test_tree
(* val primer_list_of_tree : int list = [0; 2; 5; 6; 7; 11] *)

(*----------------------------------------------------------------------------*
 Funkcija `is_bst` preveri, ali je drevo dvojiško iskalno drevo (Binary Search
 Tree, na kratko BST). Predpostavite, da v drevesu ni ponovitev elementov,
 torej drevo npr. ni oblike `Node (leaf 1, 1, leaf 2)`. Prazno drevo je BST.
[*----------------------------------------------------------------------------*)

let is_bst _ = ()

let primer_is_bst = is_bst test_tree
(* val primer_is_bst : bool = true *)

let primer_is_bst_2 = test_tree |> mirror |> is_bst
(* val primer_is_bst_2 : bool = false *)

(*----------------------------------------------------------------------------*
 ## Dvojiška iskalna drevesa

 V nadaljevanju predpostavljamo, da imajo dvojiška drevesa strukturo BST.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `insert` v iskalno drevo pravilno vstavi dani element. Funkcija
 `member` preveri, ali je dani element v iskalnem drevesu.
[*----------------------------------------------------------------------------*)

let insert _ _ = ()

let member _ _ = ()

let primer_insert = insert 2 (leaf 4)
(* val primer_insert : int tree = Node (Node (Empty, 2, Empty), 4, Empty) *)

let primer_member = member 3 test_tree
(* val primer_member : bool = false *)

(*----------------------------------------------------------------------------*
 Funkcija `member2` ne privzame, da je drevo BST.

 Premislite, kolikšna je časovna zahtevnost funkcije `member` in kolikšna
 funkcije `member2` na drevesu z `n` vozlišči, ki ima globino `log n`.
[*----------------------------------------------------------------------------*)

let member2 _ _ = ()

let primer_member2 = member2 3 test_tree
(* val primer_member2 : bool = false *)

(*----------------------------------------------------------------------------*
 Funkcija `succ` vrne naslednika korena danega drevesa, če obstaja. Za drevo
 oblike `bst = Node (l, x, r)` vrne najmanjši element drevesa `bst`, ki je večji
 od korena `x`. Funkcija `pred` simetrično vrne največji element drevesa, ki je
 manjši od korena, če obstaja.
[*----------------------------------------------------------------------------*)

let succ _ = ()

let pred _ = ()

let primer_succ = succ test_tree
(* val primer_succ : int option = Some 6 *)

let primer_pred = pred (Node (Empty, 5, leaf 7))
(* val primer_pred : int option = None *)

(*----------------------------------------------------------------------------*
 Funkcija `delete x bst` iz drevesa `bst` izbriše element `x`, če ta v drevesu
 obstaja. Element najprej poiščemo enako kot pri `member`. Ko ga najdemo, ga
 v vozlišču nadomestimo z njegovim naslednikom (funkcija `succ`), naslednika
 pa nato izbrišemo iz desnega poddrevesa. Drevo tako ostane iskalno, saj je
 naslednik večji od vseh elementov levega poddrevesa in manjši od vseh
 preostalih elementov desnega. Če naslednika ni, je desno poddrevo prazno in
 vozlišče lahko nadomestimo kar z levim poddrevesom.

 Namesto naslednika lahko uporabimo tudi predhodnika (funkcija `pred`) in ga
 izbrišemo iz levega poddrevesa. Za vajo lahko implementirate oba načina.
[*----------------------------------------------------------------------------*)

let delete _ _ = ()

let primer_delete = delete 7 test_tree
(* val primer_delete : int tree =
     Node (Node (Node (Empty, 0, Empty), 2, Empty), 5,
      Node (Node (Empty, 6, Empty), 11, Empty)) *)

(*----------------------------------------------------------------------------*
 ## AVL drevesa

 Če v iskalno drevo dodajamo že urejene elemente, dobimo drevo, ki je v resnici
 seznam, zato operacije na njem porabijo O(n) časa. Na predavanjih smo zato
 spoznali AVL drevesa. To so iskalna drevesa, pri katerih se v vsakem vozlišču
 višini levega in desnega poddrevesa razlikujeta za največ 1, zato je višina
 AVL drevesa z `n` vozlišči O(log n). Ko v drevo dodajamo elemente, ga po
 potrebi uravnotežimo z rotacijami.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Na papirju v prazno AVL drevo po vrsti dodajte elemente 1, 2, 3, 4, 5, 6 in 7.
 Narišite drevo po vsakem koraku in označite, kje ga je bilo treba zavrteti.
 Nato v prazno AVL drevo dodajte še elemente 3, 1 in 2. Kaj je drugače?
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `is_avl` preveri, ali je drevo uravnoteženo, torej ali se v vsakem
 vozlišču višini levega in desnega poddrevesa razlikujeta za največ 1. Kakšna
 je časovna zahtevnost vaše funkcije na AVL drevesu z `n` vozlišči? Napišite še
 različico, ki porabi le O(n) časa.

 Namig: Pomožna funkcija naj hkrati izračuna višino drevesa in preveri, ali je
 uravnoteženo.
[*----------------------------------------------------------------------------*)

let is_avl _ = ()

let primer_is_avl = is_avl test_tree
(* val primer_is_avl : bool = true *)

let primer_is_avl_2 = is_avl (leaf 1 |> insert 2 |> insert 3)
(* val primer_is_avl_2 : bool = false *)

(*----------------------------------------------------------------------------*
 Funkcija `rotate_left` drevo zavrti v levo, funkcija `rotate_right` pa v
 desno, kot kaže spodnja slika. Če drevesa ne moremo zavrteti, naj funkciji
 javita napako.

          x                       y
         / \     rotate_left     / \
        a   y   ------------>   x   c
           / \  <------------  / \
          b   c  rotate_right a   b

 Preverite, da se seznam `list_of_tree` pri rotaciji ne spremeni, torej da
 rotacija ohranja strukturo BST.
[*----------------------------------------------------------------------------*)

let rotate_left _ = ()

let rotate_right _ = ()

let primer_rotate_left = rotate_left test_tree
(* val primer_rotate_left : int tree =
     Node
      (Node (Node (Node (Empty, 0, Empty), 2, Empty), 5, Node (Empty, 6, Empty)),
      7, Node (Empty, 11, Empty)) *)

let primer_rotate_right = rotate_right test_tree
(* val primer_rotate_right : int tree =
     Node (Node (Empty, 0, Empty), 2,
      Node (Empty, 5, Node (Node (Empty, 6, Empty), 7, Node (Empty, 11, Empty)))) *)

let primer_rotate_left_2 = list_of_tree (rotate_left test_tree)
(* val primer_rotate_left_2 : int list = [0; 2; 5; 6; 7; 11] *)

(*----------------------------------------------------------------------------*
 Na predavanjih smo napisali funkcijo `balance`, ki uravnoteži drevo, katerega
 poddrevesi sta AVL drevesi, njuni višini pa se razlikujeta za največ 2. Pri
 tem si pomaga s funkcijo `imbalance`, ki vrne razliko višin levega in desnega
 poddrevesa. Funkcija `avl_insert` deluje enako kot `insert`, le da drevo na
 poti nazaj uravnoteži s funkcijo `balance`.

 Spodaj so funkcije s predavanj, prepisane z našimi imeni. Odkomentirajte jih
 in z `avl_insert` preverite svojo rešitev naloge na papirju. Premislite še,
 katere primere obravnava funkcija `balance`.
[*----------------------------------------------------------------------------*)

(*
let imbalance = function
  | Empty -> 0
  | Node (l, _, r) -> height l - height r

let balance tree =
  match tree with
  | Node (l, _, _) when imbalance tree = 2 && imbalance l = 1 ->
      rotate_right tree
  | Node (l, x, r) when imbalance tree = 2 ->
      Node (rotate_left l, x, r) |> rotate_right
  | Node (_, _, r) when imbalance tree = -2 && imbalance r = -1 ->
      rotate_left tree
  | Node (l, x, r) when imbalance tree = -2 ->
      Node (l, x, rotate_right r) |> rotate_left
  | _ -> tree

let rec avl_insert x tree =
  match tree with
  | Empty -> Node (Empty, x, Empty)
  | Node (l, y, r) ->
      if x < y then Node (avl_insert x l, y, r) |> balance
      else if x > y then Node (l, y, avl_insert x r) |> balance
      else tree

let primer_avl_insert =
  List.fold_left (fun tree x -> avl_insert x tree) Empty [1; 2; 3; 4; 5; 6; 7]
(* val primer_avl_insert : int tree =
     Node (Node (Node (Empty, 1, Empty), 2, Node (Empty, 3, Empty)), 4,
      Node (Node (Empty, 5, Empty), 6, Node (Empty, 7, Empty))) *)

let primer_avl_insert_2 =
  let xs = List.init 100 (fun i -> i) in
  ( height (List.fold_left (fun tree x -> insert x tree) Empty xs),
    height (List.fold_left (fun tree x -> avl_insert x tree) Empty xs) )
(* val primer_avl_insert_2 : int * int = (100, 7) *)
*)

(*----------------------------------------------------------------------------*
 Funkcija `avl_delete x tree` iz AVL drevesa `tree` izbriše element `x`.
 Element izbrišemo enako kot pri `delete`, le da drevo na poti nazaj
 uravnotežimo s funkcijo `balance`, tako kot pri `avl_insert`.

 Iz spodnjega drevesa `avl_test_tree` izbrišite 9 in rezultat preverite z
 `is_avl`. Zakaj dobimo drevo, ki ni AVL? Popravite funkcijo `balance`, da bo
 delovala tudi pri brisanju.

          7
        /   \
       4     8
      / \     \
     2   5     9
    / \   \
   1   3   6
[*----------------------------------------------------------------------------*)

let avl_test_tree =
  Node
    ( Node (Node (leaf 1, 2, leaf 3), 4, Node (Empty, 5, leaf 6)),
      7,
      Node (Empty, 8, leaf 9) )

let avl_delete _ _ = ()

let primer_avl_delete = avl_delete 9 avl_test_tree
(* val primer_avl_delete : int tree =
     Node (Node (Node (Empty, 1, Empty), 2, Node (Empty, 3, Empty)), 4,
      Node (Node (Empty, 5, Node (Empty, 6, Empty)), 7, Node (Empty, 8, Empty))) *)

let primer_avl_delete_2 = is_avl (avl_delete 9 avl_test_tree)
(* val primer_avl_delete_2 : bool = true *)

(*----------------------------------------------------------------------------*
 (Dodatna naloga) Naj bo `N(h)` najmanjše število vozlišč v AVL drevesu višine
 `h`. Najmanjše AVL drevo višine `h` je sestavljeno iz korena ter najmanjših
 AVL dreves višin `h - 1` in `h - 2` (to so Fibonaccijeva drevesa s
 predavanj), zato velja `N(h) = 1 + N(h - 1) + N(h - 2)`. Pokažite, da je
 `N(h) >= 2^(h/2) - 1`, in od tod sklepajte, da je višina AVL drevesa z `n`
 vozlišči O(log n).
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Slovarji

 S pomočjo BST lahko (zadovoljivo) učinkovito definiramo slovarje. V praksi se
 slovarje definira s pomočjo hash tabel, ki so še učinkovitejše. V
 nadaljevanju pa predpostavimo, da so naši slovarji `dict` dvojiška iskalna
 drevesa, ki v vsakem vozlišču hranijo tako ključ kot tudi pripadajočo
 vrednost, in imajo strukturo BST glede na ključe. Ker slovar potrebuje
 parameter za tip ključa in tip vrednosti, ga parametriziramo kot
 `('key, 'value) dict`.
[*----------------------------------------------------------------------------*)

type ('key, 'value) dict = ('key * 'value) tree

(*----------------------------------------------------------------------------*
 Za testni primer vzemimo slovar `test_dict`:

      "b":1
      /    \
  "a":0  "d":2
         /
     "c":-2
[*----------------------------------------------------------------------------*)

let test_dict : (string, int) dict =
  Node (leaf ("a", 0), ("b", 1), Node (leaf ("c", -2), ("d", 2), Empty))

(*----------------------------------------------------------------------------*
 Funkcija `dict_get key dict` v slovarju poišče vrednost s ključem `key`. Ker
 slovar vrednosti morda ne vsebuje, vrne `option` tip.
[*----------------------------------------------------------------------------*)

let dict_get _ _ = ()

let primer_dict_get = dict_get "banana" test_dict
(* val primer_dict_get : int option = None *)

let primer_dict_get_2 = dict_get "c" test_dict
(* val primer_dict_get_2 : int option = Some (-2) *)

(*----------------------------------------------------------------------------*
 Funkcija `print_dict` sprejme slovar s ključi tipa `string` in vrednostmi tipa
 `int` in v pravilnem vrstnem redu izpiše vrstice "ključ : vrednost" za vsa
 vozlišča slovarja.

 Namig: Uporabite funkciji `print_string` in `print_int`. Nize združujemo z
 operatorjem `^`. V tipu funkcije si oglejte, kako uporaba teh funkcij določi
 parametra za tip ključev in vrednosti v primerjavi s tipom `dict_get`.
[*----------------------------------------------------------------------------*)

let print_dict _ = ()

let primer_print_dict = print_dict test_dict
(* a : 0
   b : 1
   c : -2
   d : 2
   val primer_print_dict : unit = () *)

(*----------------------------------------------------------------------------*
 Funkcija `dict_insert key value dict` v slovar `dict` pod ključ `key` vstavi
 vrednost `value`. Če za nek ključ vrednost že obstaja, jo zamenja.
[*----------------------------------------------------------------------------*)

let dict_insert _ _ _ = ()

let primer_dict_insert = dict_insert "1" 14 test_dict |> print_dict
(* 1 : 14
   a : 0
   b : 1
   c : -2
   d : 2
   val primer_dict_insert : unit = () *)

let primer_dict_insert_2 = dict_insert "c" 14 test_dict |> print_dict
(* a : 0
   b : 1
   c : 14
   d : 2
   val primer_dict_insert_2 : unit = () *)

(*----------------------------------------------------------------------------*
 Napišite primerno signaturo za slovarje `DICT` in naredite implementacijo
 modula `Tree_dict` z drevesi. Modul naj vsebuje prazen slovar `empty` in pa
 funkcije `get`, `insert` in `print` (`print` naj ponovno deluje zgolj na
 `(string, int) t`).
[*----------------------------------------------------------------------------*)

module type DICT = sig
  type ('key, 'value) t

  val empty : ('key, 'value) t
  (* Dodajte manjkajoče! *)
end

module Tree_dict : DICT = struct
  type ('key, 'value) t = ('key, 'value) dict

  let empty = Empty
  (* Dodajte manjkajoče! *)
end

(*----------------------------------------------------------------------------*
 Podobno kot pri funktorju `StetjeRazlicnih` s predavanj napišite funktor
 `Count`, ki sprejme modul s signaturo `DICT`. Vsebuje naj funkcijo
 `count list`, ki s pomočjo slovarja prešteje in izpiše pojavitve posameznih
 elementov v seznamu `list`.
[*----------------------------------------------------------------------------*)

module Count (D : DICT) = struct
  let count _ = ()
end

module Count_tree = Count (Tree_dict)

let primer_count = Count_tree.count ["b"; "a"; "n"; "a"; "n"; "a"]
(* a : 3
   b : 1
   n : 2
   val primer_count : unit = () *)
