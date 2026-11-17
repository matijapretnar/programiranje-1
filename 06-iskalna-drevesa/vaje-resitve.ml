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

let test_tree = 
  Node (
    Node (
      leaf 0,
      2, 
      Empty), 
    5, 
    Node (
      leaf 6, 
      7, 
      leaf 11)
    )

(*----------------------------------------------------------------------------*
 Funkcija `mirror` vrne prezrcaljeno drevo. Na primeru `test_tree` torej vrne

          5
         / \
        7   2
       / \   \
      11  6   0
[*----------------------------------------------------------------------------*)

let rec mirror = function
  | Empty -> Empty
  | Node (l, x, r) -> Node (mirror r, x, mirror l)

let primer_mirror = mirror test_tree
(* val primer_mirror : int tree =
     Node (Node (Node (Empty, 11, Empty), 7, Node (Empty, 6, Empty)), 5,
      Node (Empty, 2, Node (Empty, 0, Empty))) *)

(*----------------------------------------------------------------------------*
 Funkcija `height` vrne višino oz. globino drevesa, funkcija `size` pa število
 vseh vozlišč drevesa.
[*----------------------------------------------------------------------------*)

let rec height = function
  | Empty -> 0
  | Node (l, _, r) -> 1 + max (height l) (height r)

let rec size = function
  | Empty -> 0
  | Node (l, _, r) -> 1 + size l + size r

let primer_height = height test_tree
(* val primer_height : int = 3 *)

let primer_size = size test_tree
(* val primer_size : int = 6 *)

(*----------------------------------------------------------------------------*
 Funkcija `map_tree f tree` preslika drevo v novo drevo, ki vsebuje podatke
 drevesa `tree`, preslikane s funkcijo `f`.
[*----------------------------------------------------------------------------*)

let rec map_tree f = function
  | Empty -> Empty
  | Node (l, x, r) -> Node (map_tree f l, f x, map_tree f r)

let primer_map_tree = map_tree ((<) 3) test_tree
(* val primer_map_tree : bool tree =
     Node (Node (Node (Empty, false, Empty), false, Empty), true,
      Node (Node (Empty, true, Empty), true, Node (Empty, true, Empty))) *)

(*----------------------------------------------------------------------------*
 Funkcija `list_of_tree` pretvori drevo v seznam. Vrstni red podatkov v seznamu
 naj bo takšen, da v primeru dvojiškega iskalnega drevesa vrne urejen seznam.
[*----------------------------------------------------------------------------*)

let rec list_of_tree = function
  | Empty -> []
  | Node (l, x, r) -> list_of_tree l @ [x] @ list_of_tree r

let primer_list_of_tree = list_of_tree test_tree
(* val primer_list_of_tree : int list = [0; 2; 5; 6; 7; 11] *)

(*----------------------------------------------------------------------------*
 Funkcija `is_bst` preveri, ali je drevo dvojiško iskalno drevo (Binary Search
 Tree, na kratko BST). Predpostavite, da v drevesu ni ponovitev elementov,
 torej drevo npr. ni oblike `Node (leaf 1, 1, leaf 2)`. Prazno drevo je BST.
[*----------------------------------------------------------------------------*)

let is_bst_list tree =
  let rec is_ordered = function
    | [] | [_] -> true
    | x :: y :: rest -> x < y && is_ordered (y :: rest)
  in
  tree |> list_of_tree |> is_ordered


let is_bst tree =
  let rec aux = function
    | Empty -> failwith "Ne more se zgoditi"
    | Node (Empty, x, Empty) -> (x, true, x)
    | Node (Empty, x, r) ->
        let min_r, bst_r, max_r = aux r in
        (x, bst_r && x < min_r, max_r)
    | Node (l, x, Empty) ->
        let min_l, bst_l, max_l = aux l in
        (min_l, bst_l && max_l < x, x)
    | Node (l, x, r) ->
        let min_l, bst_l, max_l = aux l in
        let min_r, bst_r, max_r = aux r in
        (min_l, bst_l && bst_r && max_l < x && x < min_r, max_r)
  in
  match tree with
  | Empty -> true
  | _ ->
      let _, bst, _ = aux tree in
      bst

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

let rec insert x = function
  | Empty -> leaf x
  | Node (l, y, r) when x < y -> Node (insert x l, y, r)
  | Node (l, y, r) when x > y -> Node (l, y, insert x r)
  | tree -> tree

let rec member x = function
  | Empty -> false
  | Node (l, y, r) ->
      if x < y then member x l else if x > y then member x r else true

let primer_insert = insert 2 (leaf 4)
(* val primer_insert : int tree = Node (Node (Empty, 2, Empty), 4, Empty) *)

let primer_member = member 3 test_tree
(* val primer_member : bool = false *)

(*----------------------------------------------------------------------------*
 Funkcija `member2` ne privzame, da je drevo BST.

 Premislite, kolikšna je časovna zahtevnost funkcije `member` in kolikšna
 funkcije `member2` na drevesu z `n` vozlišči, ki ima globino `log n`.
[*----------------------------------------------------------------------------*)

let rec member2 x = function
  | Empty -> false
  | Node (l, y, r) -> x = y || member2 x l || member2 x r

let primer_member2 = member2 3 test_tree
(* val primer_member2 : bool = false *)

(*
 Funkcija `member` se spusti le po eni poti od korena proti listom, zato
 porabi O(log n) časa. 
 Funkcija `member2` pa mora v najslabšem primeru pregledati vsa vozlišča, 
 kar nas stane O(n) časa.
*)

(*----------------------------------------------------------------------------*
 Funkcija `succ` vrne naslednika korena danega drevesa, če obstaja. Za drevo
 oblike `bst = Node (l, x, r)` vrne najmanjši element drevesa `bst`, ki je večji
 od korena `x`. Funkcija `pred` simetrično vrne največji element drevesa, ki je
 manjši od korena, če obstaja.
[*----------------------------------------------------------------------------*)

let succ bst =
  let rec minimal = function
    | Empty -> None
    | Node (Empty, x, _) -> Some x
    | Node (l, _, _) -> minimal l
  in
  match bst with
  | Empty -> None
  | Node (_, _, r) -> minimal r

let pred bst =
  let rec maximal = function
    | Empty -> None
    | Node (_, x, Empty) -> Some x
    | Node (_, _, r) -> maximal r
  in
  match bst with
  | Empty -> None
  | Node (l, _, _) -> maximal l

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

let rec delete x = function
  | Empty -> Empty
  | Node (l, y, r) when x < y -> Node (delete x l, y, r)
  | Node (l, y, r) when x > y -> Node (l, y, delete x r)
  | Node (l, _, r) as bst -> (
      match succ bst with
      | None -> l (* To se zgodi le, kadar je `r` prazno. *)
      | Some s -> Node (l, s, delete s r))

let rec delete_pred x = function
  | Empty -> Empty
  | Node (l, y, r) when x < y -> Node (delete_pred x l, y, r)
  | Node (l, y, r) when x > y -> Node (l, y, delete_pred x r)
  | Node (l, _, r) as bst -> (
      match pred bst with
      | None -> r (* To se zgodi le, kadar je `l` prazno. *)
      | Some p -> Node (delete_pred p l, p, r))

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

let rec is_avl = function
  | Empty -> true
  | Node (l, _, r) -> abs (height l - height r) <= 1 && is_avl l && is_avl r

let is_avl' tree =
  let rec aux = function
    | Empty -> Some 0
    | Node (l, _, r) -> (
        match (aux l, aux r) with
        | Some hl, Some hr when abs (hl - hr) <= 1 -> Some (1 + max hl hr)
        | _ -> None)
  in
  aux tree <> None

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

let rotate_left = function
  | Node (a, x, Node (b, y, c)) -> Node (Node (a, x, b), y, c)
  | _ -> failwith "Tega drevesa ne morem zavrteti"

let rotate_right = function
  | Node (Node (a, x, b), y, c) -> Node (a, x, Node (b, y, c))
  | _ -> failwith "Tega drevesa ne morem zavrteti"

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

(*
 Po izbrisu 9 dobimo drevo

          5
        /   \
       4     7
      /     / \
     2     6   8
    / \
   1   3

 ki v vozlišču 4 ni uravnoteženo.
*)

let balance tree =
  match tree with
  | Node (l, _, _) when imbalance tree = 2 && imbalance l >= 0 ->
      rotate_right tree
  | Node (l, x, r) when imbalance tree = 2 ->
      Node (rotate_left l, x, r) |> rotate_right
  | Node (_, _, r) when imbalance tree = -2 && imbalance r <= 0 ->
      rotate_left tree
  | Node (l, x, r) when imbalance tree = -2 ->
      Node (l, x, rotate_right r) |> rotate_left
  | _ -> tree

let rec avl_delete x tree =
  match tree with
  | Empty -> Empty
  | Node (l, y, r) ->
      if x < y then Node (avl_delete x l, y, r) |> balance
      else if x > y then Node (l, y, avl_delete x r) |> balance
      else (
        match succ tree with
        | None -> l
        | Some s -> Node (l, s, avl_delete s r) |> balance)

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

let rec dict_get key = function
  | Empty -> None
  | Node (l, (k, v), r) ->
      if key < k then dict_get key l
      else if key > k then dict_get key r
      else Some v

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

let rec print_dict = function
  | Empty -> ()
  | Node (l, (k, v), r) ->
      print_dict l;
      print_string (k ^ " : ");
      print_int v;
      print_newline ();
      print_dict r

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

let rec dict_insert key value = function
  | Empty -> leaf (key, value)
  | Node (l, (k, v), r) ->
      if key < k then Node (dict_insert key value l, (k, v), r)
      else if key > k then Node (l, (k, v), dict_insert key value r)
      else Node (l, (key, value), r)

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
  val get : 'key -> ('key, 'value) t -> 'value option
  val insert : 'key -> 'value -> ('key, 'value) t -> ('key, 'value) t
  val print : (string, int) t -> unit
end

module Tree_dict : DICT = struct
  type ('key, 'value) t = ('key, 'value) dict

  let empty = Empty
  let get = dict_get
  let insert = dict_insert
  let print = print_dict
end

(*----------------------------------------------------------------------------*
 Podobno kot pri funktorju `StetjeRazlicnih` s predavanj napišite funktor
 `Count`, ki sprejme modul s signaturo `DICT`. Vsebuje naj funkcijo
 `count list`, ki s pomočjo slovarja prešteje in izpiše pojavitve posameznih
 elementov v seznamu `list`.
[*----------------------------------------------------------------------------*)

module Count (D : DICT) = struct
  let count list =
    let rec aux dict = function
      | [] -> D.print dict
      | x :: xs ->
          let n = match D.get x dict with None -> 0 | Some n -> n in
          aux (D.insert x (n + 1) dict) xs
    in
    aux D.empty list
end

module Count_tree = Count (Tree_dict)

let primer_count = Count_tree.count ["b"; "a"; "n"; "a"; "n"; "a"]
(* a : 3
   b : 1
   n : 2
   val primer_count : unit = () *)

(*
 V praksi v OCamlu za slovarje uporabljamo modul `Map` (poglejte si
 dokumentacijo). Slovar s ključi poljubnega tipa dobimo s funktorjem
 `Map.Make`, ki mu podamo modul s tipom ključev in funkcijo `compare` za
 njihovo primerjanje. Nekateri moduli, npr. `String`, ju že vsebujejo.
*)

module String_map = Map.Make (String)
