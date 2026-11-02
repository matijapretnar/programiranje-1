(* ========== Vaje 11: Iskalna Drevesa  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
   Ocaml omogoča enostavno delo z drevesi. Konstruiramo nov tip dreves, ki so
   bodisi prazna, bodisi pa vsebujejo podatek in imajo dve (morda prazni)
   poddrevesi. Na tej točki ne predpostavljamo ničesar drugega o obliki dreves.
  [*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type 'a tree = Empty | Node of 'a tree * 'a * 'a tree

(*----------------------------------------------------------------------------*]
   Definirajmo si testni primer za preizkušanje funkcij v nadaljevanju. Testni
   primer predstavlja spodaj narisano drevo, pomagamo pa si s pomožno funkcijo
   [leaf], ki iz podatka zgradi list.
            5
           / \
          2   7
         /   / \
        0   6   11
  [*----------------------------------------------------------------------------*)

let leaf x = Node (Empty, x, Empty)
let test_tree = Node (Node (leaf 0, 2, Empty), 5, Node (leaf 6, 7, leaf 11))

(*----------------------------------------------------------------------------*]
   Funkcija [mirror] vrne prezrcaljeno drevo. Na primeru [test_tree] torej vrne
            5
           / \
          7   2
         / \   \
        11  6   0
   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   # mirror test_tree ;;
   - : int tree =
   Node (Node (Node (Empty, 11, Empty), 7, Node (Empty, 6, Empty)), 5,
   Node (Empty, 2, Node (Empty, 0, Empty)))
  [*----------------------------------------------------------------------------*)

let rec mirror = function
  | Empty -> Empty
  | Node (l, x, r) -> Node (mirror r, x, mirror l)

(*----------------------------------------------------------------------------*]
   Funkcija [height] vrne višino oz. globino drevesa, funkcija [size] pa število
   vseh vozlišč drevesa.
   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   # height test_tree;;
   - : int = 3
   # size test_tree;;
   - : int = 6
  [*----------------------------------------------------------------------------*)

let rec height = function
  | Empty -> 0
  | Node (l, _, r) -> 1 + max (height l) (height r)

let rec size = function Empty -> 0 | Node (l, _, r) -> 1 + size l + size r

(*----------------------------------------------------------------------------*]
   Funkcija [map_tree f tree] preslika drevo v novo drevo, ki vsebuje podatke
   drevesa [tree] preslikane s funkcijo [f].
   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   # map_tree ((<)3) test_tree;;
   - : bool tree =
   Node (Node (Node (Empty, false, Empty), false, Empty), true,
   Node (Node (Empty, true, Empty), true, Node (Empty, true, Empty)))
  [*----------------------------------------------------------------------------*)

let rec map_tree f = function
  | Empty -> Empty
  | Node (l, x, r) -> Node (map_tree f l, f x, map_tree f r)

(*----------------------------------------------------------------------------*]
   Funkcija [list_of_tree] pretvori drevo v seznam. Vrstni red podatkov v seznamu
   naj bo takšen, da v primeru binarnega iskalnega drevesa vrne urejen seznam.
   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   # list_of_tree test_tree;;
   - : int list = [0; 2; 5; 6; 7; 11]
  [*----------------------------------------------------------------------------*)

let rec list_of_tree = function
  | Empty -> []
  | Node (l, x, r) -> list_of_tree l @ [ x ] @ list_of_tree r

(*----------------------------------------------------------------------------*]
   Funkcija [is_bst] preveri ali je drevo binarno iskalno drevo (Binary Search
   Tree, na kratko BST). Predpostavite, da v drevesu ni ponovitev elementov,
   torej drevo npr. ni oblike Node( leaf 1, 1, leaf 2)). Prazno drevo je BST.
   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   # is_bst test_tree;;
   - : bool = true
   # test_tree |> mirror |> is_bst;;
   - : bool = false
  [*----------------------------------------------------------------------------*)

let is_bst t =
  let rec list_is_ordered = function
    | [] | _ :: [] -> true
    | x :: y :: tl -> if x <= y then list_is_ordered (y :: tl) else false
  in
  t |> list_of_tree |> list_is_ordered

let is_bst' tree =
  let rec is_bst_nonempty tree =
    match tree with
    | Empty -> failwith "Can't happen"
    | Node (Empty, x, Empty) -> (x, true, x)
    | Node (Empty, x, r) ->
        let min, is_bst, max = is_bst_nonempty r in
        (x, is_bst && x < min, max)
    | Node (l, x, Empty) ->
        let min, is_bst, max = is_bst_nonempty l in
        (min, is_bst && x > max, x)
    | Node (l, x, r) ->
        let min_l, is_bst_l, max_l = is_bst_nonempty l in
        let min_r, is_bst_r, max_r = is_bst_nonempty r in
        (min_l, is_bst_l && is_bst_r && max_l < x && x < min_r, max_r)
  in
  match tree with
  | Empty -> true
  | Node (l, x, r) ->
      let _, is_bst, _ = is_bst_nonempty tree in
      is_bst

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
   V nadaljevanju predpostavljamo, da imajo dvojiška drevesa strukturo BST.
  [*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
   Funkcija [insert] v iskalno drevo pravilno vstavi dani element. Funkcija
   [member] preveri ali je dani element v iskalnem drevesu.
   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   # insert 2 (leaf 4);;
   - : int tree = Node (Node (Empty, 2, Empty), 4, Empty)
   # member 3 test_tree;;
   - : bool = false
  [*----------------------------------------------------------------------------*)

let rec insert x = function
  | Empty -> leaf x
  | Node (l, y, r) when x = y -> Node (l, y, r)
  | Node (l, y, r) when x < y -> Node (insert x l, y, r)
  | Node (l, y, r) (* when x > y *) -> Node (l, y, insert x r)

let rec member x = function
  | Empty -> false
  | Node (l, y, r) when x = y -> true
  | Node (l, y, r) when x < y -> member x l
  | Node (l, y, r) (* when x > y *) -> member x r

(*----------------------------------------------------------------------------*]
   Funkcija [member2] ne privzame, da je drevo bst.

   Opomba: Premislte kolikšna je časovna zahtevnost funkcije [member] in kolikšna
   funkcije [member2] na drevesu z n vozlišči, ki ima globino log(n).
  [*----------------------------------------------------------------------------*)

let rec member2 x = function
  | Empty -> false
  | Node (l, y, r) -> x = y || member2 x l || member2 x r

(*----------------------------------------------------------------------------*]
   Funkcija [succ] vrne naslednjika korena danega drevesa, če obstaja. Za drevo
   oblike [bst = Node(l, x, r)] vrne najmanjši element drevesa [bst], ki je večji
   od korena [x].
   Funkcija [pred] simetrično vrne največji element drevesa, ki je manjši od
   korena, če obstaja.
   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   # succ test_tree;;
   - : int option = Some 6
   # pred (Node(Empty, 5, leaf 7));;
   - : int option = None
  [*----------------------------------------------------------------------------*)

let succ bst =
  let rec minimal = function
    | Empty -> None
    | Node (Empty, x, _) -> Some x
    | Node (l, _, _) -> minimal l
  in
  match bst with Empty -> None | Node (_, _, r) -> minimal r

let pred bst =
  let rec maximal = function
    | Empty -> None
    | Node (_, x, Empty) -> Some x
    | Node (_, _, r) -> maximal r
  in
  match bst with Empty -> None | Node (l, _, _) -> maximal l

(*----------------------------------------------------------------------------*]
   Na predavanjih ste omenili dva načina brisanja elementov iz drevesa. Prvi
   uporablja [succ], drugi pa [pred]. Funkcija [delete x bst] iz drevesa [bst]
   izbriše element [x], če ta v drevesu obstaja. Za vajo lahko implementirate
   oba načina brisanja elementov.
   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   # (*<< Za [delete] definiran s funkcijo [succ]. >>*)
   # delete 7 test_tree;;
   - : int tree =
   Node (Node (Node (Empty, 0, Empty), 2, Empty), 5,
   Node (Node (Empty, 6, Empty), 11, Empty))
  [*----------------------------------------------------------------------------*)

let rec delete x = function
  | Empty -> Empty
  | Node (l, y, r) when x > y -> Node (l, y, delete x r)
  | Node (l, y, r) when x < y -> Node (delete x l, y, r)
  | Node (l, y, r) as bst -> (
      (*Potrebno je izbrisati vozlišče.*)
      match succ bst with
      | None -> l (*To se zgodi le kadar je [r] enak [Empty].*)
      | Some s ->
          let clean_r = delete s r in
          Node (l, s, clean_r))

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
   SLOVARJI

   S pomočjo BST lahko (zadovoljivo) učinkovito definiramo slovarje. V praksi se
   slovarje definira s pomočjo hash tabel, ki so še učinkovitejše. V nadaljevanju
   pa predpostavimo, da so naši slovarji [dict] binarna iskalna drevesa, ki v
   vsakem vozlišču hranijo tako ključ kot tudi pripadajočo vrednost, in imajo BST
   strukturo glede na ključe. Ker slovar potrebuje parameter za tip ključa in tip
   vrednosti, ga parametriziramo kot [('key, 'value) dict].
  [*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
   Napišite testni primer [test_dict]:
        "b":1
        /    \
    "a":0  "d":2
           /
       "c":-2
  [*----------------------------------------------------------------------------*)

type ('key, 'value) dict = ('key * 'value) tree

(*----------------------------------------------------------------------------*]
   Funkcija [dict_get key dict] v slovarju poišče vrednost z ključem [key]. Ker
   slovar vrednosti morda ne vsebuje, vrne [option] tip.
   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   # dict_get "banana" test_dict;;
   - : 'a option = None
   # dict_get "c" test_dict;;
   - : int option = Some (-2)
  [*----------------------------------------------------------------------------*)

let test_dict : (string, int) dict =
  Node (leaf ("a", 0), ("b", 1), Node (leaf ("c", -2), ("d", 2), Empty))

(*----------------------------------------------------------------------------*]
   Funkcija [print_dict] sprejme slovar s ključi tipa [string] in vrednostmi tipa
   [int] in v pravilnem vrstnem redu izpiše vrstice "ključ : vrednost" za vsa
   vozlišča slovarja.
   Namig: Uporabite funkciji [print_string] in [print_int]. Nize združujemo z
   operatorjem [^]. V tipu funkcije si oglejte, kako uporaba teh funkcij določi
   parametra za tip ključev in vrednosti v primerjavi s tipom [dict_get].
   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   # print_dict test_dict;;
   a : 0
   b : 1
   c : -2
   d : 2
   - : unit = ()
  [*----------------------------------------------------------------------------*)

let rec dict_get k = function
  | Empty -> None
  | Node (l, (k', v), r) ->
      if k = k' then Some v else if k < k' then dict_get k l else dict_get k r

(*----------------------------------------------------------------------------*]
   Funkcija [dict_insert key value dict] v slovar [dict] pod ključ [key] vstavi
   vrednost [value]. Če za nek ključ vrednost že obstaja, jo zamenja.
   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   # dict_insert "1" 14 test_dict |> print_dict;;
   1 : 14
   a : 0
   b : 1
   c : -2
   d : 2
   - : unit = ()
   # dict_insert "c" 14 test_dict |> print_dict;;
   a : 0
   b : 1
   c : 14
   d : 2
   - : unit = ()
  [*----------------------------------------------------------------------------*)

let rec print_dict = function
  | Empty -> ()
  | Node (d_l, (k, v), d_r) ->
      print_dict d_l;
      print_string (k ^ " : ");
      print_int v;
      print_newline ();
      print_dict d_r

(*----------------------------------------------------------------------------*]
   Napišite primerno signaturo za slovarje [DICT] in naredite implementacijo
   modula z drevesi.

   Modul naj vsebuje prazen slovar [empty] in pa funkcije [get], [insert] in
   [print] (print naj ponovno deluje zgolj na [(string, int) t].
  [*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*]
   Funkcija [count (module Dict) list] prešteje in izpiše pojavitve posameznih
   elementov v seznamu [list] s pomočjo izbranega modula slovarjev.
   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   # count (module Tree_dict) ["b"; "a"; "n"; "a"; "n"; "a"];;
   a : 3
   b : 1
   n : 2
   - : unit = ()
  [*----------------------------------------------------------------------------*)

let rec dict_insert k v = function
  | Empty -> leaf (k, v)
  | Node (l, (k', _), r) when k = k' -> Node (l, (k, v), r)
  | Node (l, (k', v'), r) when k < k' -> Node (dict_insert k v l, (k', v'), r)
  | Node (l, (k', v'), r) (* when k > k' *) ->
      Node (l, (k', v'), dict_insert k v r)

(*
   V praksi v ocamlu za slovarje uporabljamo modul `Map` (poglejte si dokumentacijo).
   Za delo s splošnimi tipi ključev in vrednosti uporabimo `Map.Make`, kjer mu podamo ttip ključa in primerjalno funkcijo za ključe.
*)
(* Nekateri moduli imajo to že vgrajeno *)
module StringMap = Map.Make (String)

module PairMap = Map.Make (struct
  type t = string * int

  let compare (x1, y1) (x2, y2) =
    if x1 = x2 then compare y1 y2 else compare x1 x2

  (* ali na krajše z vgrajeno funkcijo compare *)
  let compare = compare
end)
