(* ========== Vaja 4: Iskalna Drevesa  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Ocaml omogoča enostavno delo z drevesi. Konstruiramo nov tip dreves, ki so
 bodisi prazna, bodisi pa vsebujejo podatek in imajo dve (morda prazni)
 poddrevesi. Na tej točki ne predpostavljamo ničesar drugega o obliki dreves.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)
type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

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

let leaf x = Node(Empty, x, Empty) 

let test_tree = Node( Node(leaf 0, 2, Empty), 5, Node(leaf 6, 7, leaf 11))

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
  | Node(l, x, r) -> Node(mirror r, x, mirror l)

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
  | Node(l, _, r) -> 1 + max (height l) (height r)

let rec size = function
  | Empty -> 0
  | Node(l, _, r) -> 1 + (size l) + (size r)

(*----------------------------------------------------------------------------*]
 Funkcija [follow directions tree] tipa [direction list -> 'a tree -> 'a option]
 sprejme seznam navodil za premikanje po drevesu in vrne vozlišče do katerega 
 vodi podana pot. Ker navodila morda ne vodijo do nobenega vozlišča v drevesu
 vrne rezultat kot [option] tip.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # follow [Right; Left] test_tree;;
 - : int option = Some 6
 # follow [Right; Left; Right; Right] test_tree;;
 - : int option = None
[*----------------------------------------------------------------------------*)

type direction = Left | Right

let rec follow directions = function
  | Empty -> None
  | Node(l, x, r) ->
    (match directions with
     | [] -> Some x
     | Left :: tl -> follow tl l
     | Right :: tl -> follow tl r)

(*----------------------------------------------------------------------------*]
 Funkcija [prune directions tree] poišče vozlišče v drevesu [t] glede na 
 navodila, ter izbriše poddrevo, ki se začne v izbranem vozlišču.

 Opozorilo: Pri uporabi [Some Node(l, x, r)] se OCaml pritoži, saj to razume
            kot [(Some Node)(l, x, r)], zato pravilno postavite potrebne 
            oklepaje.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # prune [Right] test_tree;;
 - : int tree option =
 Some (Node (Node (Node (Empty, 0, Empty), 2, Empty), 5, Empty))
[*----------------------------------------------------------------------------*)

let rec prune directions tree = 
  match directions, tree with
  | [], _ -> Some Empty
  | _, Empty -> None
  | Left :: tl, Node(l, x, r) ->
    (match prune tl l with
     | None -> None
     | Some new_l -> Some (Node(new_l, x, r)))
  | Right :: tl, Node(l, x, r) ->
    (match prune tl r with
     |None -> None
     | Some new_l -> Some (Node(new_l, x, r)))

(*----------------------------------------------------------------------------*]
 Funkcija [map_tree f tree] preslika drevo [tree] v novo drevo, ki vsebuje
 podatke drevesa [tree] preslikane s funkcijo [f].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # map_tree ((<)3) test_tree;;
 - : bool tree =
 Node (Node (Node (Empty, false, Empty), false, Empty), true,
 Node (Node (Empty, true, Empty), true, Node (Empty, true, Empty)))
[*----------------------------------------------------------------------------*)

let rec map_tree f = function
  | Empty -> Empty
  | Node(l, x, r) -> Node(map_tree f l, f x, map_tree f r)

(*----------------------------------------------------------------------------*]
 Funkcija [list_of_tree] pretvori drevo v seznam. Vrstni red podatkov v seznamu
 naj bo takšen, da v primeru binarnega iskalnega drevesa vrne urejen seznam.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # list_of_tree test_tree;;
 - : int list = [0; 2; 5; 6; 7; 11]
[*----------------------------------------------------------------------------*)

let rec list_of_tree = function
  | Empty -> []
  | Node(l, x, r) -> (list_of_tree l) @ [x] @ (list_of_tree r)

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

let rec is_bst t =
  let rec list_is_ordered = function
    | [] | _ :: [] -> true
    | x :: y :: tl -> if x <= y then list_is_ordered (y :: tl) else false
  in t |> list_of_tree |> list_is_ordered

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
  | Node(l, y, r) when x = y -> Node(l, y, r)
  | Node(l, y, r) when x < y -> Node(insert x l, y, r)
  | Node(l, y, r) when x > y -> Node(l, y, insert x r)

let rec member x = function
  | Empty -> false
  | Node(l, y, r) when x = y -> true
  | Node(l, y, r) when x < y -> member x l
  | Node(l, y, r) when x > y -> member x r

(*----------------------------------------------------------------------------*]
 Funkcija [member2] ne privzame, da je drevo bst.
 
 Opomba: Premislte kolikšna je časovna zahtevnost funkcije [member] in kolikšna
         funkcije [member2] na drevesu z n vozlišči, ki ima globino log(n). 
[*----------------------------------------------------------------------------*)

let rec member2 x = function
  | Empty -> false
  | Node(l, y, r) -> x = y || (member2 x l) || (member2 x r)

(*----------------------------------------------------------------------------*]
 Funkcija [bst_of_list] iz seznama naredi dvojiško iskalno drevo.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # [11; 6; 7; 0; 2; 5] |> bst_of_list |> is_bst;;
 - : bool = true
[*----------------------------------------------------------------------------*)


let bst_of_list list = List.fold_right insert list Empty

(*----------------------------------------------------------------------------*]
 Funkcija [tree_sort] uredi seznam s pomočjo pretvorbe v bst.

 Opomba: Prosim ne uporabljajte te funkcije v praksi.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # tree_sort ["a"; "c"; "f"; "b"; "e"; "d"];;
 - : string list = ["a"; "b"; "c"; "d"; "e"; "f"]
[*----------------------------------------------------------------------------*)

let tree_sort list = list |> bst_of_list |> list_of_tree

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
    | Node(Empty, x, _) -> Some x
    | Node(l, _, _) -> minimal l
  in
  match bst with
  | Empty -> None
  | Node(_, _, r) -> minimal r

let pred bst =
  let rec maximal = function
    | Empty -> None
    | Node(_, x, Empty) -> Some x
    | Node(_, _, r) -> maximal r
  in
  match bst with
  | Empty -> None
  | Node(l, _, _) -> maximal l

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
  | Node(l, y, r) when x > y -> Node(l, y, delete x r)
  | Node(l, y, r) when x < y -> Node(delete x l, y, r)
  | Node(l, y, r) as bst -> (
      (*Potrebno je izbrisati vozlišče.*)
      match succ bst with
      | None -> l (*To se zgodi le kadar je [r] enak [Empty].*)
      | Some s ->
        let clean_r = delete s r in
        Node(l, s, clean_r))

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Druga možnost pri brisanju podatkov je, da spremenimo tip s katerim
 predstavljamo drevo. Definirate nov tip fantomskega drevesa, ki poleg podatka,
 levega in desnega poddrevesa hrani še dodatno informacijo o stanju [state], ki 
 je bodisi [Exists] če je vozlišče še prisotno in pa [Ghost] če je vozlišče v 
 drevesu izbrisano in ga upoštevamo le še kot delitveno vozlišče.
 Še vedno predpostavljamo, da imajo drevesa obliko BST.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type state = Exists | Ghost

type 'a phantom_tree =
  | P_Empty
  | P_Node of 'a phantom_tree * 'a * 'a phantom_tree * state

(*----------------------------------------------------------------------------*]
 Funkcija [phantomize] tipa ['a tree -> 'a phantom_tree] navadnemu drevesu
 priredi ekvivalentno fantomsko drevo.
 Funkcija [kill x ptree] izbriše element [x] v fantomskem drevesu tako, da 
 njegovo stanje nastavi na [Ghost].
 Predpostavite lahko, da v drevesu ni ponovitev elementov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # phantomize test_tree;;
 - : int phantom_tree =
 P_Node (P_Node (P_Node (P_Empty, 0, P_Empty, Exists), 2, P_Empty, Exists), 5,
 P_Node (P_Node (P_Empty, 6, P_Empty, Exists), 7,
 P_Node (P_Empty, 11, P_Empty, Exists), Exists),
 Exists)

 # bst_of_list [3; 4; 2] |> phantomize |> kill 3 |> kill 6;;
 - : int phantom_tree =
 P_Node (P_Empty, 2,
 P_Node (P_Node (P_Empty, 3, P_Empty, Ghost), 4, P_Empty, Exists), Exists)
[*----------------------------------------------------------------------------*)

let rec phantomize = function
  | Empty -> P_Empty
  | Node(l, x, r) ->
    let p_l = phantomize l in
    let p_r = phantomize r in
    P_Node(p_l, x, p_r, Exists)

let rec kill x = function
  | P_Empty -> P_Empty
  | P_Node(p_l, y, p_r, s) when x = y -> P_Node(p_l, y, p_r, Ghost)
  | P_Node(p_l, y, p_r, s) when x < y -> P_Node(kill x p_l, y, p_r, s)
  | P_Node(p_l, y, p_r, s) -> P_Node(p_l, y, kill x p_r, s)
      

(*----------------------------------------------------------------------------*]
 Funkcija [unphantomize] tipa ['a phantom_tree -> 'a tree] fantomskemu drevesu 
 priredi navadno drevo, ki vsebuje zgolj vozlišča, ki še obstajajo. Vrstni red
 vozlišč v končnem drevesu ni pomemben.

 Namig: Lahko uporabite vmesni prehodom na drugo podatkovno strukturo.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # test_tree |> phantomize |> kill 7 |> kill 0 |> kill 5 |> unphantomize;;
 - : int tree = Node (Node (Node (Empty, 2, Empty), 6, Empty), 11, Empty)
[*----------------------------------------------------------------------------*)

let unphantomize ptree =
  let rec list_of_ptree = function
    | P_Empty -> []
    | P_Node(l, x, r, Ghost) -> (list_of_ptree l) @ (list_of_ptree r)
    | P_Node(l, x, r, Exists) -> (list_of_ptree l) @ [x] @ (list_of_ptree r)
  in
  ptree |> list_of_ptree |> bst_of_list
