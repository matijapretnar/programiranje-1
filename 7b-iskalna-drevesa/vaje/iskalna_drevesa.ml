(* ===== Vaja 4: Iskalna Drevesa  ===== *)

type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

(* Standardni testni primer.
          5
         / \
        2   7
       /   / \
      0   6   11
*)

let leaf x = Node(Empty, x, Empty) (* Funkcija zakrajši zapis lista drevesa. *)

let test_tree = Node( Node(leaf 0, 2, Empty), 5, Node(leaf 6, 7, leaf 11))

(* Funkcija "mirror t" vrne prezrcaljeno drevo. Na primeru test_tree:
       5
      / \
     7   2
    / \   \
   11  6   0
   ----------
   # mirror test_tree ;;
   - : int tree =
   Node (Node (Node (Empty, 11, Empty), 7, Node (Empty, 6, Empty)), 5,
   Node (Empty, 2, Node (Empty, 0, Empty)))
   ---------- *)

let rec mirror = function
  | Empty -> Empty
  | Node(l, x, r) -> Node(mirror r, x, mirror l)

(* Funkcija "height t" vrne višino oz. globino drevesa, funkcija "size t" pa
   število vseh vozlišč drevesa.
   ----------
   # height test_tree;;
   - : int = 3
   # size test_tree;;
   - : int = 6
   ---------- *)

let rec height = function
  | Empty -> 0
  | Node(l, _, r) -> 1 + max (height l) (height r)

let rec size = function
  | Empty -> 0
  | Node(l, _, r) -> 1 + (size l) + (size r)

(* Funkcija "follow directions t" tipa [direction list -> 'a tree -> 'a option]
   sprejme seznam navodil za premikanje po drevesu. Ker morda navodila ne vodijo
   do nobenega vozlišča v drevesu uporabi tip option.
   ----------
   # follow [Right;Left] test_tree;;
   - : int option = Some 6
   # follow [Right;Left;Right;Right] test_tree;;
   - : int option = None
   ---------- *)

type direction = Left | Right

let rec follow directions = function
  | Empty -> None
  | Node(l, x, r) ->
    (match directions with
     | [] -> Some x
     | Left::tl -> follow tl l
     | Right::tl -> follow tl r)

(* Funkcija "prune directions t"  [direction list -> 'a tree -> 'a tree option]
   poišče vozlišče v drevesu glede na navodila, ter izbriše poddrevo, ki se
   začne v izbranem vozlišču.
   Opozorilo: pri uporabi Some Node(l, x, r) se OCaml pritoži, saj to prebere
   kot (Some Node)(l, x, r) zato pravilno postavi dodatne oklepaje.
   ----------
   # prune [Right] test_tree;;
   - : int tree option =
   Some (Node (Node (Node (Empty, 0, Empty), 2, Empty), 5, Empty))
   ---------- *)

let rec prune directions = function
  | Empty -> None
  | Node(l, x, r) ->
    (match directions with
     | [] -> Some Empty
     | Left::tl ->
       (match prune tl l with
        | None -> None
        | Some new_l -> Some (Node(new_l, x, r)))
     | Right::tl ->
       (match prune tl r with
        | None -> None
        | Some new_r -> Some (Node(l, x, new_r)))
    )

(* Funkcija "map_tree f t"  [('a -> 'b) -> 'a tree -> 'b tree] preslika podatke
   drevesa t s funkcijo f.
   ----------
   # map_tree ((<)3) test_tree;;
   - : bool tree =
   Node (Node (Node (Empty, false, Empty), false, Empty), true,
   Node (Node (Empty, true, Empty), true, Node (Empty, true, Empty)))
   ---------- *)

let rec map_tree f = function
  | Empty -> Empty
  | Node(l, x, r) -> Node( map_tree f l, f x, map_tree f r)

(* Funkcija "list_of_tree t"  ['a tree -> 'a list] preslika podatke
   drevesa t v seznam. Vrstni red naj bo takšen, da v primeru binarnega
   iskalnega drevesa vrne urejen seznam.
   ----------
   # list_of_tree test_tree;;
   - : int list = [0; 2; 5; 6; 7; 11]
   ---------- *)

let rec list_of_tree = function
  | Empty -> []
  | Node(l, x, r) -> (list_of_tree l)@[x]@(list_of_tree r)

(* Funkcija "is_bst t" ['a tree -> bool] preveri ali je drevo binarno iskalno
   drevo (Binary Search Tree, BST). Predpostavi, da v drevesu ni ponovitev
   elementov (torej drevo npr. ni oblike Node( leaf 1, 1, leaf 2)).
   Prazno drevo naj predstavlja BST.
   ----------
   # is_bst test_tree;;
   - : bool = true
   # test_tree |> mirror |> is_bst;;
   - : bool = false
   ---------- *)

let rec is_bst t =
  let rec is_ordered = function
    | [] | _::[] -> true
    | x::y::tl -> if x<=y then is_ordered (y::tl) else false
  in t |> list_of_tree |> is_ordered

(*------------------------------------------------------------------------------
   V nadaljevanju s spremenljivko 'bst' označujemo 'a tree, ki ima strukturo BST
  ----------------------------------------------------------------------------*)

(* Funkcija "insert x bst" ['a -> 'a tree -> 'a tree] v bst vstavi element x.
   Funkcija "member x bst" ['a -> 'a tree -> bool] preveri ali je element v bst.
   ----------
   # insert 2 (leaf 4);;
   - : int tree = Node (Node (Empty, 2, Empty), 4, Empty)
   # member 3 test_tree;;
   - : bool = false
   ---------- *)

let rec insert x = function
  | Empty -> leaf x
  | Node(l, y, r) as t ->
    if x < y then
      Node(insert x l, y, r)
    else if x=y then
      t
    else
      Node(l, y, insert x r)

let rec member x = function
  | Empty -> false
  | Node(l, y, r) ->
    if x < y then
      member x l
    else if x=y then
      true
    else
      member x r

(* Napiši še funkcijo "member2", kjer ne privzameš, da je drevo bst.
   Premisli kakšna je časovna zahtevnost funkcije "member" in kakšna funkcije
   "member2" na drevesu z n vozlišči, ki ima globino log(n). *)

let rec member2 x = function
  | Empty -> false
  | Node(l, y, r) -> x=y || (member2 x l) || (member2 x r)

(* Funkcija "bst_of_list l" ['a list -> 'a tree] iz seznama naredi binarno
   iskalno drevo.
   Namig: na predavanjih je profesor najprej definiral funkcijo "insert", ki
   v bst vstavi en element.
   ----------
   # [11;6;7;0;2;5] |> bst_of_list |> is_bst;;
   - : bool = true
   ---------- *)

let bst_of_list l =
  List.fold_right insert l Empty

(* Sestavi funkcijo "tree_sort l" ['a list -> 'a list], ki uredi seznam l.
   ----------
   # tree_sort ["a";"c";"f";"b";"e";"d"];;
   - : string list = ["a"; "b"; "c"; "d"; "e"; "f"]
   ---------- *)

let tree_sort l = l |> bst_of_list |> list_of_tree

(* Funkcija "succ bst" ['a tree -> 'a option] vrne naslednjika korena drevesa,
   če obstaja. Torej za drevo oblike bst = Node(l, x, r) vrne najmanjši element
   drevesa, ki je večji od x.
   Funkcija "pred bst" ['a tree -> 'a option] simetrično vrne največji element,
   ki je manjši od korena, če obstaja.
   ----------
   # succ test_tree;;
   - : int option = Some 6
   # pred (Node(Empty, 5, leaf 7));;
   - : int option = None
   ---------- *)

let succ bst =
  let rec min = function
    | Empty -> None
    | Node(Empty, x, _) -> Some x
    | Node(l, _, _) -> min l
  in
  match bst with
  | Empty -> None
  | Node(_, _, r) -> min r

let pred bst =
  let rec max = function
    | Empty -> None
    | Node(_, x, Empty) -> Some x
    | Node(_, _, r) -> max r
  in
  match bst with
  | Empty -> None
  | Node(l, _, _) -> max l

(* Na predavanjih ste omenili dva načina brisanja elementov iz drevesa.
   Prvi uporablja "succ", drugi pa "pred".
   Napiši funkcijo "delete x bst" ['a tree -> 'a tree], ki iz drevesa izbriše
   element x, če ta v drevesu obstaja. Za vajo lahko implementiraš obe verziji
   brisanja elementov.
   Namig: Premisli kako izbrišeš x iz dreves oblike
      Node(Empty, x, Empty)
      Node(Empty, x, r)
      Node(l, x, Empty)
      Node(l, x, r)
   ----------
   [Za delete definiran s funkcijo "succ".]
   # delete 7 test_tree;;
   - : int tree =
   Node (Node (Node (Empty, 0, Empty), 2, Empty), 5,
   Node (Node (Empty, 6, Empty), 11, Empty))
   ---------- *)

let rec delete x = function
  | Empty -> Empty
  | Node(l, y, r) as t->
    if x > y then
      Node(l, y, delete x r)
    else if x < y then
      Node(delete x l, y, r)
    else
      (*Potrebno izbrisati vozlišče.*)
      match succ t with
      | None -> l (*To se zgodi le kadar je r enak Empty.*)
      | Some s ->
        let clean_r = delete s r in
        Node(l, s, clean_r)

(* Dodatna možnost je, da spremenimo tip s katerim predstaviljamo drevo.
   Definiraj nov tip drevesa, ki poleg podatka, levega in desnega poddrevesa
   hrani še dodatno informacijo o stanju "state", ki je lahko "Exists" če
   vozlišče upoštevamo in pa "Ghost" če je vozlišče v drevesu le še delitveno. *)

type state = Exists | Ghost

type 'a phantom_tree =
  | P_Empty
  | P_Node of 'a phantom_tree * 'a * 'a phantom_tree * state

(* Funkcija "phantomize t" ['a tree -> 'a phantom_tree], ki navadnemu drevesu
   priredi fantomsko drevo.
   Nato napiši funkcijo "kill x pt" ['a -> 'a phantom_tree -> 'a phantom_tree],
   ki izbriše element v drevesu tako, da njegovo stanje nastavi na Ghost.
   ----------
   # phantomize test_tree;;
   - : int phantom_tree =
   P_Node (P_Node (P_Node (P_Empty, 0, P_Empty, Exists), 2, P_Empty, Exists), 5,
   P_Node (P_Node (P_Empty, 6, P_Empty, Exists), 7,
   P_Node (P_Empty, 11, P_Empty, Exists), Exists),
   Exists)

   # bst_of_list [3;4;2] |> phantomize |> kill 3 |> kill 6;;
   - : int phantom_tree =
   P_Node (P_Empty, 2,
   P_Node (P_Node (P_Empty, 3, P_Empty, Ghost), 4, P_Empty, Exists), Exists)
   ---------- *)

let rec phantomize = function
  | Empty -> P_Empty
  | Node(l, x, r) ->
    let p_l = phantomize l in
    let p_r = phantomize r in
    P_Node(p_l, x, p_r, Exists)

let rec kill x = function
  | P_Empty -> P_Empty
  | P_Node(p_l, y, p_r, s) ->
    if x < y then
      P_Node(kill x p_l, y, p_r, s)
    else if x > y then
      P_Node(p_l, y, kill x p_r, s)
    else
      P_Node(p_l, y, p_r, Ghost)

(* Funkcija "unphantomize pt" ['a phantom_tree -> 'a tree], ki fantomskemu
   drevesu priredi navadno drevo, ki vsebuje le vozlišča, ki še obstajajo.
   Vrstni red vozlišč v končnem drevesu ni pomemben.
   Namig: lahko si pomagaš z vmesnim prehodom na drugo podatkovno strukturo.
   ----------
   # test_tree |> phantomize |> kill 7 |> kill 0 |> kill 5 |> unphantomize;;
   - : int tree = Node (Node (Node (Empty, 2, Empty), 6, Empty), 11, Empty)
   ---------- *)

let unphantomize pt =
  let rec list_of_phantom_tree = function
    | P_Empty -> []
    | P_Node(l, x, r, Ghost) ->
      (list_of_phantom_tree l) @ (list_of_phantom_tree r)
    | P_Node(l, x, r, Exists) ->
      (list_of_phantom_tree l) @ [x] @ (list_of_phantom_tree r)
  in
  pt |> list_of_phantom_tree |> bst_of_list

(*========== Ideje za dodatne vaje ==========*)
(*
1.) Priredi funkciji "insert" in "member" za fantomsko drevo.
2.) Naredi splošnejše odločitveno drevo, ki v vsakem vozlišču hrani primerjalno
    funkcijo, na podlagi katere se v vozlišču odločamo [primer uporabe takšnih
    odločitvenih dreves je področje strojnega učenja, kjer med seboj primerjamo
    vektorje, in se odločamo zgolj glede na vrednost neke določene komponente]
3.) S pomočjo naloge 2 lahko definiramo tudi drevesne slovarje, kjer par
    (podatek, ključ) shranjujemo v drevo glede na vrednosti ključa. Definiraj
    nov tip vozlišč, ki poleg podatka hrani tudi ključ podatka, in definiraj
    nekatere od funkcij "member", "insert", "dict_of_list", ... tako da
    ustrezajo obnašanju slovarjev.
*)
