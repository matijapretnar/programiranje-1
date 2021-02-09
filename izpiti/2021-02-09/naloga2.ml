(*============================================================================*]
  Za učinkovitejše iskanje po leksikografsko urejenih parih bomo uporabili
  leksikografska drevesa, ki jih ustvarimo s pomočjo dvojiških dreves.

    type 'a tree = Empty | Node of 'a tree * 'a * 'a tree

  Leksikografsko drevo za pare tipa ['a * 'b] je dvojiško drevo, ki ima v
  vozlišču element tipa ['a] (da lahko primerjamo po prvi komponenti) in pa
  drevo tipa ['b tree] (za primerjanje po drugi komponenti).

    type ('a, 'b) lexi_tree = ('a * 'b tree) tree

  Par [(a, b)] se nahaja v leksikografskem drevesu, če imamo v drevesu vozlišče
  s parom [(a, subtree)] in se [b] nahaja v [subtree]. 

  Primer drevesa za pare (3, "g"), (3, "t"), (7, "a"), (10, "e"), (10, "r"),
  (10, "t") in (10, "z") je:
          
          (7)--------┐
           |   "a"   |
           └---------┘
          /           \
         /             \
    (3)-------┐     (10)-----------┐
     | "g"    |      |     "r"     |
     |    \   |      |    /   \    |
     |    "t" |      |  "e"   "z"  |
     └--------┘      |       /     |
                     |     "t"     |
                     └-------------┘

[*============================================================================*)

type 'a tree = Empty | Node of 'a tree * 'a * 'a tree

type ('a, 'b) lexi_tree = ('a * 'b tree) tree


(* a *)
(*============================================================================*]
  Definirajte primer, ki ustreza zgornjemu leksikografskemu drevesu.

[*============================================================================*)
let leaf a = Node (Empty, a, Empty)

let three_subtree = Node (Empty, "g", leaf "t")
let seven_subtree = leaf "a"
let ten_subtree = Node (leaf "e", "r", Node(leaf "t", "z", Empty))

let test_tree : (int, string) lexi_tree = 
  Node (leaf (3, three_subtree), (7, seven_subtree), leaf (10, ten_subtree))

(* b *)
(*============================================================================*]
  Napišite funkcijo, ki preveri ali je par prisoten v leksikografskem drevesu.
[*============================================================================*)
let rec mem a = function
  | Empty -> false
  | Node (lt, x, rt) when x = a -> true
  | Node (lt, x, rt) when x < a -> mem a rt
  | Node (lt, x, rt) (* x > a *) -> mem a lt

let rec lexi_mem (a, b) = function
  | Empty -> false
  | Node (lt, (x, btree), rt) when x = a -> mem b btree 
  | Node (lt, (x, btree), rt) when x < a -> lexi_mem (a, b) rt 
  | Node (lt, (x, btree), rt) (* x > a *) -> lexi_mem (a, b) lt 


(* c *)
(*============================================================================*]
  Napišite funkcijo za vstavljanje elementov v leksikografsko drevo.
[*============================================================================*)
let rec insert a = function
  | Empty -> leaf a
  | Node (lt, x, rt) when x = a -> Node (lt, x, rt)
  | Node (lt, x, rt) when x < a -> Node (lt, x, insert a rt) 
  | Node (lt, x, rt) (* x < a *) -> Node (insert a lt, x, rt) 

let rec lexi_insert (a, b) = function
  | Empty -> leaf (a, leaf b)
  | Node (lt, (x, btree), rt) when x = a ->
      Node (lt, (x, insert b btree), rt)
  | Node (lt, (x, btree), rt) when x < a -> 
      Node (lt, (x, btree), lexi_insert (a, b) rt)
  | Node (lt, (x, btree), rt) (* x > a *) ->
      Node (lexi_insert (a, b) lt, (x, btree), rt)


(* d *)
(*============================================================================*]
  Napišite funkcijo [lexi_fold], ki sprejme funkcijo [f] in začetno vrednost
  akumulatorja, nato pa funkcijo zloži preko leksikografskega drevesa. Vrstni
  red zlaganja je določen z leksikografsko urejenostjo.

    lexi_fold : ('a -> 'b -> 'c -> 'a) -> 'a -> ('b, 'c) lexi_tree -> 'a
[*============================================================================*)
let rec fold f acc = function
  | Empty -> acc
  | Node (lt, x, rt) ->
      let left = fold f acc lt in
      let this = f left x in
      fold f this rt
  
let lexi_fold f acc lexi_tree =
  fold 
    (fun acc (x, tree) -> fold (fun acc -> f acc x) acc tree) 
    acc lexi_tree

(* e *)
(*============================================================================*]
  Napišite funkcijo, ki vrne urejen seznam vseh elementov, ki se nahajajo v
  leksikografskem drevesu.
[*============================================================================*)
let to_list tree = lexi_fold (fun acc a b -> (a, b) :: acc) [] tree |> List.rev
