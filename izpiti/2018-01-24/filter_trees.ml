(* ==========================================================================
   NALOGA 2.1

   Filtracijsko drevo ima dve vrsti osnovnih gradnikov:
   - Vozlišča imajo celoštevilsko vrednost, levo poddrevo in desno poddrevo.
   - Listi oz. škatle imajo seznam celoštevilskih vrednosti.

   Primer:
                       10
                     /    \
                    5      15
                  /  \    /  \
                 [1] []  []  [19;20]

   Napišite tip, ki predstavlja filtracijsko drevo in nato konstruirajte
   zgornji primer.
   ========================================================================== *)

type filter_tree = Node of int * filter_tree * filter_tree
                 | Box of int list

let example_tree = Node(10,
                Node(5, (Box [1]), (Box [])),
                Node(15, (Box []), (Box [19;20])))

(* ==========================================================================
   NALOGA 2.2

   Filtracijsko drevo razvršča števila v škatle glede na njihovo vrednost.
   Vozlišče z vrednostjo "k" razvrsti število "n" v levo poddrevo če velja
   n <= k oz. v desno poddrevo če velja n > k.
   Ko število doseže škatlo, ga dodamo v seznam števil v škatli.
   Škatle lahko vsebujejo ponovitve in niso nujno urejene.

   Napišite funkcijo, ki sprejme število in filtracijsko drevo in vrne
   filtracijsko drevo z vstavljenim številom.

   Primer:
               10                                        10
             /    \            insert 12 t             /    \
    t =     5      15          ------------>          5      15
          /  \    /  \                              /  \    /  \
         [1] []  []  [19;20]                       [1] [] [12] [19;20]
   ========================================================================== *)
let rec insert x ftree =
  match ftree with
  | Node(f, lt, rt) ->
    if f > x
    then Node(f, insert x lt, rt)
    else Node(f, lt, insert x rt)
  | Box(xs) -> Box(x::xs)

(* ==========================================================================
   NALOGA 2.3

   Napišite funkcijo, ki sprejem seznam celih števil in filtracijsko drevo
   in vrne filtracijsko drevo z vstavljenimi elementi seznama.
   Vrstni red vstavljanja ni pomemben.
   ========================================================================== *)

let rec insert_many l ftree =
  List.fold_right insert l ftree

(* ==========================================================================
   NALOGA 2.4

   Definirajte funkcijo, ki sprejme filtracijsko drevo in preveri ali
   so vsa števila v pravilnih škatlah glede na način razvrščanja.

   Primer:
       5                                      5
     /   \    ----> true                    /   \    ----> false
   [1;2] [7]                              [1]   [2;7]
   ========================================================================== *)

let rec boxed_correctly ftree =
  let checker lower upper x =
    match (lower, upper) with
    | (None, None) -> true
    | (Some l, None) -> l <= x
    | (None, Some u) -> x < u
    | (Some l, Some u) -> l <= x && x < u
  in
  let rec values_between lower upper ftree =
    match ftree with
    | Box(xs) -> List.for_all (checker lower upper) xs
    | Node(f, lt, rt) ->
      (values_between lower (Some f) lt) && (values_between (Some f) upper rt)
  in
  values_between None None ftree
