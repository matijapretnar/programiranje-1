(*----------------------------------------------------------------------------*
 # 1. domača naloga
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Ogrevanje
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ### Collatzovo zaporedje
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Collatzovo zaporedje se začne s pozitivnim naravnim številom $a_0$ ter
 nadaljuje kot:

 $$a_{n + 1} = \begin{cases} a_n / 2, & \text{če je } a_n \text{ sodo} \\ 3 a_n
 + 1, & \text{če je } a_n \text{ liho} \end{cases}$$

 Sestavite funkcijo `collatz : int -> int list`, ki sprejme začetni člen
 zaporedja in vrne seznam vseh členov, dokler zaporedje ne doseže $1$.
[*----------------------------------------------------------------------------*)

  let je_sodo n = 
    let izracun = float_of_int n /. 2. -. float_of_int ( n / 2) in
    if izracun = 0. then true
    else false

let rec collatz int = 
  let stevec = [] in
  match int with
  | 1 -> int :: stevec
  | _ when je_sodo int -> int :: collatz (int / 2)
  | _ -> int :: collatz (3 * int + 1)

let primer_ogrevanje_1 = collatz 1024
(* val primer_ogrevanje_1 : int list =
  [1024; 512; 256; 128; 64; 32; 16; 8; 4; 2; 1] *)

let primer_ogrevanje_2 = collatz 27
(* val primer_ogrevanje_2 : int list =
  [27; 82; 41; 124; 62; 31; 94; 47; 142; 71; 214; 107; 322; 161; 484; 242;
   121; 364; 182; 91; 274; 137; 412; 206; 103; 310; 155; 466; 233; 700; 350;
   175; 526; 263; 790; 395; 1186; 593; 1780; 890; 445; 1336; 668; 334; 167;
   502; 251; 754; 377; 1132; 566; 283; 850; 425; 1276; 638; 319; 958; 479;
   1438; 719; 2158; 1079; 3238; 1619; 4858; 2429; 7288; 3644; 1822; 911;
   2734; 1367; 4102; 2051; 6154; 3077; 9232; 4616; 2308; 1154; 577; 1732;
   866; 433; 1300; 650; 325; 976; 488; 244; 122; 61; 184; 92; 46; 23; 70; 35;
   106; 53; 160; 80; 40; 20; 10; 5; 16; 8; 4; 2; 1] *)

(*----------------------------------------------------------------------------*
 ### Fiksne točke
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite funkcijo `fiksne_tocke : ('a -> 'a) -> 'a list -> 'a list`, ki za
 dano funkcijo in seznam vrne podseznam vseh elementov, ki so fiksne točke.
[*----------------------------------------------------------------------------*)

let fiksne_tocke f x = 
  let preverjanje int = (if int = f int then true else false) in
  List.filter preverjanje x 

let primer_ogrevanje_3 = fiksne_tocke (fun x -> x * x) [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
(* val primer_ogrevanje_3 : int list = [0; 1] *)

let primer_ogrevanje_4 = fiksne_tocke List.rev [[3]; [1; 4; 1]; [5; 9; 2; 6]; [5; 3; 5]; []; [8; 9; 7; 9; 3; 2; 3]]
(* val primer_ogrevanje_4 : int list list = [[3]; [1; 4; 1]; [5; 3; 5]; []] *)

(*----------------------------------------------------------------------------*
 ### Združevanje z ločilom
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `sep_concat : 'a -> 'a list list -> 'a list`, ki združi
 seznam seznamov, pri čemer med elemente različnih seznamov ter na začetek in
 konec vstavi dano ločilo.
[*----------------------------------------------------------------------------*)

let sep_concat int nested_list_int = 
  let f sez = [int] @ sez in
  let int_odspredaj = List.concat_map f nested_list_int in
  int_odspredaj @ [int]

let primer_ogrevanje_5 = sep_concat 42 [[1; 2; 3]; [4; 5]; []; [6]]
(* val primer_ogrevanje_5 : int list = [42; 1; 2; 3; 42; 4; 5; 42; 42; 6; 42] *)

let primer_ogrevanje_6 = sep_concat 42 []
(* val primer_ogrevanje_6 : int list = [42] *)

(*----------------------------------------------------------------------------*
 ### Razbitje seznama
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `partition : int -> 'a list -> 'a list list`, ki sprejme 
 pozitivno naravno število $k$ in seznam $[a_0, \dots, a_n]$ ter ga razdeli na 
 zaporedne podsezname $[a_0, \dots, a_{k - 1}], [a_k, \dots, a_{2 k - 1}], \dots$,
 pri čemer je zadnji podseznam lahko tudi krajši.
[*----------------------------------------------------------------------------*)

let rec take n list =    (* definirane v novah verzijah OCaml*)
  match (n, list) with
  | (0, _) | (_, []) -> []
  | (n, x :: xs) -> x :: take (n - 1) xs 

let rec drop n list =
  match (n, list) with
  | (0, list) -> list
  | (_, []) -> []
  | (n, _ :: xs) -> drop (n - 1) xs 

let rec partition n list =
  match list with
  | [] -> []
  | _ ->
      let first_n = take n list in
      let rest = drop n list in
      first_n :: partition n rest


let primer_ogrevanje_7 = partition 3 [1; 2; 3; 4; 5; 6; 7; 8; 9]
(* val primer_ogrevanje_7 : int list list = [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]] *)

let primer_ogrevanje_8 = partition 3 [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
(* val primer_ogrevanje_8 : int list list =
  [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]; [10]] *)

(*----------------------------------------------------------------------------*
 ## Izomorfizmi množic
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Na predavanjih smo videli, da funkciji `curry : ('a * 'b -> 'c) -> ('a -> ('b
 -> 'c))` in `uncurry : ('a -> ('b -> 'c)) -> ('a * 'b -> 'c)` predstavljata
 izomorfizem množic $C^{A \times B} \cong (C^B)^A$, če kartezični produkt
 predstavimo s produktnim, eksponent pa s funkcijskim tipom.

 Podobno velja tudi za ostale znane izomorfizme, če disjunktno unijo
   $$A + B = \{ \mathrm{in}_1(a) \mid a \in A \} \cup \{ \mathrm{in}_2(b) \mid b
 \in B \}$$
 predstavimo s tipom `('a, 'b) sum`, definiranim z:
[*----------------------------------------------------------------------------*)

type ('a, 'b) sum = In1 of 'a | In2 of 'b

(*----------------------------------------------------------------------------*
 Napišite pare funkcij `phi1` & `psi1`, …, `phi7` & `psi7`, ki predstavljajo
 spodnje izomorfizme množic. Tega, da so si funkcije inverzne, ni treba
 dokazovati.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ### $A \times B \cong B \times A$
[*----------------------------------------------------------------------------*)

(* kartezicni produkt je kot tuple *)

let phi1 par_AB =
  match par_AB with
  | (a, b) -> (b, a)

let psi1 par_BA = phi1 par_BA

(*----------------------------------------------------------------------------*
 ### $A + B \cong B + A$
[*----------------------------------------------------------------------------*)

let  phi2 menjava_AB =
  match menjava_AB with
  | In1 a -> In2 a
  | In2 b -> In1 b

let psi2 menjava_BA = phi2 menjava_BA

(*----------------------------------------------------------------------------*
 ### $A \times (B \times C) \cong (A \times B) \times C$
[*----------------------------------------------------------------------------*)

let phi3 _AparBC =
  match _AparBC with
  | (a, (b, c)) -> ((a, b), c)

let psi3 parAB_C =
  match parAB_C with
  | ((a, b), c) -> (a, (b, c))
(*----------------------------------------------------------------------------*
 ### $A + (B + C) \cong (A + B) + C$
[*----------------------------------------------------------------------------*)

let phi4 _AparBC = 
  match _AparBC with
  | In1 a -> In1 (In1 a )
  | In2 (In1 b) -> In1 (In2 b)
  | In2 (In2 c) -> In2 c

let psi4 parAB_C = 
  match parAB_C with
  | In1 (In1 a ) -> In1 a
  | In1 (In2 b) -> In2 (In1 b)
  | In2 c -> In2 (In2 c)

(*----------------------------------------------------------------------------*
 ### $A \times (B + C) \cong (A \times B) + (A \times C)$
[*----------------------------------------------------------------------------*)

let phi5 _AparBC = 
  match _AparBC with
  | (a, In1 b) -> In1 (a, b)
  | (a, In2 c) -> In2 (a, c)

let psi5 parAB_C = 
  match parAB_C with
  | In1 (a, b) -> (a, In1 b) 
  | In2 (a, c) -> (a, In2 c) 

(*----------------------------------------------------------------------------*
 ### $A^{B + C} \cong A^B \times A^C$
[*----------------------------------------------------------------------------*)

let phi6 f =
  let g b = f (In1 b) in
  let h c = f (In2 c) in
  (g, h)

let psi6 (f, g) = 
  let h x = 
    match x with
    | In1 b -> f b
    | In2 c -> g c in
  h

(*----------------------------------------------------------------------------*
 ### $(A \times B)^C \cong A^C \times B^C$
[*----------------------------------------------------------------------------*)

let phi7 f_v_par = 
  let g x = fst (f_v_par x) in
  let h x = snd (f_v_par x) in
  (g, h)

let psi7 (f, g) = 
  let h x = (f x, g x) in
  h

(*----------------------------------------------------------------------------*
 ## Permutacije
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Permutacije so preureditve elementov $\{0, 1, \dots, n-1\}$, torej bijektivne
 preslikave $$p \colon \{0, 1, \dots, n-1\} \to \{0, 1, \dots, n-1\}.$$ V nalogi
 bomo permutacije predstavili s seznamom števil, v katerem je na $i$-tem mestu
 seznama zapisana slika elementa $i$.
 Na primer, permutaciji $0 \, 1 \, 2 \, 3 \, 4 \, 5 \choose 5 \, 3 \, 2 \, 1 \,
 4 \, 0$ in $0 \, 1 \, 2 \, 3 \, 4 \, 5 \, 6 \, 7 \, 8 \, 9 \choose 3 \, 9 \, 1
 \, 7 \, 5 \, 4 \, 6 \, 8 \, 2 \, 0$ bi zapisali s seznamoma:
[*----------------------------------------------------------------------------*)

let permutacija_1 = [5; 3; 2; 1; 4; 0]
let permutacija_2 = [3; 9; 1; 7; 5; 4; 6; 8; 2; 0]

(* val permutacija_1 : int list = [5; 3; 2; 1; 4; 0] *)
(* val permutacija_2 : int list = [3; 9; 1; 7; 5; 4; 6; 8; 2; 0] *)

(*----------------------------------------------------------------------------*
 ### Kompozitum
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `kompozitum : int list -> int list -> int list`, ki sprejme
 dve permutaciji in vrne njun kompozitum. Za permutaciji $p$ in $q$, je njun
 kompozitum funkcija

 $$ p \circ q \colon i \mapsto p ( q ( i ) ). $$

 Predpostavite lahko, da sta seznama enakih dolžin.
[*----------------------------------------------------------------------------*)

(* permutacija1 od indeksov je že enaka permutaciji 2... aka vzamemo elemente 1. permutacije in jih preslikamo z 2.*)

let kompozitum perm1 perm2 =
  List.map (fun vrednost -> List.nth perm2 vrednost) perm1

let primer_permutacije_1 = kompozitum permutacija_1 permutacija_1
(* val primer_permutacije_1 : int list = [0; 1; 2; 3; 4; 5] *)

let primer_permutacije_2 = kompozitum permutacija_2 permutacija_2
(* val primer_permutacije_2 : int list = [7; 0; 9; 8; 4; 5; 6; 2; 1; 3] *)

(*----------------------------------------------------------------------------*
 ### Inverz
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napiši funkcijo `inverz : int list -> int list`, ki vrne inverz dane
 permutacije $p$, torej tako permutacijo $p^{-1}$, da velja $$p \circ p^{-1} =
 \mathrm{id},$$ kjer je $\mathrm{id}$ indentiteta.
[*----------------------------------------------------------------------------*)

let pari list =
  List.mapi (fun id v -> (id, v)) list

(*check again, ne zdi se optimalno spisan...except če ocenjuješ kodo :) *)
let find_idx list (idx, value) =     
    let index_enake_vrednosti (idx2, value2) = (if value2 = idx then idx2 else 0)  in
    let idx_vec = List.map index_enake_vrednosti list in
    List.fold_left ( + ) 0 idx_vec

let inverz list =
  let seznam_parov = pari list in
  let find_idx_in_list (idx, value) = find_idx seznam_parov (idx, value) in
   List.map find_idx_in_list seznam_parov 

let primer_permutacije_3 = inverz permutacija_1
(* val primer_permutacije_3 : int list = [5; 3; 2; 1; 4; 0] *)

let primer_permutacije_4 = inverz permutacija_2
(* val primer_permutacije_4 : int list = [9; 2; 8; 0; 5; 4; 6; 3; 7; 1] *)

let primer_permutacije_5 = kompozitum permutacija_2 (inverz permutacija_2)
(* val primer_permutacije_5 : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] *)

(*----------------------------------------------------------------------------*
 ### Razcep na cikle
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `cikli : int list -> int list list`, ki za dano permutacijo
 vrne seznam ciklov, ki to permutacijo sestavljajo. Vsak element $\{0, 1, \dots,
 n-1\}$ naj se pojavi v natanko enem ciklu.
[*----------------------------------------------------------------------------*)

let first list = List.hd list   (* slaba praksa menda *)

let cikel paired_list =
  let (idx, value) = first paired_list in
  let rec tracking (id, v) storage = 
    let koncali_cikel x = (x = v) in
    if List.exists koncali_cikel storage then
      storage
    else
      let next_idx = List.assoc v paired_list in
      let next_pair = (v, next_idx) in
      tracking next_pair (storage @ [v]) in
  tracking (idx, value) [idx]


let cikli permutation = 
  let rec aux pair_perm = 
  if pair_perm = [] then []
  else 
    let cycle = cikel pair_perm in
    let not_cycle (idx, value) = not (List.mem value cycle) in
    let rest_pair = List.filter not_cycle pair_perm in
    cycle :: aux rest_pair in
  aux (pari permutation )
  

let primer_permutacije_6 = cikli permutacija_1
(* val primer_permutacije_6 : int list list = [[0; 5]; [1; 3]; [2]; [4]] *)

let primer_permutacije_7 = cikli permutacija_2
(* val primer_permutacije_7 : int list list =
  [[0; 3; 7; 8; 2; 1; 9]; [4; 5]; [6]] *)

let primer_permutacije_8 = cikli (inverz permutacija_2)
(* val primer_permutacije_8 : int list list =
  [[0; 9; 1; 2; 8; 7; 3]; [4; 5]; [6]] *)

(*----------------------------------------------------------------------------*
 ### Transpozicije permutacije
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Vsako permutacijo lahko zapišemo kot produkt transpozicij, torej menjav dveh
 elementov. Na primer, permutacijo $0 \, 1 \, 2 \, 3 \choose 1 \, 0 \, 3 \, 2$
 dobimo kot produkt transpozicij $(0, 1) \circ (2, 3)$.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `iz_transpozicij : int -> (int * int) list -> int list`, ki
 sprejme dolžino permutacije in seznam transpozicij ter vrne permutacijo, ki jim
 ustreza.
[*----------------------------------------------------------------------------*)

let rec iz_transpozicij n pair_list = 
  match pair_list with
  | [] -> []
  | (prvi, drugi) :: xs -> [drugi; prvi] @ (iz_transpozicij n xs)

let primer_permutacije_9 = iz_transpozicij 4 [(0, 1); (2, 3)]
(* val primer_permutacije_9 : int list = [1; 0; 3; 2] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `v_transpozicije : int list -> (int * int) list`, ki zapiše
 permutacijo kot produkt transpozicij, torej menjav dveh elementov. Možnih
 produktov je več, veljati mora le, da je kompozicija dobljenih ciklov enaka
 prvotni permutaciji.

 *Namig: Pomagate si lahko z lastnostjo, da poljubni cikel razpade na
 transpozicije po naslednji formuli*
 $$(i_1, i_2, i_3, \ldots, i_{k-1}, i_k) = (i_1, i_k)\circ(i_1,
 i_{k-1})\circ(i_1, i_3)\circ(i_1, i_2).$$
[*----------------------------------------------------------------------------*)

let rec preveri_par zanke =
    match zanke with 
    | [] -> []
    | cikel :: xs -> 
      match cikel with
      | [] -> [] 
      | [_] -> []
      | [prvi; drugi] -> [prvi, drugi] @ preveri_par xs
      | y :: ys -> List.mapi (fun id x -> (y, x)) (List.rev ys) @ preveri_par xs

let v_transpozicije permutacija = 
  let cycles = cikli permutacija in
    preveri_par cycles

let primer_permutacije_10 = v_transpozicije permutacija_1
(* val primer_permutacije_10 : (int * int) list = [(0, 5); (1, 3)] *)

let primer_permutacije_11 = v_transpozicije permutacija_2
(* val primer_permutacije_11 : (int * int) list =
  [(0, 9); (0, 1); (0, 2); (0, 8); (0, 7); (0, 3); (4, 5)] *)

(*----------------------------------------------------------------------------*
 ## Sudoku
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sudoku je igra, v kateri mrežo $9 \times 9$ dopolnimo s števili od $1$ do $9$,
 tako da se nobeno število v nobeni vrstici, stolpcu ali eni od devetih škatel
 velikosti $3 \times 3$ ne ponovi. Primer začetne postavitve in ustrezne rešitve
 je:

 ```plaintext
 +-------+-------+-------+       +-------+-------+-------+
 | 5 4 . | . 7 . | . . . |       | 5 4 3 | 6 7 8 | 9 1 2 |
 | 6 . . | 1 9 5 | . . . |       | 6 7 2 | 1 9 5 | 3 4 8 |
 | . 9 8 | . . . | . 6 . |       | 1 9 8 | 3 4 2 | 5 6 7 |
 +-------+-------+-------+       +-------+-------+-------+
 | 8 . . | . 6 . | . . 3 |       | 8 1 9 | 7 6 4 | 2 5 3 |
 | 4 . . | 8 . 3 | . . 1 |       | 4 2 6 | 8 5 3 | 7 9 1 |
 | 7 . . | . 2 . | . . 6 |       | 7 3 5 | 9 2 1 | 4 8 6 |
 +-------+-------+-------+       +-------+-------+-------+
 | . 6 . | . . 7 | 8 . . |       | 9 6 1 | 5 3 7 | 8 2 4 |
 | . . . | 4 1 9 | . . 5 |       | 2 8 7 | 4 1 9 | 6 3 5 |
 | . . . | . 8 . | . 7 9 |       | 3 5 4 | 2 8 6 | 1 7 9 |
 +-------+-------+-------+       +-------+-------+-------+
 ```
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Delno izpolnjen sudoku bomo predstavili s tabelo tabel tipa `int option array
 array`, kjer bomo prazna mesta označili z `None`, rešen sudoku pa s tabelo
 tabel običajnih števil.
[*----------------------------------------------------------------------------*)

type mreza = int option array array
type resitev = int array array

(*----------------------------------------------------------------------------*
 Na primer, zgornjo mrežo in rešitev bi predstavili s seznamoma:
[*----------------------------------------------------------------------------*)

let primer_mreze : mreza = [|
  [|Some 5; Some 4; None;   None;   Some 7; None;   None;   None;   None|];
  [|Some 6; None;   None;   Some 1; Some 9; Some 5; None;   None;   None|];
  [|None;   Some 9; Some 8; None;   None;   None;   None;   Some 6; None|];
  [|Some 8; None;   None;   None;   Some 6; None;   None;   None;   Some 3|];
  [|Some 4; None;   None;   Some 8; None;   Some 3; None;   None;   Some 1|];
  [|Some 7; None;   None;   None;   Some 2; None;   None;   None;   Some 6|];
  [|None;   Some 6; None;   None;   None;   Some 7; Some 8; None;   None|];
  [|None;   None;   None;   Some 4; Some 1; Some 9; None;   None;   Some 5|];
  [|None;   None;   None;   None;   Some 8; None;   None;   Some 7; Some 9|]
|]

let primer_resitve : resitev = [|
  [|5; 4; 3; 6; 7; 8; 9; 1; 2|];
  [|6; 7; 2; 1; 9; 5; 3; 4; 8|];
  [|1; 9; 8; 3; 4; 2; 5; 6; 7|];
  [|8; 1; 9; 7; 6; 4; 2; 5; 3|];
  [|4; 2; 6; 8; 5; 3; 7; 9; 1|];
  [|7; 3; 5; 9; 2; 1; 4; 8; 6|];
  [|9; 6; 1; 5; 3; 7; 8; 2; 4|];
  [|2; 8; 7; 4; 1; 9; 6; 3; 5|];
  [|3; 5; 4; 2; 8; 6; 1; 7; 9|];
|]
(* val primer_mreze : mreza =
  [|[|Some 5; Some 4; None; None; Some 7; None; None; None; None|];
    [|Some 6; None; None; Some 1; Some 9; Some 5; None; None; None|];
    [|None; Some 9; Some 8; None; None; None; None; Some 6; None|];
    [|Some 8; None; None; None; Some 6; None; None; None; Some 3|];
    [|Some 4; None; None; Some 8; None; Some 3; None; None; Some 1|];
    [|Some 7; None; None; None; Some 2; None; None; None; Some 6|];
    [|None; Some 6; None; None; None; Some 7; Some 8; None; None|];
    [|None; None; None; Some 4; Some 1; Some 9; None; None; Some 5|];
    [|None; None; None; None; Some 8; None; None; Some 7; Some 9|]|] *)
(* val primer_resitve : resitev =
  [|[|5; 4; 3; 6; 7; 8; 9; 1; 2|]; [|6; 7; 2; 1; 9; 5; 3; 4; 8|];
    [|1; 9; 8; 3; 4; 2; 5; 6; 7|]; [|8; 1; 9; 7; 6; 4; 2; 5; 3|];
    [|4; 2; 6; 8; 5; 3; 7; 9; 1|]; [|7; 3; 5; 9; 2; 1; 4; 8; 6|];
    [|9; 6; 1; 5; 3; 7; 8; 2; 4|]; [|2; 8; 7; 4; 1; 9; 6; 3; 5|];
    [|3; 5; 4; 2; 8; 6; 1; 7; 9|]|] *)

(*----------------------------------------------------------------------------*
 ### Dopolnitev mreže
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `dodaj : int -> int -> int -> mreza -> mreza` tako da `dodaj
 i j n m` vrne mrežo, ki je povsod enaka mreži `m`, le na mestu v vrstici `i` in
 stolpcu `j` ima zapisano število `n`.

 **Pozor:** OCaml dopušča spreminjanje tabel (o tem se bomo učili kasneje). Vaša
 funkcija naj te možnosti ne uporablja, temveč naj sestavi in vrne novo tabelo.
[*----------------------------------------------------------------------------*)

(*let dodaj i j n sudoku = 
  let nov_sudoku = sudoku.(i).(j) <- (Some n) in
  nov_sudoku oz. neki z deep copy
  
  alternativna rešitev spodnji z (i) in (j)*)

let dodaj i j n sudoku =
  let spremeni_kvadratek idx e = 
        if idx != j then e
        else (Some n) in
  let v_pravi_vrstici id ele = 
    if id != i then ele
    else Array.mapi spremeni_kvadratek ele in
  Array.mapi v_pravi_vrstici sudoku 

let primer_sudoku_1 = primer_mreze |> dodaj 0 8 2
(* val primer_sudoku_1 : mreza =
  [|[|Some 5; Some 4; None; None; Some 7; None; None; None; Some 2|];
    [|Some 6; None; None; Some 1; Some 9; Some 5; None; None; None|];
    [|None; Some 9; Some 8; None; None; None; None; Some 6; None|];
    [|Some 8; None; None; None; Some 6; None; None; None; Some 3|];
    [|Some 4; None; None; Some 8; None; Some 3; None; None; Some 1|];
    [|Some 7; None; None; None; Some 2; None; None; None; Some 6|];
    [|None; Some 6; None; None; None; Some 7; Some 8; None; None|];
    [|None; None; None; Some 4; Some 1; Some 9; None; None; Some 5|];
    [|None; None; None; None; Some 8; None; None; Some 7; Some 9|]|] *)

(*----------------------------------------------------------------------------*
 ### Izpiši mrežo
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite funkciji `izpis_mreze : mreza -> string` in `izpis_resitve : resitev
 -> string`, ki sprejmeta mrežo oziroma rešitev in vrneta niz, ki predstavlja
 izpis v zgornji obliki.
[*----------------------------------------------------------------------------*)

(* .copy doesn't work ker kopiraš samo zunanji array, notranji imajo pa še vedno 
isti pointer lahko pa z map prekopiraš vse*)

let string_of_int_opt x =
  match x with
  | None -> ". "
  | Some x -> string_of_int x ^ " "


let rec zapisi_trojico ele =  (* lepše z rekurzijo, preglednejše brez :) *)
    let str_v_sez = List.map string_of_int_opt ele in 
    match str_v_sez with
    | [] -> ""
    | [x; y; z] -> "| " ^  x ^  y ^  z
    | _ -> failwith "Ni trojica"


let pretvori vrstica =
  let seznam = Array.to_list vrstica in
  let razdeljen = partition 3 seznam in
  let seznam_strings = List.map zapisi_trojico razdeljen in
  (String.concat "" seznam_strings) ^ "|\n"
  

let izpis_mreze sudoku =
    let vodoravna = "+-------+-------+-------+\n" in
    let output = "" in
    let vrstice id vrstica = 
      match id with
      | 0|3|6 -> output ^ vodoravna ^ pretvori vrstica
      | 8 -> output ^ pretvori vrstica ^ vodoravna
      | _  -> output ^ pretvori vrstica in
    Array.mapi vrstice sudoku |> Array.to_list |> String.concat ""
    
(* alternativna ideja za takt k maš spet čs:
- črte so lahko separator*)

let primer_sudoku_2 = primer_mreze |> izpis_mreze |> print_endline
(* 
  +-------+-------+-------+
  | 5 4 . | . 7 . | . . . |
  | 6 . . | 1 9 5 | . . . |
  | . 9 8 | . . . | . 6 . |
  +-------+-------+-------+
  | 8 . . | . 6 . | . . 3 |
  | 4 . . | 8 . 3 | . . 1 |
  | 7 . . | . 2 . | . . 6 |
  +-------+-------+-------+
  | . 6 . | . . 7 | 8 . . |
  | . . . | 4 1 9 | . . 5 |
  | . . . | . 8 . | . 7 9 |
  +-------+-------+-------+
  
  val primer_sudoku_2 : unit = ()
*)

(* lepše bi bila nova funkcija ali int v option of int, hitrejše in še vedno pregledno je skoraj ista koda*)

let rec zapisi_trojico_2 ele =  (* lepše z rekurzijo, preglednejše brez :) *)
    let str_v_sez = List.map (fun x -> string_of_int x ^ " ") ele in 
    match str_v_sez with
    | [] -> ""
    | [x; y; z] -> "| " ^  x ^  y ^  z
    | _ -> failwith "Ni trojica"

let pretvori_2 vrstica =
  let seznam = Array.to_list vrstica in
  let razdeljen = partition 3 seznam in
  let seznam_strings = List.map zapisi_trojico_2 razdeljen in
  (String.concat "" seznam_strings) ^ "|\n"


let izpis_resitve sudoku =
    let vodoravna = "+-------+-------+-------+\n" in
    let output = "" in
    let vrstice id vrstica = 
      match id with
      | 0|3|6 -> output ^ vodoravna ^ pretvori_2 vrstica
      | 8 -> output ^ pretvori_2 vrstica ^ vodoravna
      | _  -> output ^ pretvori_2 vrstica in
    Array.mapi vrstice sudoku |> Array.to_list |> String.concat ""


let primer_sudoku_3 = primer_resitve |> izpis_resitve |> print_endline
(*
  +-------+-------+-------+
  | 5 4 3 | 6 7 8 | 9 1 2 |
  | 6 7 2 | 1 9 5 | 3 4 8 |
  | 1 9 8 | 3 4 2 | 5 6 7 |
  +-------+-------+-------+
  | 8 1 9 | 7 6 4 | 2 5 3 |
  | 4 2 6 | 8 5 3 | 7 9 1 |
  | 7 3 5 | 9 2 1 | 4 8 6 |
  +-------+-------+-------+
  | 9 6 1 | 5 3 7 | 8 2 4 |
  | 2 8 7 | 4 1 9 | 6 3 5 |
  | 3 5 4 | 2 8 6 | 1 7 9 |
  +-------+-------+-------+

  val primer_sudoku_3 : unit = ()
*)

(*----------------------------------------------------------------------------*
 ### Preveri, ali rešitev ustreza mreži
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `ustreza : mreza -> resitev -> bool`, ki preveri, ali rešitev
 ustreza dani mreži. Rešitev ustreza mreži, če se na vseh mestih, kjer je v
 mreži podana številka, v rešitvi nahaja enaka številka.
[*----------------------------------------------------------------------------*)

let deep_to_list array_array =
  let sez_arr = Array. to_list array_array in
  List.map (Array.to_list) sez_arr

let deep_map2 nes_arrey1 nes_arrey2 = 
  let zgradi_tuple prvi drugi = (prvi, drugi) in
  let pari_elementov arr1 arr2 = Array.map2 zgradi_tuple arr1 arr2 in
  Array.map2 pari_elementov nes_arrey1 nes_arrey2 |> deep_to_list |> List.concat

let rec preveri_ujemanje pari =
  match pari with
    | ((Some x, y) :: xs) when x != y -> false
    | [] -> true
    | _ :: xs -> preveri_ujemanje xs

let ustreza sudoku resitev = 
  let zdruzena = deep_map2 sudoku resitev in
  preveri_ujemanje zdruzena

(*alternativna ideja exists2*)

let primer_sudoku_4 = ustreza primer_mreze primer_resitve
(* val primer_sudoku_4 : bool = true *)

(*----------------------------------------------------------------------------*
 ### Kandidati za dano prazno mesto
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcije `ni_v_vrstici`, `ni_v_stolpcu` in `ni_v_skatli`, vse tipa
 `mreza * int -> int -> bool`, ki preverijo, ali se v določeni vrstici, stolpcu
 oziroma škatli mreže ne nahaja dano število. Vrstice, stolpci in škatle so
 indeksirani kot:

 ```plaintext
     0 1 2   3 4 5   6 7 8
   +-------+-------+-------+
 0 |       |       |       |
 1 |   0   |   1   |   2   |
 2 |       |       |       |
   +-------+-------+-------+
 3 |       |       |       |
 4 |   3   |   4   |   5   |
 5 |       |       |       |
   +-------+-------+-------+
 6 |       |       |       |
 7 |   6   |   7   |   8   |
 8 |       |       |       |
   +-------+-------+-------+
 ```
[*----------------------------------------------------------------------------*)

let ni_v_vrstici par_mreza_i n  = 
  let (mreza, i) = par_mreza_i in
  let vrstica = mreza.(i) in
  let se_nahaja = Array.exists (fun x -> x = (Some n)) vrstica in
  se_nahaja = false


let primer_sudoku_5 = ni_v_vrstici (primer_mreze, 0) 1
(* val primer_sudoku_5 : bool = true *)

let primer_sudoku_6 = ni_v_vrstici (primer_mreze, 1) 1
(* val primer_sudoku_6 : bool = false *)

(*alternativna : transponiranje arraya ali deep_map2 za to_list in transponiranje seznama*)
let ni_v_stolpcu par_mreza_j n  = 
let (mreza, j) = par_mreza_j in
let se_nahaja = Array.exists (fun x -> x.(j) = (Some n)) mreza in
se_nahaja = false


let deep_partition mreza =
  let seznam = deep_to_list mreza in
  let mini_trojice = List.map (partition 3) seznam in
  (partition 3) mini_trojice

let ni_v_skatli par_mreza_id n  = 
let (mreza, id) = par_mreza_id in
let razdeljen = deep_partition mreza in
let rows_id sez = List.nth sez (id / 3) in
let cols_id sez' = List.nth sez' (id mod 3) in
let rows = rows_id razdeljen  in
let vrednosti = List.map cols_id rows |> List.concat in 
let se_nahaja = List.exists (fun x -> x = (Some n)) vrednosti in
se_nahaja = false


(*----------------------------------------------------------------------------*
 Napišite funkcijo `kandidati : mreza -> int -> int -> int list option`, ki
 sprejme mrežo in indeksa vrstice in stolpca praznega mesta ter vrne seznam vseh
 številk, ki se lahko pojavijo na tem mestu. Če je polje že izpolnjeno, naj
 funkcija vrne `None`.
[*----------------------------------------------------------------------------*)

let vec1_9 = [1; 2; 3; 4; 5; 6; 7; 8; 9]
let najdi_skatlo i j = 3 * (i / 3) + (j / 3)

let kandidati sudoku i j = 
  let preveri x = 
    (ni_v_vrstici (sudoku, i) x) && 
    (ni_v_stolpcu (sudoku, j) x) && 
    (ni_v_skatli (sudoku, najdi_skatlo i j ) x) in
  let se_nahajajo = List.filter preveri vec1_9 in
  match sudoku.(i).(j) with
  | Some _ -> None
  | _ -> Some se_nahajajo

let primer_sudoku_7 = kandidati primer_mreze 0 2
(* val primer_sudoku_7 : int list option = Some [1; 2; 3] *)

let primer_sudoku_8 = kandidati primer_mreze 0 0
(* val primer_sudoku_8 : int list option = None *)

(*----------------------------------------------------------------------------*
 ### Iskanje rešitve
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `resi : mreza -> resitev option`, ki izpolni mrežo sudokuja.
 Če je dana mreža rešljiva, mora funkcija najti rešitev, ki ustreza začetni
 mreži in jo vrniti v obliki `Some resitev`, sicer naj vrne `None`.
 Predpostavite lahko, da je rešitev enolična, zato lahko funkcija vrne prvo, ki
 jo najde.

 *Namig: Poiščite celico mreže z najmanj kandidati in rekurzivno preizkusite vse
 možnosti.*
[*----------------------------------------------------------------------------*)

let kandidati_vseh sudoku = 
  let g i j y = (kandidati sudoku i j, i, j) in
  let f i x = Array.mapi (g i) x in
  Array.mapi f sudoku

let najdi_stevilo kandidati_sudoku = 
  let condition x = 
    match x with
    | Some [_] -> true
    | _ -> false in
  let f row = Array.find_opt (fun (v, i, j) -> condition v) row in
  Array.find_map f kandidati_sudoku

let int_opt_to_int option =
  match option with
  | Some x -> x
  | None -> 0

let rec resi sudoku =
  let kopija_sudoka = Array.map Array.copy sudoku |> Array.copy in
  let kandidati_sudoku = kandidati_vseh kopija_sudoka in
  let stevilo = najdi_stevilo kandidati_sudoku in
  let nov_sudoku = match stevilo with
  | Some (Some [n] , i, j) ->  dodaj i j n sudoku
  | _ -> failwith "" in
  let zdruzen = Array.to_list nov_sudoku |> Array.concat in
  if Array.mem None zdruzen then resi nov_sudoku
  else 
    let ostevilcen = Array.map (fun x -> Array.map int_opt_to_int x) nov_sudoku in
    Some ostevilcen

let primer_sudoku_9 = resi primer_mreze
(* val primer_sudoku_9 : resitev option =
  Some
   [|[|5; 4; 3; 6; 7; 8; 9; 1; 2|]; [|6; 7; 2; 1; 9; 5; 3; 4; 8|];
     [|1; 9; 8; 3; 4; 2; 5; 6; 7|]; [|8; 1; 9; 7; 6; 4; 2; 5; 3|];
     [|4; 2; 6; 8; 5; 3; 7; 9; 1|]; [|7; 3; 5; 9; 2; 1; 4; 8; 6|];
     [|9; 6; 1; 5; 3; 7; 8; 2; 4|]; [|2; 8; 7; 4; 1; 9; 6; 3; 5|];
     [|3; 5; 4; 2; 8; 6; 1; 7; 9|]|] *)
