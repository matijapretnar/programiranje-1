(* Konstruiraj seznam dolžine "len" z naključnimi vrednostmi med 0 in "max". 

   Primer:

   utop[1]> let l = randlist 10 10 ;;
   val l : int list = [0; 1; 0; 4; 0; 9; 1; 2; 5; 4]

   S pomočjo te funkcije lahko kasneje testiraš algoritme za urejanje.
   let l = (randlist 100 100) in selection_imperative_list l = List.sort compare l;;
 *)
let rec randlist len max = failwith "todo"

(* Funkcija "insert y xs" vstavi "y" v že urejen seznam "xs" in vrne posodobljen
   urejen seznam.
   
   Primer:

   utop[75]> insert 9 [0; 2];;
   - : int list = [0; 2; 9]
   utop[76]> insert 1 [0; 2];;
   - : int list = [0; 1; 2]
   utop[79]> insert 1 [];;
   - : int list = [1]
 *)
let rec insert y = failwith "todo"

(* Prazen seznam je urejen. Seznam lahko uredimo tako, da pravilno vstavljamo
   vse elemente seznama v prazen seznam. S pomočjo te ideje napiši funkcijo
   "insertion_sort" z uporabo "List.fold_left" in "insert". *)
let rec ins_sort l = failwith "todo"

(* Napiši rekurzivno funkcijo, ki sprejme seznam "l" in v primeru, ko "l"
   ni prazen seznam, vrne par "Some (z, l_without_z)" pri čemer je "z" 
   najmanjši element seznama "l" in "l_without_z" ne vsebuje prve ponovitve
   elementa "z". *)
let rec min_and_rest l = failwith "todo"


(* Urejanje z izbiranjem poteka tako, da hranimo seznam "l" ločen v 
   dveh podseznamih. Prvi seznam vsebuje že urejen del seznama, drugi 
   pa vse elemente, ki jih je še potrebno urediti.
   Nato zaporedno izmed še neurejenih elementov izbiramo najmanjšega in
   ga dodamo urejenemu podseznamu. *)

(* Z uporabo "min_and_rest" napiši rekurzivno funkcijo, ki ureja z 
   izbiranjem. *)
let rec selection_sort l = failwith "todo"


(* Pri delu z tabelami (array) namesto seznami, lahko urejanje z izbiranjem
   naredimo "na mestu", t.j. brez uporabe vmesnih kopij (delov) vhoda.
   Kot prej tabelo ločujemo na že urejen del in še neurejen del, le da 
   tokrat vse elemente hranimo v vhodni tabeli, mejo med deloma pa hranimo
   v spremenljivki (npr. "boundary_sorted").
   Na vsakem koraku poiščemo indeks najmanjšega elementa neurejenega dela
   tabele in ga zamenjamo z elementom na meji med deloma (dodamo na konec
   urejenega dela). Postopek končamo, ko meja doseže konec tabele. *)

(* Napiši funkcijo "swap a i j", ki zamenja "a.(i)" in "a.(j)". *)
let swap a i j = failwith "todo"

(* Napiši funkcijo "index_min a lower upper", ki izračuna index najmanjšega
   elementa v "a" med indeksoma "lower" in "upper".
   
   Primer:
   
   index_min [|0; 2; 9; 3; 6|] 2 4 = 4 
*)
let index_min a lower upper = failwith "todo"

(* Konstruiraj urejanje z izbiro na mestu. *)
let selection_imperative a = failwith "todo"

(* Za testiranje urejanja tabel lahko funkcijo pretvoriš v funkcijo, ki 
   ureja sezname z uporabo "Array.of_list" in "Array.to_list". *)
let selection_imperative_list l = failwith "todo"
