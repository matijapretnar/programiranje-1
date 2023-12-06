(* ========== Vaje 5: Urejanje  ========== *)

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*]
 Urejanje z Vstavljanjem
[*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

(*----------------------------------------------------------------------------*]
 Funkcija [insert y xs] vstavi [y] v že urejen seznam [xs] in vrne urejen
 seznam. 
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 9 [0; 2];;
 - : int list = [0; 2; 9]
 # insert 1 [4; 5];;
 - : int list = [1; 4; 5]
 # insert 7 [];;
 - : int list = [7]
[*----------------------------------------------------------------------------*)
let rec insert y xs =
  match xs with
  |z :: zs when y < z -> y::z::zs
  |z :: zs -> z::(insert y zs)
  |[] -> [y]

(*----------------------------------------------------------------------------*]
 Prazen seznam je že urejen. Funkcija [insert_sort] uredi seznam tako da
 zaporedoma vstavlja vse elemente seznama v prazen seznam.
[*----------------------------------------------------------------------------*)
let insert_sort lst =
  let rec aux list acc =
    match list with
    |x :: xs -> aux xs (insert x acc)
    |[] -> acc
  in
  aux lst []


(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*]
 Urejanje z Izbiranjem
[*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)


(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
Pri urejanju z izbiranjem na vsakem koraku ločimo dva podseznama, kjer je prvi
že urejen, drugi pa vsebuje vse elemente, ki jih je še potrebno urediti. Nato
zaporedoma prenašamo najmanjši element neurejenega podseznama v urejen
podseznam, dokler ne uredimo vseh. 

Če pričnemo z praznim urejenim podseznamom, vemo, da so na vsakem koraku vsi
elementi neurejenega podseznama večji ali enaki elementom urejenega podseznama,
saj vedno prenesemo najmanjšega. Tako vemo, da moramo naslednji najmanjši člen
dodati na konec urejenega podseznama.
(Hitreje je obrniti vrstni red seznama kot na vsakem koraku uporabiti [@].)
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)


(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*]
Urejanje z Izbiranjem na Tabelah
[*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
Pri delu z tabelami (array) namesto seznami, lahko urejanje z izbiranjem 
naredimo "na mestu", t.j. brez uporabe vmesnih kopij (delov) vhoda. Kot prej
tabelo ločujemo na že urejen del in še neurejen del, le da tokrat vse elemente
hranimo v vhodni tabeli, mejo med deloma pa hranimo v spremenljivki
[boundary_sorted]. Na vsakem koraku tako ne izvlečemo najmanjšega elementa
neurejenga dela tabele temveč poiščemo njegov indeks in ga zamenjamo z
elementom na meji med deloma (in s tem dodamo na konec urejenega dela).
Postopek končamo, ko meja doseže konec tabele.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
Funkcija [swap a i j] zamenja elementa [a.(i)] and [a.(j)]. Zamenjavo naredi
na mestu in vrne unit.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# let test = [|0; 1; 2; 3; 4|];;
val test : int array = [|0; 1; 2; 3; 4|]
# swap test 1 4;;
- : unit = ()
# test;;
- : int array = [|0; 4; 2; 3; 1|]
[*----------------------------------------------------------------------------*)
let swap a i j =
  let x = a.(i) in
  a.(i) <- a.(j);
  a.(j) <- x

(*----------------------------------------------------------------------------*]
Funkcija [index_min a lower upper] poišče indeks najmanjšega elementa tabele
[a] med indeksoma [lower] and [upper] (oba indeksa sta vključena).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
index_min [|0; 2; 9; 3; 6|] 2 4 = 4
[*----------------------------------------------------------------------------*)
let index_min a lower upper =
  let min = ref a.(lower) in
  let index = ref lower in
  for i = lower to upper do
    if !min > a.(i) then (min := a.(i); index := i)
  done;
  !index


(*----------------------------------------------------------------------------*]
Funkcija [selection_sort_array] implementira urejanje z izbiranjem na mestu. 
[*----------------------------------------------------------------------------*)
let selection_sort_array a =
  let n = Array.length a in
  for i = 0 to (n-1) do
    swap a i (index_min a i (n-1))
  done;
  a

(*----------------------------------------------------------------------------*]
Funkcija [min_and_rest list] vrne par [Some (z, list')] tako da je [z]
najmanjši element v [list] in seznam [list'] enak [list] z odstranjeno prvo
pojavitvijo elementa [z]. V primeru praznega seznama vrne [None]. 
[*----------------------------------------------------------------------------*)
let min_and_rest list =
  match list with
  |[] -> None
  |x :: xs ->
      let min = ref x in
      let rec aux list =
        match list with
        |[] -> min
        |x :: xs -> if !min > x then ((min := x); (aux xs))
            else aux xs
      in
      let rec remove list z =
        match list with
        |[] -> []
        |x::xs  when x = z -> xs 
        |x::xs -> x::(remove xs z)
      in
      Some (!(aux xs), (remove (x::xs) !(aux xs)))

(*----------------------------------------------------------------------------*]
Funkcija [selection_sort] je implementacija zgoraj opisanega algoritma.
Namig: Uporabi [min_and_rest] iz prejšnje naloge.
[*----------------------------------------------------------------------------*)
let selection_sort list =
  let rec aux list acc =
    match min_and_rest list with
    |None -> acc
    |Some(x, xs) -> aux xs (x::acc)
  in
  List.rev (aux list [])

(*----------------------------------------------------------------------------*]
Funkcija [randlist len max] generira seznam dolžine [len] z naključnimi
celimi števili med 0 in [max].
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# let l = randlist 10 10 ;;
val l : int list = [0; 1; 0; 4; 0; 9; 1; 2; 5; 4]
[*----------------------------------------------------------------------------*)
;;
Random.self_init ();;
let rec randlist len max =
  if len <= 0 then []
  else (Random.int(max)) :: (randlist (len-1) max)


(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
Sedaj lahko s pomočjo [randlist] primerjamo našo urejevalno funkcijo (imenovana
[our_sort] v spodnjem primeru) z urejevalno funkcijo modula [List]. Prav tako
lahko na manjšem seznamu preverimo v čem je problem.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
let test = (randlist 100 100) in (our_sort test = List.sort compare test);;
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)