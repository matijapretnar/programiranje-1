(*----------------------------------------------------------------------------*
 # Računska zahtevnost
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Urejenost seznama
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Natančno definirajte pogoje, pri katerih rečemo, da funkcija `f` uredi seznam.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Urejanje z izbiranjem

 Pri urejanju z izbiranjem na vsakem koraku ločimo dva podseznama, kjer je prvi
 že urejen, drugi pa vsebuje vse elemente, ki jih je še potrebno urediti. Nato
 zaporedoma prenašamo najmanjši element neurejenega podseznama v urejen
 podseznam, dokler ne uredimo vseh.

 Če pričnemo s praznim urejenim podseznamom, vemo, da so na vsakem koraku vsi
 elementi neurejenega podseznama večji ali enaki elementom urejenega
 podseznama, saj vedno prenesemo najmanjšega. Tako vemo, da moramo naslednji
 najmanjši element dodati na konec urejenega podseznama. Ker je dodajanje na
 začetek seznama hitrejše, urejeni podseznam gradimo v obratnem vrstnem redu in
 ga na koncu obrnemo.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `min_and_rest list` vrne `Some (z, list')`, kjer je `z` najmanjši
 element seznama `list`, seznam `list'` pa je enak seznamu `list` brez prve
 pojavitve elementa `z`. V primeru praznega seznama vrne `None`. Kakšna je
 časovna zahtevnost funkcije?
[*----------------------------------------------------------------------------*)

let min_and_rest _ = ()

let primer_min_and_rest = min_and_rest [3; 1; 2; 1]
(* val primer_min_and_rest : (int * int list) option = Some (1, [3; 2; 1]) *)

(*----------------------------------------------------------------------------*
 Funkcija `selection_sort` je implementacija zgoraj opisanega algoritma.
 Kakšna je njena časovna zahtevnost?

 Namig: Uporabite funkcijo `min_and_rest` iz prejšnje naloge.
[*----------------------------------------------------------------------------*)

let selection_sort _ = ()

let primer_selection_sort = selection_sort [3; 2; 5; 9; 1; 7; 4; 3]
(* val primer_selection_sort : int list = [1; 2; 3; 3; 4; 5; 7; 9] *)

(*----------------------------------------------------------------------------*
 Urejeni podseznam smo gradili v obratnem vrstnem redu in ga na koncu obrnili.
 Kaj pa, če bi namesto tega vsak naslednji element na konec urejenega
 podseznama dodali z `@`? Ali bi se s tem spremenila časovna zahtevnost
 urejanja?
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Urejanje z izbiranjem na tabelah

 Pri delu s tabelami (`array`) namesto s seznami lahko urejanje z izbiranjem
 naredimo "na mestu", tj. brez uporabe vmesnih kopij (delov) vhoda. Kot prej
 tabelo ločimo na že urejen in še neurejen del, le da tokrat vse elemente
 hranimo v vhodni tabeli, mejo med deloma pa podajamo kot argument
 `boundary_sorted` pomožne rekurzivne funkcije (podobno kot pri funkciji
 `obrni_na_mestu` s predavanj). Na vsakem koraku tako ne izvlečemo najmanjšega
 elementa neurejenega dela tabele, temveč poiščemo njegov indeks in ga
 zamenjamo z elementom na meji med deloma (in ga s tem dodamo na konec
 urejenega dela). Postopek končamo, ko meja doseže konec tabele.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `swap a i j` zamenja elementa `a.(i)` in `a.(j)`. Zamenjavo naredi
 na mestu in vrne naj `unit`.
[*----------------------------------------------------------------------------*)

let swap _ _ _ = ()

let primer_swap =
  let test = [|0; 1; 2; 3; 4|] in
  swap test 1 4;
  test
(* val primer_swap : int array = [|0; 4; 2; 3; 1|] *)

(*----------------------------------------------------------------------------*
 Funkcija `index_min a lower upper` poišče indeks najmanjšega elementa tabele
 `a` med indeksoma `lower` in `upper` (oba indeksa sta vključena).
[*----------------------------------------------------------------------------*)

let index_min _ _ _ = ()

let primer_index_min = index_min [|0; 2; 9; 3; 6|] 2 4
(* val primer_index_min : int = 3 *)

(*----------------------------------------------------------------------------*
 Funkcija `selection_sort_array` implementira urejanje z izbiranjem na mestu.
 Kakšna je njena časovna zahtevnost?
[*----------------------------------------------------------------------------*)

let selection_sort_array _ = ()

let primer_selection_sort_array =
  let test = [|3; 2; 5; 9; 1; 7; 4; 3|] in
  selection_sort_array test;
  test
(* val primer_selection_sort_array : int array = [|1; 2; 3; 3; 4; 5; 7; 9|] *)

(*----------------------------------------------------------------------------*
 ## Testiranje

 Funkcija `randlist len max` generira seznam dolžine `len` z naključnimi celimi
 števili med 0 (vključno) in `max` (izključno).
[*----------------------------------------------------------------------------*)

let rec randlist len max =
  if len <= 0 then [] else Random.int max :: randlist (len - 1) max

let primer_randlist = randlist 10 10
(* val primer_randlist : int list = [0; 1; 0; 4; 0; 9; 1; 2; 5; 4] *)

(*----------------------------------------------------------------------------*
 Sedaj lahko s pomočjo `randlist` primerjamo našo urejevalno funkcijo
 (imenovano `our_sort` v spodnjem primeru) z urejevalno funkcijo modula `List`.
 Prav tako lahko na manjšem seznamu preverimo, ali deluje pravilno.

     let test = randlist 100 100 in
     our_sort test = List.sort compare test

[*----------------------------------------------------------------------------*)

let primer_test_selection_sort =
  let test = randlist 100 100 in
  selection_sort test = List.sort compare test
(* val primer_test_selection_sort : bool = true *)

let primer_test_selection_sort_array =
  let test = randlist 100 100 in
  let a = Array.of_list test in
  selection_sort_array a;
  Array.to_list a = List.sort compare test
(* val primer_test_selection_sort_array : bool = true *)

(*----------------------------------------------------------------------------*
 ## Merjenje časa

 Na predavanjih smo napisali funkcijo `stopaj`, ki izpiše, koliko časa porabi
 funkcija `f` na argumentu `x`, ter urejanje z vstavljanjem.
[*----------------------------------------------------------------------------*)

let stopaj f x =
    let zacetek = Sys.time () in
    let y = f x in
    let konec = Sys.time () in
    let izpis =
      Printf.sprintf "Porabljen čas: %f ms" (1000. *. (konec -. zacetek))
    in
    print_endline izpis;
    y

let rec vstavi x xs =
    match xs with
    | x' :: xs' when x > x' -> x' :: vstavi x xs'
    | _ -> x :: xs

let rec uredi_z_vstavljanjem xs =
    match xs with
    | [] -> []
    | x :: xs' -> vstavi x (uredi_z_vstavljanjem xs')

(*----------------------------------------------------------------------------*
 Napišite funkcijo `izmeri sort n`, ki s funkcijo `stopaj` izmeri, koliko časa
 funkcija `sort` porabi za urejanje naraščajočega, padajočega in naključnega
 seznama dolžine `n`.

 Z njo primerjajte funkciji `selection_sort` in `uredi_z_vstavljanjem` za
 dolžine 1000, 2000, 4000 in 8000. Kako se čas spremeni, ko dolžino seznama
 podvojimo? Na katerih seznamih je urejanje z vstavljanjem najhitrejše in
 zakaj? Ali ima tudi urejanje z izbiranjem kakšne posebej ugodne vhode?
[*----------------------------------------------------------------------------*)

let izmeri sort n =
  let narascajoc = List.init n (fun i -> i) in
  let padajoc = List.rev narascajoc in
  let nakljucen = randlist n 1_000_000 in
  List.iter
    (fun (opis, sez) ->
      print_string (opis ^ ": ");
      ignore (stopaj sort sez))
    [("naraščajoč", narascajoc); ("padajoč", padajoc); ("naključen", nakljucen)]

let primer_izmeri = izmeri selection_sort 4_000
(* naraščajoč: Porabljen čas: ... ms
   padajoč: Porabljen čas: ... ms
   naključen: Porabljen čas: ... ms
   val primer_izmeri : unit = () *)
