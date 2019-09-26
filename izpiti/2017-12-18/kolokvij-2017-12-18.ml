(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

(* 1.1) Definirajte funkcijo, ki vzame tri cela števila ter vrne njihov produkt.
   Primer: /zmnozi 2 3 4 = 24/ *)
 let zmnozi x y z = x * y * z

(* 1.2) Definirajte funkcijo, ki vzame celo število x in celo število k, ter
   vrne vrednost izraza x^3 + k.
   Primer: /afin_kub 2 1 = 9/ *)
 let afin_kub x k = zmnozi x x x + k

(* 1.3) Definirajte funkcijo, ki vzame seznam in izračuna seznam vrednosti funkcije
   f(x) = x^3 + 2 za elemente vhodnega seznama.
   Primer: /vse_kubiraj_in_pristej_dva [1; 2; 3] = [3; 10; 29]/ *)
let vse_kubiraj_in_pristej_dva = List.map (fun x -> afin_kub x 2)

(* 1.4) Definirajte funkcijo, ki varno vrne zadnji element seznama v primeru,
   da seznam ni prazen. Uporabite tip option.
   Primer: /zadnji_element [1; 2; 3] = Some 3/ *)
 let rec zadnji_element = function |[] -> None | [x] -> Some x | _ :: xs -> zadnji_element xs

(* 1.5) Definirajte funkcijo, ki izračuna n-to Fibonaccijevo število.
   Pri tem upoštevamo začetna pogoja /fibonacci 0 = 1/ in /fibonacci 1 = 1/.
   Primer: /fibonacci 20 = 10946/ *)
let rec fibonacci n =
  if n <= 1
  then 1
  else fibonacci (n-1) + fibonacci (n-2)

(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)

(* 2.1) Rožno drevo je drevo, v katerem ima vsak koren poljubno mnogo otrok,
   ki so zopet rožna drevesa. Rožna drevesa predstavimo s parametričnim
   tipom /'a drevo/ z enim konstruktorjem, ki sprejme:
   - vrednost (koren) tipa /'a/ in
   - seznam (gozd) dreves tipa /'a drevo/. *)
type 'a drevo = Rose of 'a * 'a drevo list

(* 2.2) Definirajte naslednja rožna drevesa:

   t = 1,  t' =  2   ,      t'' =  3
                / \               /| \
               t  t              / |  \
                               -1  t'  0

 *)

let t = Rose (1,[])
let t' = Rose (2, [t;t])
let t'' = Rose (3, [Rose (-1, []); t'; Rose (0, [])])

(* 2.3) Definirajte funkcijo, ki preveri ali je dano rožno drevo list drevesa,
   torej ima prazen gozd poddreves. *)
let je_list (Rose (_, forest)) = (forest = [])

(* 2.4) Definirajte funkcijo, ki preveri, ali drevo celih števil vsebuje zgolj pozitivna števila. *)
let rec vsa_pozitivna (Rose (root, forest)) =
  let rec for_all f = function
    | [] -> true
    | x :: xs -> f x && for_all f xs
  in
  root > 0 && for_all vsa_pozitivna forest

(* 2.5) Definirajte funkcijo, ki izračuna največjo širino rožnega drevesa, torej največjo dolžino
   gozda, ki se pojavi v kateremkoli vozlišču rožnega drevesa. *)
let rec sirina_drevesa (Rose (_, forest)) =
  List.map sirina_drevesa forest |> List.fold_left max (List.length forest)

(* 2.6) Definirajte funkcijo, ki sestavi (poljubno) rožno drevo globine n.
   Vrednosti v korenih so poljubne. *)
let globoko_drevo n =
  let rec aux acc n =
    if n > 0
    then aux (Rose (n, [acc])) (n-1)
    else acc
  in aux (Rose (n, [])) (n-1)

(* 2.7) Definirajte funkcijo, ki pretvori rožno drevo v seznam. Vrstni red vrednosti v seznamu
   pri tem ni pomemben.
   Primer: /drevo_v_seznam t'' = [3; -1; 2; 1; 1; 0]/ (ali katerakoli permutacija [3; -1; 2; 1; 1; 0])

   Če želite vse točke, mora biti funkcija repno rekurzivna.

   Opomba: kot ste videli na vajah, nekatere funkcije iz modula List,
   na primer List.map, niso repno rekurzivne, zato se jim raje
   izognite. *)


(* Stack predstavlja seznam še vseh neobdelanih poddreves.*)
let drevo_v_seznam t =
  let rec aux (acc : 'a list) (stack : 'a drevo list list) =
    (* Poglej če imamo še kakšen gozd za obdelavo.*)
    match stack with
    | [] -> acc
    | ts :: stack ->
      (* Poglej ali je v gozdu še kakšno drevo.*)
      (match ts with
       | [] -> aux acc stack
       | (Rose (root, forest)) :: ts ->
         (* Dodaj koren v akumulator in gozd v stack. *)
          aux (root :: acc) (forest :: ts :: stack))
 in aux [] [[t]] |> List.rev
