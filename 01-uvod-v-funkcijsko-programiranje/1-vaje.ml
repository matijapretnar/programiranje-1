(*----------------------------------------------------------------------------*
 # Uvod v funkcijsko programiranje
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Vektorji
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `razteg : float -> float list -> float list`, ki vektor,
 predstavljen s seznamom števil s plavajočo vejico, pomnoži z danim skalarjem.
[*----------------------------------------------------------------------------*)

(*let razteg _ _ = ()*)
let razteg scalar fl_list = 
  let pomnozi element = element *. scalar in 
  List.map pomnozi fl_list

(* ALTERNATIVNI REŠITVI:
let razteg koef vec =
  List.map (fun x -> x *. koef) vec

let razteg koef vec =
  List.map (( *.) koef) vec
*)


let primer_vektorji_1 = razteg 2.0 [1.0; 2.0; 3.0]
(* val primer_vektorji_1 : float list = [2.; 4.; 6.] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `sestej : float list -> float list -> float list`, ki vrne
 vsoto dveh vektorjev.
[*----------------------------------------------------------------------------*)

let sestej vecA vecB = 
  let sestej ele1 ele2 = ele1 +. ele2 in
  List.map2 sestej vecA vecB

(* ALTERNATIVNE REŠITVE:
let sestej vec1 vec2 =
  List.map2 ( +.) vec1 vec2

let sestej =
  List.map2 ( +.)

let sestej l1, l2 =
  let pari = List.combine l1 l2 in
  List.map (fun (c1, c2) -> c1 .+ c2) pari
*)

let primer_vektorji_2 = sestej [1.0; 2.0; 3.0] [4.0; 5.0; 6.0]
(* val primer_vektorji_2 : float list = [5.; 7.; 9.] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `skalarni_produkt : float list -> float list -> float`, ki
 izračuna skalarni produkt dveh vektorjev. Pri tem si lahko pomagate s funkcijo
 `vsota_seznama : float list -> float`, definirano prek funkcije
 `List.fold_left`, ki jo bomo spoznali kasneje:
[*----------------------------------------------------------------------------*)

let vsota_seznama = List.fold_left (+.) 0.

let skalarni_produkt vec1 vec2 = 
  let produkti = List.map2 ( *. ) vec1 vec2 in
  vsota_seznama produkti

let primer_vektorji_3 = skalarni_produkt [1.0; 2.0; 3.0] [4.0; 5.0; 6.0]
(* val primer_vektorji_3 : float = 32. *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `norma : float list -> float`, ki vrne evklidsko normo
 vektorja.
[*----------------------------------------------------------------------------*)

let norma vec = 
  let kvadratne_vred = skalarni_produkt vec vec in
  sqrt kvadratne_vred

let primer_vektorji_4 = norma [3.0; 4.0]
(* val primer_vektorji_4 : float = 5. *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `vmesni_kot : float list -> float list -> float`, ki izračuna
 kot med dvema vektorjema v radianih.
[*----------------------------------------------------------------------------*)

let vmesni_kot vec1 vec2 = 
  let imenovalec = norma vec1 *. norma vec2 in
  let stevec = skalarni_produkt vec1 vec2 in
  acos (stevec /. imenovalec)

let primer_vektorji_5 = vmesni_kot [1.0; 0.0] [0.0; 1.0]
(* val primer_vektorji_5 : float = 1.57079632679489656 *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `normirani : float list -> float list`, ki normira dani
 vektor.
[*----------------------------------------------------------------------------*)

let normirani vec = 
  let vec_norm = norma vec in
  let divide_by_norm x = x /. vec_norm in
  List.map divide_by_norm vec

let primer_vektorji_6 = normirani [3.0; 4.0]
(* val primer_vektorji_6 : float list = [0.600000000000000089; 0.8] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `projeciraj : float list -> float list -> float list`, ki
 izračuna projekcijo prvega vektorja na drugega.
[*----------------------------------------------------------------------------*)

let projekcija vec base_vec = 
  let normirani_base_vec = normirani base_vec in
  let kvadrat_norm_base_vec = List.map (fun x -> if x = 0. then x else  x /. x ) normirani_base_vec in
  List.map2 ( *. ) kvadrat_norm_base_vec vec

let primer_vektorji_7 = projekcija [3.0; 4.0] [1.0; 0.0]
(* val primer_vektorji_7 : float list = [3.; 0.] *)

(*----------------------------------------------------------------------------*
 ## Generiranje HTML-ja
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `ovij : string -> string -> string`, ki sprejme ime HTML
 oznake in vsebino ter vrne niz, ki predstavlja ustrezno HTML oznako.
[*----------------------------------------------------------------------------*)

let ovij tag text = 
  "<" ^ tag ^ ">" ^ text ^ "</" ^ tag ^ ">"

let primer_html_1 = ovij "h1" "Hello, world!"
(* val primer_html_1 : string = "<h1>Hello, world!</h1>" *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `zamakni : int -> string -> string`, ki sprejme število
 presledkov in niz ter vrne niz, v katerem je vsaka vrstica zamaknjena za
 ustrezno število presledkov.
[*----------------------------------------------------------------------------*)

(* \n je en char , podobn tud t pa r *)

(* let zamakni repetition str = 
  let insert = String.make repetition ' ' in
  let insert_middle = "\n" ^ insert in
  let zamaknjen_zacetek_str = insert ^ str in
  let vzorec = Str.regexp "\\n" in
  Str.global_replace vzorec insert_middle zamaknjen_zacetek_str;; *)

let zamakni repetition str = 
  let insert = String.make repetition ' ' in
  let zamaknjen_zacetek_str = insert ^ str in
  let razcep = String.split_on_char '\n' zamaknjen_zacetek_str in
  String.concat ("\n" ^ insert) razcep

(* let zamakni _ _ = ()*)
let primer_html_2 = zamakni 4 "Hello,\nworld!"
(* val primer_html_2 : string = "    Hello,\n    world!" *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `ul : string list -> string`, ki sprejme seznam nizov in vrne
 niz, ki predstavlja ustrezno zamaknjen neurejeni seznam v HTML-ju:
[*----------------------------------------------------------------------------*)

let ul str_list = 
  let oviti ele = ovij "li" ele in
  let vsi_oviti = List.map oviti str_list in
  let zdruzeni = String.concat "\n" vsi_oviti in
  ovij "ul" ("\n" ^ zamakni 2 zdruzeni ^ "\n")

let primer_html_3 = ul ["ananas"; "banana"; "čokolada"]
(* val primer_html_3 : string =
  "<ul>\n  <li>ananas</li>\n  <li>banana</li>\n  <li>čokolada</li>\n</ul>" *)

(*----------------------------------------------------------------------------*
 ## Nakupovalni seznam
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `razdeli_vrstico : string -> string * string`, ki sprejme
 niz, ki vsebuje vejico, loči na del pred in del za njo.
[*----------------------------------------------------------------------------*)

let razdeli_vrstico str = 
  let razcep = String.split_on_char ',' str in
  let tuple_of_list = function
  | [e1; e2] -> (e1, e2)
  |_ -> failwith "expected two elements" in
  tuple_of_list razcep


let primer_seznam_1 = razdeli_vrstico "mleko, 2"
(* val primer_seznam_1 : string * string = ("mleko", "2") *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `pretvori_v_seznam_parov : string -> (string * string) list`,
 ki sprejme večvrstični niz, kjer je vsaka vrstica niz oblike `"izdelek,
 vrednost"`, in vrne seznam ustreznih parov.
[*----------------------------------------------------------------------------*)

let pretvori_v_seznam_parov str = 
  let seznam_parov = String.split_on_char '\n' str in
  List.map razdeli_vrstico seznam_parov

let primer_seznam_2 = pretvori_v_seznam_parov "mleko, 2\nkruh, 1\njabolko, 5"
(* val primer_seznam_2 : (string * string) list =
  [("mleko", "2"); ("kruh", "1"); ("jabolko", "5")] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `pretvori_druge_komponente : ('a -> 'b) -> (string * 'a) list
 -> (string * 'b) list`, ki dano funkcijo uporabi na vseh drugih komponentah
 elementov seznama.
[*----------------------------------------------------------------------------*)

let pretvori_druge_komponente _ _ = ()

let primer_seznam_3 =
  let seznam = [("ata", "mama"); ("teta", "stric")] in
  pretvori_druge_komponente String.length seznam
(* val primer_seznam_3 : (string * int) list = [("ata", 4); ("teta", 5)] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `izracunaj_skupni_znesek : string -> string -> float`, ki
 sprejme večvrstična niza nakupovalnega seznama in cenika in izračuna skupni
 znesek nakupa.
[*----------------------------------------------------------------------------*)

let izracunaj_skupni_znesek _ _ = ()

let primer_seznam_4 = 
  let nakupovalni_seznam = "mleko, 2\njabolka, 5"
  and cenik = "jabolka, 0.5\nkruh, 2\nmleko, 1.5" in
  izracunaj_skupni_znesek cenik nakupovalni_seznam
(* val primer_seznam_4 : float = 5.5 *)
