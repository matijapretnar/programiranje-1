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

let razteg koeficient vektor = 
  List.map (fun x -> koeficient *. x) vektor

let razteg koeficient vektor = 
  let f_raztega x = koeficient *. x in
  List.map f_raztega vektor


(* Spodnji primer ni najboljši v praksi, saj je težko razumljiv *)
let razteg koeficient vektor = 
  List.map ( ( *. ) koeficient ) vektor

let primer_vektorji_1 = razteg 2.0 [1.0; 2.0; 3.0]
(* val primer_vektorji_1 : float list = [2.; 4.; 6.] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `sestej : float list -> float list -> float list`, ki vrne
 vsoto dveh vektorjev.
[*----------------------------------------------------------------------------*)

let sestej v1 v2 = 
  List.map2 ( +. ) v1 v2

let sestej v1 v2 = 
  List.map2 (fun x y -> x +. y ) v1 v2

let primer_vektorji_2 = sestej [1.0; 2.0; 3.0] [4.0; 5.0; 6.0]
(* val primer_vektorji_2 : float list = [5.; 7.; 9.] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `skalarni_produkt : float list -> float list -> float`, ki
 izračuna skalarni produkt dveh vektorjev. Pri tem si lahko pomagate s funkcijo
 `vsota_seznama : float list -> float`, definirano prek funkcije
 `List.fold_left`, ki jo bomo spoznali kasneje:
[*----------------------------------------------------------------------------*)

let vsota_seznama = List.fold_left (+.) 0.

let skalarni_produkt v1 v2 =
  let zmnozeno = List.map2 ( *. ) v1 v2 in
  vsota_seznama zmnozeno

let primer_vektorji_3 = skalarni_produkt [1.0; 2.0; 3.0] [4.0; 5.0; 6.0]
(* val primer_vektorji_3 : float = 32. *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `norma : float list -> float`, ki vrne evklidsko normo
 vektorja.
[*----------------------------------------------------------------------------*)

let norma vektor =
  let produkt = skalarni_produkt vektor vektor in
  sqrt produkt

let primer_vektorji_4 = norma [3.0; 4.0]
(* val primer_vektorji_4 : float = 5. *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `vmesni_kot : float list -> float list -> float`, ki izračuna
 kot med dvema vektorjema v radianih.
[*----------------------------------------------------------------------------*)

let vmesni_kot v1 v2 =
  let norma1 = norma v1 in
  let norma2 = norma v2 in
  let produkt = skalarni_produkt v1 v2 in
  let kosinus_kota = produkt /. (norma1 *. norma2) in
  acos kosinus_kota

let primer_vektorji_5 = vmesni_kot [1.0; 0.0] [0.0; 1.0]
(* val primer_vektorji_5 : float = 1.57079632679489656 *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `normirani : float list -> float list`, ki normira dani
 vektor.
[*----------------------------------------------------------------------------*)

let normirani vektor = 
  let norma_vektorja = norma vektor in
  let normiranje koeficient vek =  
    List.map (fun x -> x /. koeficient) vek in
  normiranje norma_vektorja vektor

(* Ta je boljša, bolj natančno izračuna *)
let normirani vektor = 
  let norma_vektorja = norma vektor in
  razteg ( 1. /. norma_vektorja )  vektor

let primer_vektorji_6 = normirani [3.0; 4.0]
(* val primer_vektorji_6 : float list = [0.600000000000000089; 0.8] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `projeciraj : float list -> float list -> float list`, ki
 izračuna projekcijo prvega vektorja na drugega.
[*----------------------------------------------------------------------------*)

let projekcija v1 v2 = 
  let skalarni = skalarni_produkt v1 v2 in
  let norm = norma v2 in
  razteg (skalarni /. norm) v2

let primer_vektorji_7 = projekcija [3.0; 4.0] [1.0; 0.0]
(* val primer_vektorji_7 : float list = [3.; 0.] *)

(*----------------------------------------------------------------------------*
 ## Generiranje HTML-ja
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `ovij : string -> string -> string`, ki sprejme ime HTML
 oznake in vsebino ter vrne niz, ki predstavlja ustrezno HTML oznako.
[*----------------------------------------------------------------------------*)

let ovij html_oznaka vsebina = 
  "<" ^ html_oznaka ^ ">" ^ vsebina ^ "<" ^ "/" ^html_oznaka ^">"

let primer_html_1 = ovij "h1" "Hello, world!"
(* val primer_html_1 : string = "<h1>Hello, world!</h1>" *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `zamakni : int -> string -> string`, ki sprejme število
 presledkov in niz ter vrne niz, v katerem je vsaka vrstica zamaknjena za
 ustrezno število presledkov.
[*----------------------------------------------------------------------------*)

let zamakni n niz = 
  let presledki = String.make n ' ' in
  let vrstice = String.split_on_char '\n' niz in
  let zamaknjene_vrstice = List.map (fun beseda -> presledki ^ beseda) vrstice in
  String.concat "\n" zamaknjene_vrstice

(* String.concat združi vse podnize seznama nazaj v en niz, z \n med njimi *)

let primer_html_2 = zamakni 4 "Hello,\nworld!"
(* val primer_html_2 : string = "    Hello,\n    world!" *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `ul : string list -> string`, ki sprejme seznam nizov in vrne
 niz, ki predstavlja ustrezno zamaknjen neurejeni seznam v HTML-ju:
[*----------------------------------------------------------------------------*)

let ul seznam_besed =
  let ovite_besede = List.map (ovij "li")  seznam_besed in
  let niz = String.concat "\n" ovite_besede in
  let zamaknjeno = zamakni 2 niz in
  "<ul>\n" ^ zamaknjeno ^ "\n</ul>"

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

let razdeli_vrstico niz = 
  let i = String.index niz ',' in
  let pred = String.sub niz 0 i in
  let za = String.sub niz (i + 2) (String.length niz - i - 2) in
  (pred, za)

let primer_seznam_1 = razdeli_vrstico "mleko, 2"
(* val primer_seznam_1 : string * string = ("mleko", "2") *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `pretvori_v_seznam_parov : string -> (string * string) list`,
 ki sprejme večvrstični niz, kjer je vsaka vrstica niz oblike `"izdelek,
 vrednost"`, in vrne seznam ustreznih parov.
[*----------------------------------------------------------------------------*)

let pretvori_v_seznam_parov niz = 
  let seznam = String.split_on_char '\n' niz in
List.map razdeli_vrstico seznam

let primer_seznam_2 = pretvori_v_seznam_parov "mleko, 2\nkruh, 1\njabolko, 5"
(* val primer_seznam_2 : (string * string) list =
  [("mleko", "2"); ("kruh", "1"); ("jabolko", "5")] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `pretvori_druge_komponente : ('a -> 'b) -> (string * 'a) list
 -> (string * 'b) list`, ki dano funkcijo uporabi na vseh drugih komponentah
 elementov seznama.
[*----------------------------------------------------------------------------*)

let pretvori_druge_komponente funkcija sez = 
  List.map (fun (a, b) -> (a, funkcija b)) sez

let primer_seznam_3 =
  let seznam = [("ata", "mama"); ("teta", "stric")] in
  pretvori_druge_komponente String.length seznam
(* val primer_seznam_3 : (string * int) list = [("ata", 4); ("teta", 5)] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `izracunaj_skupni_znesek : string -> string -> float`, ki
 sprejme večvrstična niza nakupovalnega seznama in cenika in izračuna skupni
 znesek nakupa.
[*----------------------------------------------------------------------------*)
let sortiranje_sez sez = 
  List.sort (fun (a1, _) (a2, _) -> compare a1 a2) sez

let izracunaj_skupni_znesek cenik nakupovalni_seznam = 
  let sez_kolicine = sortiranje_sez (pretvori_v_seznam_parov nakupovalni_seznam) in
  let sez_cen = sortiranje_sez (pretvori_v_seznam_parov cenik) in

  let kaj_kupiti = List.map (fun (x, y) -> x) sez_kolicine in
  let kolicine_string_list = List.map (fun (x, y) -> y) sez_kolicine in
  let cene_string_list = List.map (fun (x, y) -> y) (List.filter (fun (ime, _) -> List.mem ime kaj_kupiti) sez_cen) in
  let kolicine = List.map float_of_string kolicine_string_list in
  let cene = List.map float_of_string cene_string_list in
  let produkti = List.map2 ( *. ) kolicine cene in
  List.fold_left (fun acc x -> acc +. x) 0. produkti

(* Profesorjeva rešitev: *)
let izracunaj_skupni_znesek cenik seznam =
  let cenik =
    cenik
    |> pretvori_v_seznam_parov
    |> pretvori_druge_komponente float_of_string
  in
  let cena_izdelka (izdelek, kolicina) =
    let cena = List.assoc izdelek cenik in
    float_of_int kolicina *. cena
  in
  seznam
  |> pretvori_v_seznam_parov
  |> pretvori_druge_komponente int_of_string
  |> List.map cena_izdelka
  |> vsota_seznama

(* List assoc išče v seznamu parov tisti par, katerega ključ je npr. izdelek in vrne 2. komponento para *)
let primer_seznam_4 = 
  let nakupovalni_seznam = "mleko, 2\njabolka, 5"
  and cenik = "jabolka, 0.5\nkruh, 2\nmleko, 1.5" in
  izracunaj_skupni_znesek cenik nakupovalni_seznam
(* val primer_seznam_4 : float = 5.5 *)
