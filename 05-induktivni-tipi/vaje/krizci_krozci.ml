(* ========== Vaja 8: Uporaba tipov  ========== *)

(*----------------------------------------------------------------------------*]
 V nadaljevanju si bomo ogledali kako modeliramo igro tri v vrsto, kjer bo
 poudarek na uporabi primernih tipov.

 Da vam delo olajšamo, so nekatere od funkcij že napisane. Za začetek bomo
 pripravili tipe, s katerimi bomo predstavili stanje igralne plošče in možna
 stanje igre.
[*----------------------------------------------------------------------------*)


(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
                                IGRALNO POLJE
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Definirajte vsotni tip, ki predstavlja igralca. Tip naj ima dva konstruktorja,
 ki ne rabita hraniti dodatnih podatkov. 

 Namig: Najlažje je, da igralca poimenujete kar glede na simbol.
[*----------------------------------------------------------------------------*)

type player = unit  (* DOPOLNI ME *)

(*----------------------------------------------------------------------------*]
 Igralno mrežo predstavimo s trojico vrstic, kjer je vsaka vrstica trojica polj.
 Vsako polje v mreži je prazno ali pa ga je zasedel igralec tipa [player].

 Za polje uporabite že vgrajeni tip, ki predstavlja vrednost ali pa odsotnost
 vrednosti.

 Namig: Vaša koda bo lepša, če definirate še dodatne tipe.
[*----------------------------------------------------------------------------*)

type grid = unit  (* DOPOLNI ME *)

(*----------------------------------------------------------------------------*]
 Definirajte vrednost, ki predstavlja prazno mrežo.  
[*----------------------------------------------------------------------------*)

let empty_grid : grid = () (* DOPOLNI ME *)

(*----------------------------------------------------------------------------*]
 Ker je mreža fiksne velikosti 3x3 lahko definiramo poseben tip za številčenje.
 Če bi uporabljali celoštevilski tip [int] bi morali stalno preverjati, ali
 smo v mejah mreže, čemur se izognemo z uporabo tipa [index]
[*----------------------------------------------------------------------------*)

type index = Zero | One | Two

(*----------------------------------------------------------------------------*]
 Funkcija [get_index] sprejme [index] in trojico, ter vrne primerni element
 trojice.

 Funkcija [set_index] sprejme [index], trojico in vrednost, ter vrne novo
 trojico, ki ima primerno polje posodobljeno na podano vrednost.
[*----------------------------------------------------------------------------*)

let get_index index (x0, x1, x2) = failwith "DOPOLNI ME"

let set_index index x (x0, x1, x2) = failwith "DOPOLNI ME"

(*----------------------------------------------------------------------------*]
 Funkcija [get_field] vrne vrednost polja v mreži, ki ga določata podana
 indeksa.

 Funkcija [set_field] posodobi polje v mreži glede na indeksa in podano
 vrednost. 
[*----------------------------------------------------------------------------*)

let get_field (row_i : index) (col_i : index) grid = 
  get_index col_i (get_index row_i grid)

let set_field (row_i : index) (col_i : index) x grid =
  let old_row = get_index row_i grid in
  let new_row = set_index col_i x old_row in
  set_index row_i new_row grid

(*----------------------------------------------------------------------------*]
 Poleg operacij na mreži potrebujemo tudi način, da preverimo ali je kateri od
 igralcev zmagal. Prav tako moramo ugotoviti, ali je polje že zapolnjeno.

 Funkcija [is_full_row] preveri ali so vsa polja v vrstici že zapolnjena,
 funkcija [is_full_grid] pa preveri zapolnjenost mreže.
[*----------------------------------------------------------------------------*)

let is_full_row row = failwith "DOPOLNI ME"

let is_full_grid grid =
    let (r1, r2, r3) = grid in
    (* Preklopimo na seznam, da lahko uporabimo knjižnico *)
    List.for_all is_full_row [r1; r2; r3]

(*----------------------------------------------------------------------------*]
 Funkcija [winner_of_triple] preveri ali je kateri od igralcev zmagal v treh
 poljih. Če zmagovalca ni, naj vrne [None].

 V funkciji [winner_of_list] preverimo seznam trojic. Če ima katera koli od
 trojic zmagovalca, ga funkcija vrne, sicer pa vrne [None]. Primer kjer hkrati
 zmagata oba igralca lahko zanemarite.

 Funkcija [winner_of_grid] poišče možnega zmagovalca mreže, tako da generira
 seznam vseh možnosti in preveri seznam.
[*----------------------------------------------------------------------------*)

let winner_of_triple triple : player option = failwith "DOPOLNI ME"

let winner_of_list triples : player option = failwith "DOPOLNI ME"

let winner_of_grid grid =
  (* Pripravimo si vse trojice, kjer bi lahko dosegli tri v vrsto. *)
  let (a00, a01, a02), (a10, a11, a12), (a20, a21, a22) = grid in
  let row_triples = [ (a00, a01, a02); (a10, a11, a12); (a20, a21, a22) ] in
  let column_triples = [ (a00, a10, a20); (a01, a11, a21); (a02, a12, a22) ] in
  let diagonal_triples = [ (a00, a11, a22); (a02, a11, a20) ] in
  let triples = row_triples @ column_triples @ diagonal_triples in
  (* Sedaj pogledamo kdo je zmagovalec v seznamu vseh možnosti. *)
  winner_of_list triples

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
                                LOGIKA IGRE
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Definirajte vsotni tip, ki predstavlja končni rezultat igre. Igra se lahko 
 konča bodisi z zmago nekega igralca bodisi z remijem.  
[*----------------------------------------------------------------------------*)

type result = unit  (* DOPOLNI ME *)

(*----------------------------------------------------------------------------*]
 Stanje igre predstavimo z vsotnim tipom [state]. Ali je na potezi eden od
 igralcev, ali pa se je igra končala. V obeh primerih prav tako hranimo stanje
 mreže, saj ga moramo posodobiti in/ali prikazati.

 Pri večjih projektih je za stanja bolje uporabljati zapisne tipe (kot jih
 uporablja [state]) namesto preprostih n-teric, saj omogočajo boljšo
 preglednost.
[*----------------------------------------------------------------------------*)

type state = 
  | OnTurn of {player : player; grid : grid} 
  | GameOver of {result : result; final_grid : grid}

let initial_state = "DOPOLNI ME"  (* in dodaj anotacijo [: state] *) 

(*----------------------------------------------------------------------------*]
 Funkcija [other_player] sprejme igralca in vrne njegovega nasprotnika.
 
 Funkcija [place_token] sprejme igralca, ki je trenutno na potezi, trenutno
 stanje mreže in potezo igralca (v obliki indeksov). Predpostavimo, da je
 poteza legalna. Funkcija preveri, ali je igrana poteza privedla do konca igre
 in vrne posodobljeno stanje. 
[*----------------------------------------------------------------------------*)

let other_player player = failwith "DOPOLNI ME"

let place_token player grid (row_i, col_i) : state =
  (* [updated_grid] je mreža, kjer je na mestu določenim z [row_i] in [col_i]
     igralec [player] odigral potezo. Uporabite funkcijo [set_field]. *)
  let updated_grid = failwith "DOPOLNI ME" in
  (* Preverimo, ali smo dobili zmagovalca. *)
  match winner_of_grid updated_grid with
  | None when not (is_full_grid updated_grid) ->
      (* V tej potezi nismo dobili zmagovalca ampak igra se lahko nadaljuje.
      Vrnemo [OnTurn] s posodobljeno mrežo in nasprotnikom, ki je na potezi. *)
      failwith "DOPOLNI ME"
  | None (* grid is full *) ->
      (* Ni bilo zmagovalca, vendar so vsa polja polna. Vrnemo [GameOver] z
      novo mrežo in oznako za neodločen izid. *)
      failwith "DOPOLNI ME"
  | Some player ->
      (* Dobili smo zmagovalca, torej vrnemo [GameOver] z novo mrežo in
      zmagovalcem *)
      failwith "DOPOLNI ME"


(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
                                PRIKAZ
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Funkcije za prikaz stanja so povečini že napisane zaradi pomanjkanja časa.
 Dopolniti morate zgolj funkciji [show_player] in pa [show_state] saj sta
 odvisni od vaših tipov.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*]
 Funkcija [show_player] sprejme igralca in vrne niz, ki predstavlja njegov
 simbol.

 Funkcija [show_field] pretvori polje v niz, ki predstavlja vsebino polja. 

 Funkcija [show_row] vrstico pretvori v niz. Funkcija [String.concat] je
 ekvivalent Pythonovi metodi [.join].

 Funkcija [show_grid] mrežo pretvori v niz.

 Funkcija [show_state] sprejme trenutno stanje igre in uporabniku izpiše
 mrežo ter trenutno situacijo (kdo je na potezi, kdo je zmagal).
[*----------------------------------------------------------------------------*)

let show_player player = failwith "DOPOLNI ME"

let show_field = function
  | None -> " "
  | Some p -> show_player p

let show_row (a0, a1, a2) =
    "| "
  ^ String.concat " | " (List.map show_field [ a0; a1; a2 ])
  ^ " |\n"

let show_grid (row1, row2, row3) =
  let line = "+---+---+---+\n" in
    line
  ^ ( [row1; row2; row3] |> List.map show_row |> String.concat line )
  ^ line

let show_state = function
  | OnTurn {player; grid} ->
      "Na potezi je: " ^ show_player player ^ "\n" ^ show_grid grid ^ "\n"
  | GameOver {result; final_grid} ->
      let winner_message = "DOPOLNI ME" in
      winner_message ^ "\n" ^ show_grid final_grid ^ "\n"

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
                                VNOS POTEZ
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Funkcija [string_to_index] niz pretvori v tip [index] če niz ustreza celemu
 številu, ki opisuje indeks. Sicer vrne [None].

 Funkcija [choose] sprejme niz, ki igralcu pove katerega od indeksov izbira
 (stolpec ali vrstico). Nato prebere izbiro, in jo poskusi pretvoriti v indeks.
 Če vnos ni primeren, proces ponovi.
[*----------------------------------------------------------------------------*)
let string_to_index = failwith "DOPOLNI ME"

let rec choose kind =
  (* Sporočilo kaj želimo. *)
  print_string ("Prosimo izberite " ^ kind ^ " :");
  (* Preberemo izbiro. *)
  let choice = read_line () in
  match string_to_index choice with
  | Some result -> result (* Vrnemo veljaven rezultat. *)
  | None -> choose kind   (* Izbrana vrednost ni veljavna. *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
                                POTEK IGRE
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Funkcija [loop] simulira potek igre. 
 - Najprej uporabniku izpiše trenutno stanje ([print_string] in [show_state]).
 - Preveri stanje igre. 
   * Če je igre konec se zanka zaključi (vrne vrednost tipa [unit]). 
   * Sicer od igralca zahteva vnos za indeks vrstice ([choose "vrstico"] in
     nato vnos za indeks stolpca ([choose "stolpec"]). 

     Nato preveri, ali je poteza veljavna (polje mora biti prazno). 

     Če je poteza veljavna, jo izvede ([place_token]), sicer stanje pusti
     nespremenjeno. V obeh primerih ponovno požene zanko, da se igra nadaljuje.
[*----------------------------------------------------------------------------*)

let rec loop state = failwith "DOPOLNI ME"

(*----------------------------------------------------------------------------*]
 Funkcija [play_game] požene svežo igro.
[*----------------------------------------------------------------------------*)
let rec play_game () = failwith "DOPOLNI ME"
