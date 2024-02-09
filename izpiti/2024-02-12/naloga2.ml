type element = Variable of char | Constant of char

(* Pravilo pove, v kaj se preslika posamezen element *)
type result = element list

(* Seznam pravil podamo s pari, ki spremenljivkam priredijo sliko  *)
(* Za vse preslikave bomo predpostavili, da so enolične *)
type rules = (char * result) list
type l_tree = Node of element * l_tree list option

let example0 =
  Node
    ( Variable 'A',
      Some
        [
          Node
            ( Variable 'A',
              Some [ Node (Variable 'A', None); Node (Variable 'B', None) ] );
          Node (Variable 'B', Some [ Node (Variable 'A', None) ]);
        ] )

let rules0 = [ ('A', [ Variable 'A'; Variable 'B' ]); ('B', [ Variable 'A' ]) ]

let example1 =
  Node
    ( Variable '0',
      Some
        [
          Node (Variable '1', Some []);
          Node (Constant '[', Some []);
          Node (Variable '0', Some []);
          Node (Constant ']', Some []);
          Node (Variable '0', Some []);
        ] )

let rules1 =
  [
    ('1', [ Variable '1'; Variable '1' ]);
    ( '0',
      [ Variable '1'; Constant '['; Variable '0'; Constant ']'; Constant '0' ]
    );
  ]

(* 2. a) *)

(*
Element drevesa je `veljaven`, če nima praznega seznama poddreves (če torej nima za poddrevo `Some []`).
Element drevesa je `list`, če ima za poddrevo `None`.
Drevo je `veljavno`, če so vsi njegovi elementi veljavni, so globine vseh poddreves enake in so vsa poddrevesa veljavna.
Napišite funkcijo `veljavno : l_tree -> bool`, ki preveri, ali je dano drevo veljavno.
Oba podana primera dreves sta veljavna.
*)

(* 2. b) *)

(*
Napišite funkcijo `preslikaj : rules -> element -> result list`, ki za podan seznam pravil in element vrne rezultat, v katerega se element preslika.
Lahko predpostavite, da je element vedno možno preslikati.
Če je element konstanta, naj funkcija vrne seznam, ki vsebuje samo ta element, ne glede na seznam pravil.
*)

(* 2. c) *)

(*
Napišite funkcijo `zadnja_vrstica : l_tree -> element list`, ki vrne seznam elementov zadnje vrstice drevesa.
Lahko predpostavite, da je drevo vedno veljavno.
Elementi v rezultatu naj bodo v istem vrstnem redu, kot se pojavijo v drevesu, če bi ga brali od leve proti desni.
Za vse točke naj bo funkcija repno rekurzivna.
*)

(* 2. d) *)
(*
Napišite funkcijo `preveri_pravila : rules -> bool`, ki za dani seznam pravil preveri, ali so:
1. Enolična, torej ali se vsaka spremenljivka pojavi največ enkrat na levi strani pravila.
2. Ponovljiva, torej, ali je vsaka spremenljivka, ki se pojavi na desni strani pravila, tudi na levi.  
*)

(* 2. e *)

(*
Napišite funkcijo `preslikaj_drevo : rules -> l_tree -> l_tree`, ki za dano drevo in seznam pravil vrne preslikano drevo - drevo, pri katerem smo na spodnjem nivoju enkrat uporabili seznam pravil.
Preslikano drevo je takšno, da so otroci listov izvirnega drevesa zamenjani listi, kjer so vrednosti novih listov take, kot jih določajo pravila (enako, kot pri funkciji `preslikaj`).
Lahko predpostavite, da je drevo vedno veljavno.
Drevo iz primera `example0` smo dobili tako, da smo dvakrat preslika drevo `Node (Variable 'A', None)` z uporabo `rules0`.
Drevo iz primera `example1` smo dobili tako, da smo enkrat preslikali drevo `Node (Variable '0', None)` enkrat preslikali z uporabo `rules1`.
*)
