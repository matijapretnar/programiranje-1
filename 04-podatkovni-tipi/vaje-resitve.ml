(*----------------------------------------------------------------------------*
   # Podatkovni tipi
  [*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
   ## Valute

   Pri modeliranju denarja ponavadi uporabljamo racionalna števila. Problemi se
   pojavijo, ko uvedemo različne valute. Oglejmo si dva pristopa k izboljšavi
   varnosti pri uporabi valut.
  [*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
   Definirajte tipa `euro` in `dollar`, kjer ima vsak od tipov zgolj en
   konstruktor, ki sprejme racionalno število. Nato napišite funkciji
   `euro_to_dollar` in `dollar_to_euro`, ki primerno pretvarjata valuti (točne
   vrednosti pridobite na internetu ali pa si jih izmislite).

   Namig: Občudujte informativnost tipov funkcij.
  [*----------------------------------------------------------------------------*)

type euro = Euro of float
type dollar = Dollar of float

let dollar_to_euro (Dollar d) = Euro (d *. 0.96)
let euro_to_dollar (Euro e) = Dollar (e *. 1.04)

(* let primer_valute_1 = dollar_to_euro (Dollar 0.5) *)
(* val primer_valute_1 : euro = Euro 0.4305 *)

(*----------------------------------------------------------------------------*
   Definirajte tip `currency` kot en vsotni tip z konstruktorji za jen, funt in
   švedsko krono. Nato napišite funkcijo `to_pound`, ki primerno pretvori valuto
   tipa `currency` v funte.

   Namig: V tip dodajte še švicarske franke in se navdušite nad dejstvom, da vas
   Ocaml sam opozori, da je potrebno popraviti funkcijo `to_pound`.
  [*----------------------------------------------------------------------------*)

type currency = Yen of float | Pound of float | Krona of float

let to_pound = function
  | Yen y -> Pound (y *. 0.007)
  | Pound p -> Pound p
  | Krona k -> Pound (k *. 0.08)

(* let primer_valute_2 = to_pound (Yen 100.) *)
(* val primer_valute_2 : currency = Pound 0.700000000000000067 *)

(*----------------------------------------------------------------------------*
   ## Mešani seznami

   Želimo uporabljati sezname, ki hranijo tako cela števila kot tudi logične
   vrednosti. To bi lahko rešili tako da uvedemo nov tip, ki predstavlja celo
   število ali logično vrednost, v nadaljevanju pa bomo raje konstruirali nov tip
   seznamov.

   Spomnimo se, da lahko tip `list` predstavimo s konstruktorjem za prazen seznam
   `Nil`(oz. `[]` v Ocamlu) in pa konstruktorjem za člen `Cons(x, xs)` (oz. `x ::
   xs` v Ocamlu).
  [*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
   Definirajte tip `intbool_list` z konstruktorji za:

   - prazen seznam,
   - člen s celoštevilsko vrednostjo,
   - člen z logično vrednostjo.

   Nato napišite testni primer, ki bi predstavljal `[5; true; false; 7]`.
  [*----------------------------------------------------------------------------*)

type intbool_list =
  | Nil
  | Int of int * intbool_list
  | Bool of bool * intbool_list

let test = Int (5, Bool (true, Bool (false, Int (7, Nil))))

(*----------------------------------------------------------------------------*
   Funkcija `intbool_length ib_list` vrne dolžino `ib_list`.
  [*----------------------------------------------------------------------------*)

let rec intbool_length ib_list =
  match ib_list with
  | Nil -> 0
  | Int (_, ib_list') -> 1 + intbool_length ib_list'
  | Bool (_, ib_list') -> 1 + intbool_length ib_list'

(*----------------------------------------------------------------------------*
   Funkcija `intbool_map f_int f_bool ib_list` preslika vrednosti `ib_list` v nov
   `intbool_list` seznam, kjer na elementih uporabi primerno od funkcij `f_int`
   oz. `f_bool`.
  [*----------------------------------------------------------------------------*)

let rec intbool_map f_int f_bool ib_list =
  match ib_list with
  | Nil -> Nil
  | Int (i, ib_list') -> Int (f_int i, intbool_map f_int f_bool ib_list')
  | Bool (b, ib_list') -> Bool (f_bool b, intbool_map f_int f_bool ib_list')

(*----------------------------------------------------------------------------*
   Funkcija `intbool_reverse` obrne vrstni red elementov `intbool_list` seznama.
   Funkcija je repno rekurzivna.
  [*----------------------------------------------------------------------------*)

let rec intbool_reverse l =
  let rec reverse' acc = function
    | Nil -> acc
    | Int (i, l') -> reverse' (Int (i, acc)) l'
    | Bool (b, l') -> reverse' (Bool (b, acc)) l'
  in
  reverse' Nil l

(*----------------------------------------------------------------------------*
   Funkcija `intbool_separate ib_list` loči vrednosti `ib_list` v par `list`
   seznamov, kjer prvi vsebuje vse celoštevilske vrednosti, drugi pa vse logične
   vrednosti. Funkcija je repno rekurzivna in ohranja vrstni red elementov.
  [*----------------------------------------------------------------------------*)

let rec intbool_separate ib_list =
  match ib_list with
  | Nil -> ([], [])
  | Int (i, ib_list') ->
      let ints, bools = intbool_separate ib_list' in
      (i :: ints, bools)
  | Bool (b, ib_list') ->
      let ints, bools = intbool_separate ib_list' in
      (ints, b :: bools)

(*----------------------------------------------------------------------------*
   ## Čarodeji

   Določeni ste bili za vzdrževalca baze podatkov za svetovno priznano čarodejsko
   akademijo "Effemef". Vaša naloga je konstruirati sistem, ki bo omogočil
   pregledno hranjenje podatkov.
  [*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
   Čarodeje razvrščamo glede na vrsto magije, ki se ji posvečajo. Definirajte tip
   `magic`, ki loči med magijo ognja, magijo ledu in magijo arkane oz. fire, frost
   in arcane. Ko se čarodej zaposli na akademiji, se usmeri v zgodovino,
   poučevanje ali raziskovanje oz. historian, teacher in researcher. Definirajte
   tip `specialisation`, ki loči med temi zaposlitvami.
  [*----------------------------------------------------------------------------*)

type magic = Fire | Frost | Arcane
type specialisation = Historian | Teacher | Researcher

(*----------------------------------------------------------------------------*
   Vsak od čarodejev začne kot začetnik, nato na neki točki postane študent, na
   koncu pa SE lahko tudi zaposli. Definirajte tip `status`, ki določa ali je
   čarodej:

   - začetnik `Newbie`,
   - študent `Student` (in kateri vrsti magije pripada in koliko časa študira),
   - zaposlen `Employed` (in vrsto magije in specializacijo).

   Nato definirajte zapisni tip `wizard` z poljem za ime in poljem za trenuten
   status ter dodajte primer `professor`, ki je zaposlen učitelj magije ognja, in
   `jaina`, ki je četrto leto študentka magije ledu.
  [*----------------------------------------------------------------------------*)

type status =
  | Newbie
  | Student of magic * int
  | Employed of magic * specialisation

type wizard = { name : string; status : status }

let professor = { name = "Professor"; status = Employed (Fire, Teacher) }
let jaina = { name = "Jaina"; status = Student (Frost, 4) }

(*----------------------------------------------------------------------------*
   Želimo prešteti koliko uporabnikov posamezne od vrst magije imamo na akademiji.
   Definirajte zapisni tip `magic_counter`, ki v posameznem polju hrani število
   uporabnikov magije. Nato definirajte funkcijo `update counter magic`, ki vrne
   nov števec s posodobljenim poljem glede na vrednost `magic`.
  [*----------------------------------------------------------------------------*)

type magic_counter = { fire : int; frost : int; arcane : int }

let update counter magic_type =
  match magic_type with
  | Fire -> { counter with fire = counter.fire + 1 }
  | Frost -> { counter with frost = counter.frost + 1 }
  | Arcane -> { counter with arcane = counter.arcane + 1 }

(* let primer_carovniki_1 = update {fire = 1; frost = 1; arcane = 1} Arcane *)
(* val primer_carovniki_1 : magic_counter = {fire = 1; frost = 1; arcane = 2} *)

(*----------------------------------------------------------------------------*
   Funkcija `count_magic` sprejme seznam čarodejev in vrne števec uporabnikov
   različnih vrst magij.
  [*----------------------------------------------------------------------------*)

let count_magic wizards =
  let rec count_magic' wizards counter =
    match wizards with
    | [] -> counter
    | { status = Employed (magic, _) } :: wizards' ->
        count_magic' wizards' (update counter magic)
    | { status = Student (magic, _) } :: wizards' ->
        count_magic' wizards' (update counter magic)
    | _ :: wizards' -> count_magic' wizards' counter
  in
  count_magic' wizards { fire = 0; frost = 0; arcane = 0 }

(* let primer_carovniki_2 = count_magic [professor; professor; professor] *)
(* val primer_carovniki_2 : magic_counter = {fire = 3; frost = 0; arcane = 0} *)

(*----------------------------------------------------------------------------*
   Želimo poiskati primernega kandidata za delovni razpis. Študent lahko postane
   zgodovinar po vsaj treh letih študija, raziskovalec po vsaj štirih letih
   študija in učitelj po vsaj petih letih študija. Funkcija `find_candidate magic
   specialisation wizard_list` poišče prvega primernega kandidata na seznamu
   čarodejev in vrne njegovo ime, čim ustreza zahtevam za `specialisation` in
   študira vrsto `magic`. V primeru, da ni primernega kandidata, funkcija vrne
   `None`.
  [*----------------------------------------------------------------------------*)

let find_candidate magic specialisation wizards =
  let rec find_candidate' wizards =
    match wizards with
    | [] -> None
    | { name; status = Student (magic', years) } :: wizards' ->
        if magic = magic' then
          if
            years
            >=
            match specialisation with
            | Historian -> 3
            | Teacher -> 5
            | Researcher -> 4
          then Some name
          else find_candidate' wizards'
        else find_candidate' wizards'
    | _ :: wizards' -> find_candidate' wizards'
  in
  find_candidate' wizards

(* let primer_carovniki_3 =
   find_candidate Frost Researcher [professor; jaina] *)
(* val primer_carovniki_3 : string option = Some "Jaina" *)
