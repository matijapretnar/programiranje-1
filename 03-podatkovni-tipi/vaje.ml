(*----------------------------------------------------------------------------*
 # Podatkovni tipi
[*----------------------------------------------------------------------------*)

let niz: string = "to je niz"
let pet_niz: int = 2 + 3
let pet: float = 2. +. 3. 

let resnica: bool = true
let seznam: int list = [1; 2] (* Tu imamo v seznamu samo en tip elementov. *)

(* okrajšave *)
type r3 = float * float * float
let tocka: r3 = (1., 2., 3.)

(* zapisni tip *)
type complex = {re: float; im: float}
let x: complex = {re = 1.; im = 0.}
let realni_del_x = x.re

(* nastevni tip *)
type dostava =
  | Osebno
  | PoPosti of string

let osebno: dostava = Osebno
let doma: dostava = PoPosti ("FMF")

(* algebrajski tip *)
type naravno_stevilo =
  | Nic
  | Naslednik of naravno_stevilo

let dva: naravno_stevilo = Naslednik (Naslednik Nic)

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

(* let dollar_to_euro (d: dollar) : euro = Euro (0.95 *. d) *)
(* let dollar_to_euro : dollar -> euro = fun (Dollar d) -> Euro (0.95 *. d) *)

let dollar_to_euro (Dollar d) = Euro (0.95 *. d)

let euro_to_dollar (Euro e) = Dollar (1.05 *. e)

let primer_valute_1 = dollar_to_euro (Dollar 0.5)
(* val primer_valute_1 : euro = Euro 0.4305 *)

(*----------------------------------------------------------------------------*
 Definirajte tip `currency` kot en naštevni tip s konstruktorji za jen, funt in
 švedsko krono. Nato napišite funkcijo `to_pound`, ki primerno pretvori valuto
 tipa `currency` v funte.

 Namig: V tip dodajte še švicarske franke in se navdušite nad dejstvom, da vas
 Ocaml sam opozori, da je potrebno popraviti funkcijo `to_pound`.
[*----------------------------------------------------------------------------*)

type currency =
  | Yen of float
  | Pound of float
  | Krona of float

let to_pound x = 
  match x with
  | Yen y -> Pound (0.007 *. y)
  | Pound p -> Pound p (* če napišem samo p, funkcija razume kot, da vrne float *)
  | Krona k -> Pound (0.08 *. k)

let primer_valute_2 = to_pound (Yen 100.)
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
 Definirajte tip `intbool_list` s konstruktorji za:

 - prazen seznam,
 - člen s celoštevilsko vrednostjo,
 - člen z logično vrednostjo.

 Nato napišite testni primer, ki bi predstavljal `[5; true; false; 7]`.
[*----------------------------------------------------------------------------*)

type intbool_list = 
  | Nil
  | Bool of bool * intbool_list
  | Int of int * intbool_list

let test : intbool_list = Int (5, Bool (true, Bool (false, Int (7, Nil))))

(*----------------------------------------------------------------------------*
 Funkcija `intbool_map f_int f_bool ib_list` preslika vrednosti `ib_list` v nov
 `intbool_list` seznam, kjer na elementih uporabi primerno od funkcij `f_int`
 oz. `f_bool`.
[*----------------------------------------------------------------------------*)

let rec intbool_map f_int f_bool ib_list = 
  match ib_list with 
    | Nil -> Nil
    | Int (x, ib_list') -> Int (f_int x, intbool_map f_int f_bool ib_list')
    | Bool (b, ib_list') -> Bool (f_bool b, intbool_map f_int f_bool ib_list')

(* Enako kot običajen rekurziven map:

let rec map f seznam =
  match seznam with
    | [] -> []
    | x :: xs -> (f x) :: (map f xs)

*)

(*----------------------------------------------------------------------------*
 Funkcija `intbool_reverse` obrne vrstni red elementov `intbool_list` seznama.
 Funkcija je repno rekurzivna.
[*----------------------------------------------------------------------------*)

let rec intbool_reverse ib_list = 
  let rec aux acc ib_list' = 
    match ib_list' with
      | Nil -> acc
      | Int (i, ibs) -> aux (Int (i, acc)) ibs (* kot aux (i :: acc) ibs *)
      | Bool (b, ibs) -> aux (Bool (b, acc)) ibs
  in
  aux Nil ib_list

(* brez repne rekurzije in brez obračanja: Int (i, ibs) -> Int (i, intbool_reverse ibs) *)

(*----------------------------------------------------------------------------*
 Funkcija `intbool_separate ib_list` loči vrednosti `ib_list` v par `list`
 seznamov, kjer prvi vsebuje vse celoštevilske vrednosti, drugi pa vse logične
 vrednosti. Funkcija je repno rekurzivna in ohranja vrstni red elementov.
[*----------------------------------------------------------------------------*)

(* intbool_separate : intbool_list -> (int list) * (bool list) *)
let rec intbool_separate ib_list = 
  let rec pomozna int_acc bool_acc ib_list' =
    match ib_list' with
      | Nil -> (List.rev int_acc, List.rev bool_acc)
      | Int (i, ibs) -> pomozna (i :: int_acc) bool_acc ibs (* lahko i:: int_acc, ker imamo navadne sezname, ki imajo samo en tip podatkov *) 
      | Bool (b, ibs) -> pomozna int_acc (b :: bool_acc) ibs
  in 
  pomozna [] [] ib_list

(*
let rec intbool_separate ib_list = 
  let rec pomozna int_acc bool_acc ib_list' =
    match ib_list' with
      | Nil -> (int_acc, bool_acc)
      | Int (i, ibs) -> pomozna (i :: int_acc) bool_acc ibs (* lahko i:: int_acc, ker imamo navadne sezname *) 
      | Bool (b, ibs) -> pomozna int_acc (b :: bool_acc) ibs
  in 
  pomozna [] [] (intbool_reverse ib_list)
*)

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

type magic =
 | Fire
 | Frost
 | Arcane

type specialisation =
 | Historian
 | Teacher
 | Researcher

(*----------------------------------------------------------------------------*
 Vsak od čarodejev začne kot začetnik, nato na neki točki postane študent, na
 koncu pa SE lahko tudi zaposli. Definirajte tip `status`, ki določa ali je
 čarodej:

 - začetnik `Newbie`,
 - študent `Student` (in kateri vrsti magije pripada in koliko časa študira),
 - zaposlen `Employed` (in vrsto magije in specializacijo).

 Nato definirajte zapisni tip `wizard` s poljem za ime in poljem za trenuten
 status ter dodajte primer `professor`, ki je zaposlen učitelj magije ognja, in
 `jaina`, ki je četrto leto študentka magije ledu.
[*----------------------------------------------------------------------------*)

type status =
 | Newbie
 | Student of magic * int
 | Employed of magic * specialisation

type wizard = {
 ime : string;
 status : status
}

let professor = { ime = "Profesor"; status = Employed (Fire, Teacher) }

let jaina = { ime = "Jaina"; status = Student (Frost, 4) }

(*----------------------------------------------------------------------------*
 Želimo prešteti koliko uporabnikov posamezne od vrst magije imamo na akademiji.
 Definirajte zapisni tip `magic_counter`, ki v posameznem polju hrani število
 uporabnikov magije. Nato definirajte funkcijo `update counter magic`, ki vrne
 nov števec s posodobljenim poljem glede na vrednost `magic`.
[*----------------------------------------------------------------------------*)

type magic_counter = {fire: int; frost: int; arcane: int}

(*
let update {fire = fi; frost = fr; arcane = a} magic = 
  match magic with
  | Fire -> {fire = fi + 1; frost = fr; arcane = ar}
  | Frost -> {fire = fi; frost = fr + 1; arcane = ar}
  | Arcane -> {fire = fi; frost = fr; arcane = ar + 1}
*)

(*
let update counter magic = 
  let {fire = fi; frost = fr; arcane = ar} = counter in
  match magic with
  | Fire -> {fire = fi + 1; frost = fr; arcane = ar}
  | Frost -> {fire = fi; frost = fr + 1; arcane = ar}
  | Arcane -> {fire = fi; frost = fr; arcane = ar + 1}
*)

let update counter magic = 
  match magic with
  | Fire -> { counter with fire = counter.fire + 1 }                 (* z with ne razbijemo celega objekta, če ga ne rabimo *)
  | Frost -> { counter with frost = counter.frost + 1 }
  | Arcane -> { counter with arcane = counter.arcane + 1 }  

(* Potrebujem za spodnjo nalogo: *)
let update_opt counter magic_opt =
  match magic_opt with
  | None -> counter
  | Some Fire -> { counter with fire = counter.fire + 1 }
  | Some Frost -> { counter with frost = counter.frost + 1 }
  | Some Arcane -> { counter with arcane = counter.arcane + 1 }

let primer_carovniki_1 = update {fire = 1; frost = 1; arcane = 1} Arcane
(* val primer_carovniki_1 : magic_counter = {fire = 1; frost = 1; arcane = 2} *)

(*----------------------------------------------------------------------------*
 Funkcija `count_magic` sprejme seznam čarodejev in vrne števec uporabnikov
 različnih vrst magij.
[*----------------------------------------------------------------------------*)

let magic_of_wizard w =
  match w.status with
  | Newbie -> None
  | Student (m, _) -> Some m
  | Employed (m, _) -> Some m

let count_magic wizards = 
  let rec aux counter wizards =
    match wizards with
    | [] -> counter
    | w :: wizards' -> aux (update_opt counter (magic_of_wizard w)) wizards'
  in
  aux { fire = 0; frost = 0; arcane = 0 } wizards

(* Profesorjeva rešitev:

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
*)

let primer_carovniki_2 = count_magic [professor; professor; professor]
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

(* Bolj kot ne je to profesorjeva koda *)
let find_candidate magic specialisation wizard_list = 
  let rec find_candidate' wizard_list =
    match wizard_list with
    | [] -> None
    | { ime; status = Student (m, y) } :: wizards' ->
        if magic = m then 
          if y >=
            match specialisation with
            | Historian -> 3
            | Teacher -> 5
            | Researcher -> 4
          then Some ime
          else find_candidate' wizards'
        else find_candidate' wizards'
    | _ :: wizards' -> find_candidate' wizards'
  in
  find_candidate' wizard_list

let primer_carovniki_3 =
  find_candidate Frost Researcher [professor; jaina]
(* val primer_carovniki_3 : string option = Some "Jaina" *)
