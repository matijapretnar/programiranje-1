(*============================================================================*]
  Pri tej nalogi bomo za slovar uporabili kar enostavno implementacijo z asociativnim seznamom, ki smo jo spoznali na predavanjih 
  S spodaj definiranimi funkcijami si lahko pomagate pri vseh podnalogah.
[*============================================================================*)


type ('a, 'b) slovar = ('a * 'b) list

let prazen_slovar : ('a, 'b) slovar = []

let velikost (m : ('a, 'b) slovar) = List.length m

let vsebuje (x : 'a) (m : ('a, 'b) slovar) = List.mem_assoc x m

(* Vrne vrednost, ki pripada ključu ali None *)
let najdi x (m : ('a, 'b) slovar) = List.assoc_opt x m

(* Doda vrednost v slovar in povozi prejšnjo, če obstaja *)
let dodaj (k, v) (m : ('a, 'b) slovar) = (k, v) :: List.remove_assoc k m


(*============================================================================*]
  Matematične izraze predstavimo kot binarno drevo, so vozlišča operatorji, 
  listi pa števila ali spremenljivke tipa `string`.
  Izraz v drevo pretvorimo tako, da pri operatorju levi podizraz vzamemo za levo 
  poddrevo, desni podizraz za desno, v vozlišče pa zapišemo operator.
[*============================================================================*)


type operator = Plus | Minus | Krat | Deljeno

type 'a izraz =
  | Spremenljivka of string
  | Konstanta of 'a
  | Vozlisce of ('a izraz * operator * 'a izraz)

(* (x - 3)- (y * (z / x))  *)
let primer =
  Vozlisce
    ( Vozlisce (Spremenljivka "x", Minus, Konstanta 3),
      Minus,
      Vozlisce
        ( Spremenljivka "y",
          Krat,
          Vozlisce (Spremenljivka "z", Deljeno, Spremenljivka "x") ) )

(* a *)
(*----------------------------------------------------------------------------*]
  Definirajte `izraz_1` in `izraz_2` , ki predstavljata izraza: 
  `1 + (2 + (3 / ( 3 * x)))` in `((((c * c) - (b * b)) - (a * a)) + 27)`.
[*----------------------------------------------------------------------------*)

let izraz_1 = ()

let izraz_2 = ()

(* b *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo `prestej : izraz -> int`, ki vrne število vseh "različnih" 
  spremenljivk v izrazu.

    prestej : izraz -> int

[*----------------------------------------------------------------------------*)

let prestej = ()

(* c *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo `izlusci : 'a izraz -> (string * int) slovar`, ki sprejme 
  izraz in vrne slovar, ki pove kolikokrat se posamezna spremenljivka pojavi v 
  izrazu. Vrstni red v slovarju ni pomemben.

    izlusci : 'a izraz -> (string * int) slovar

  Ocenite časovno zahtevnost funkcije v odvisnosti od velikost izraza in števila 
  različnih spremenljivk. Ali se časovna zahtevnost spremeni, če zahtevamo, da 
  so spremenljivke samo male črke angleške abecede? Kako se časovna zahtevnost 
  spremeni, če bi za slovar uporabili uravnoteženo iskalno drevo?

[*----------------------------------------------------------------------------*)

let izlusci = ()

(* d *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo `izracunaj : (string * int) slovar -> int izraz -> option int`, 
  ki sprejme izraz in slovar ter poskuša izračunati vrednost izraza. Če to ni 
  mogoče (deljenje z 0 ali manjkajoča definicija spremenljivke), naj bo rezultat 
  `None`. 

    izracunaj : (string * int) slovar -> int izraz -> option int

  Ocenite časovno zahtevnost funkcije v odvisnosti od velikost izraza in števila 
  različnih spremenljivk. Ali se časovna zahtevnost spremeni, če zahtevamo, da 
  so spremenljivke samo male črke angleške abecede? Kako se časovna zahtevnost 
  spremeni, če bi za slovar uporabili uravnoteženo iskalno drevo?

  Kot primer `izracunaj [("x",3); ("y", 4); ("z",5)] primer` vrne `Some (-4)`.

[*----------------------------------------------------------------------------*)

let izracunaj = ()
