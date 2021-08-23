(*============================================================================*]
  Pri tej nalogi bomo za slovar uporabili kar enostavno implementacijo z 
  asociativnim seznamom, ki smo jo spoznali na predavanjih.
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
  Matematične izraze predstavimo z dvojiškimi drevesi, v katerih vozlišča predstavljajo 
  aritmetične operacije, listi pa števila ali spremenljivke, predstavljene z nizi.
  Izraz v drevo pretvorimo tako, da pri operaciji levi podizraz vzamemo za levo 
  poddrevo, desni podizraz za desno, v vozlišče pa zapišemo operator.
[*============================================================================*)

type operator = Plus | Minus | Krat | Deljeno

type 'a izraz =
  | Spremenljivka of string
  | Konstanta of 'a
  | Operacija of ('a izraz * operator * 'a izraz)

(* (x - 3)- (y * (z / x))  *)
let primer =
  Operacija
    ( Operacija (Spremenljivka "x", Minus, Konstanta 3),
      Minus,
      Operacija
        ( Spremenljivka "y",
          Krat,
          Operacija (Spremenljivka "z", Deljeno, Spremenljivka "x") ) )

(* a *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo `prestej : izraz -> int`, ki vrne število vseh "različnih" 
  spremenljivk v izrazu.
[*----------------------------------------------------------------------------*)

let prestej izraz =
  let rec prestej_aux vars = function
    | Spremenljivka x -> dodaj (x, true) vars
    | Konstanta _ -> vars
    | Operacija (l, _, r) ->
        let vars = prestej_aux vars l in
        prestej_aux vars r
  in
  List.length (prestej_aux prazen_slovar izraz)


(* b *)
(*----------------------------------------------------------------------------*]
Napišite funkcijo `izlusci : 'a izraz -> (string * int) slovar`, ki sprejme izraz 
in vrne slovar, ki pove, kolikokrat se posamezna spremenljivka pojavi v izrazu. 
Vrstni red v slovarju ni pomemben.

[*----------------------------------------------------------------------------*)

let povecaj x m =
  match najdi x m with Some v -> dodaj (x, v + 1) m | None -> dodaj (x, 1) m

let izlusci izraz =
  let rec izlusci_aux vars = function
    | Spremenljivka x -> povecaj x vars
    | Konstanta _ -> vars
    | Operacija (l, _, r) ->
        let vars = izlusci_aux vars l in
        izlusci_aux vars r
  in
  izlusci_aux prazen_slovar izraz

(* c *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo `izracunaj : (string * int) slovar -> int izraz -> option int`, 
  ki sprejme izraz in slovar vrednosti spremenljivk ter poskuša izračunati vrednost 
  izraza. Če to ni mogoče (deljenje z 0 ali manjkajoča definicija spremenljivke), 
  naj bo rezultat `None`. 

    # izracunaj [("x",3); ("y", 4); ("z",5)] primer;;
    - : int option = Some (-4)

[*----------------------------------------------------------------------------*)

let izvedi l r = function
  | Plus -> Some (l + r)
  | Krat -> Some (l * r)
  | Minus -> Some (l - r)
  | Deljeno -> if r = 0 then None else Some (l / r)

let rec izracunaj slovar = function
  | Spremenljivka x -> najdi x slovar
  | Konstanta x -> Some x
  | Operacija (l, op, r) -> (
      match izracunaj slovar l with
      | Some y -> (
          match izracunaj slovar r with Some z -> izvedi y z op | None -> None)
      | None -> None)


(* c *)
(*----------------------------------------------------------------------------*]
  Ocenite časovno zahtevnost funkcije `izracunaj` v odvisnosti od velikosti 
  izraza `n` (torej števila vseh vozlišč in listov v drevesu) ter števila različnih 
  spremenljivk `m`.
  Kako se časovna zahtevnost spremeni, če bi za slovar uporabili uravnoteženo iskalno drevo?

[*----------------------------------------------------------------------------*)

(* Očitno velja m <= n. Pregledati moramo vsa vozlišča: O(n). 
   Vsako spremenljivko v drevesu moramo dodati v slovar. 
   Dodajanje stane O(m) za podano implementacijo, O(log m) za uravnoteženo iskalno drevo in 
   O(1) za slovar z zgoščevalno funkcijo (hashing). 
   Skupaj O(n + n * m) ali O(n + n * log m) ali O(n).
*)