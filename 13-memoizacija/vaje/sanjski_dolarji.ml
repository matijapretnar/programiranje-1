(* Vaja povečini povzeta iz zapiskov predavanj Jeffa Ericksona. *)

(* V prejšnjem življenju ste delali kot blagajničar v pozabljeni antarktični
   koliniji Nadira, kjer ste večino dneva porabili za vračanje drobiža kupcem.
   Ker je papir redek na antarktiki so blagajničarji morali po zakonu vračati
   najmanjše možno število bankovcev kot drobiž. Po zaslugi numerološke
   navdušenosti enega od ustanoviteljev Nadire je denar, imenovan sanjski
   dolarji, na voljo v naslednjih bankovcih: $1, $4, $7, $13, $28, $52, $91, $365.
*)

let denominations = [1; 4; 7; 13; 28; 52; 91; 365]


(* 0.i) Matematično formulirajte problem. *)

(*
   Denimo, da moramo vrniti število n, ...
*)


(* 0.ii) Opišite rekurzijo za problem. *)

(*
   Denimo, da moramo vrniti število n, ...
*)


(* 1. Požrešni algoritem zaporedno jemlje največji možni bankovec, ki ga lahko
   vrne brez da preseže vrednost. Naprimer za $122 najprej vrne $91 nato $28
   in končno še tri $1 bankovce.

   Poskusite poiskati primer, kjer to ni optimalno.

   Namig: Iskanje primera ni tako preprosto. Del lahko izpustite dokler ne
   implementirate natančne rešitve, ki jo nato primerjate z požrešno.
*)

let rec bills_greedy n = failwith "todo"

(* 2.i) Opišite in analizirajte rekurzivni algoritem, ki za dani k izračuna
   najmanjši možni nabor bankovcev, potrebnih za vračanje k sanjskih dolarjev.
   (Na tej točki hitrost algoritma ni pomembna.)
*)

let rec bills_rec n = failwith "todo"


(* 2.ii) Narišite drevo rekurzivnih klicov za n=5 in ugotovite kateri
   podproblemi se ponavljajo. Ali lahko poiščete vrstni red, ki zgradi
   rešitev od spodaj navzgor? *)



(* 2.iii) Napišite algoritem s pomočjo dinamičnega programiranja, ki za dano
   število k izračuna najmanjši možni nabor bankovcev, potrebnih za vračanje k
   sanjskih dolarjev.
   (Tokrat je hitrost pomembna.)
*)

let bills_iter n = failwith "todo"
