(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Prioritetna vrsta je podatkovna struktura, ki hrani elemente glede na njihovo
 prioriteto. Elementi z višjo prioriteto so na voljo pred elementi z nižjo
 prioriteto. 
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type comparison = LT | EQ | GT

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Prioriteto definiramo s pomočjo modula primerjav. Želimo, da je implementacija
 tipa [t] vidna, saj potrebujemo tip za uporabo funktorjev. Ker pa ne vemo 
 katere tipe bomo primerjali, jo za ta trenutek pustimo abstraktno.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

module type Comparable = sig
    type t
    val compare : t -> t -> comparison
  end

(*----------------------------------------------------------------------------*]
 Napiši modul za primerjavo celih števil. Primerjaj nekaj celih števil.
[*----------------------------------------------------------------------------*)

module Cmp_Int = struct
  type t = int
  let compare x y = failwith "to do"
end

(*----------------------------------------------------------------------------*]
 Ko modulu predpišemo signaturo, je potrebno paziti katere tipe naredimo
 abstraktne. Če je tip abstrakten v signaturi, ga lahko kljub temu 'razkrijemo'
 s pomočjo [with type t = int], ko to potrebujemo. Preverite, da je to res
 potrebno!
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 Cmp_Int_prescribed.compare (-9000) 42;;
[*----------------------------------------------------------------------------*)

module Cmp_Int_prescribed = (Cmp_Int : Comparable with type t = int)

(*----------------------------------------------------------------------------*]
 Sedaj napiši modul, ki primerja nize. Pri pisanju primerjalne funkcije si
 pomagaj s funkcijo [compare] iz vgrajenega modula [Pervasives].

 Funkcija [Pervasives.compare s t] vrne -1 če je s < t, 0 če s = t in 1 za s > t
[*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*]
 Funktor je preslikava iz modula v modul. Sedaj definiraj funktor, ki sprejme
 modul, ki ustreza [Comparable] signaturi in vrne nov [Comparable] modul na
 istem osnovnem tipu, vendar z obrnjeno funkcijo primerjanja.

 Spodnja definicija uporabla oznake za tipe. Brez oznak bi bila zgolj
 [module Cmp_inv (Cmp)] vendar z oznako tipov povemo, da se tip končnega 
 modula ujema s tipom modula, ki ga podamo kot argument
[*----------------------------------------------------------------------------*)

(*
module Cmp_inv (Cmp : Comparable) : Comparable with type t = Cmp.t  = struct
  type t = ...
  let compare x y = ...
end
 *)

(*----------------------------------------------------------------------------*]
 Funktor uporabljamo podobno kot funkcije, le da v tem primeru potrebujemo
 oklepaje okrog argumentov
[*----------------------------------------------------------------------------*)

(*
module Cmp_Int_inv = Cmp_inv (Cmp_Int)
let _ = Cmp_Int.compare (-9000) 42;;
let _ = Cmp_Int_inv.compare (-9000) 42;;
 *)

(*----------------------------------------------------------------------------*]
 V primeru, ko imamo dva modula A in B, ki ju primerjamo preko [Comparable]
 signature, ju lahko združimo v produkt. Matematično gledano imamo dve možnosti
 za ureditev produkta. Za para (a1,b1) in (a2,b2) iz A × B lahko definiramo
 urejenost (a1,b1) < (a2,b2) čim a1 < a2 in b1 < b2, vendar s tem dobimo zgolj
 delno urejenost (v ℕ × ℕ ne moremo npr. primerjati (0,1) in (1,0)).
 Druga možnost pa je leksikografska ureditev, kjer je (a1,b1) < (a2,b2) 
 kadar a1 < a2, ali pa a1 = a2 in b1 < b2. Ta urejenost je primerna za 
 [Comparable] modul. 

 Definiraj funktor, ki sprejme dva modula A, B : Comparable in vrne modul
 [Cmp_lex : Comparable with type t = A.t * B.t]
[*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*]
 Sedaj napišemo signaturo prioritetne vrste. Imamo tip kopice, ki ga označimo s
 [h], tip elementov [el], prazno kopico in pa operacijo [push], ki na kopico
 doda element in operacijo [pop], ki iz kopice odvzame prvi element po
 prioriteti. Ker je kopica lahko prazna, nam [pop] vrne opcijski tip
[*----------------------------------------------------------------------------*)

(*
module type Priority_Queue = sig
    type h
    type el
    val empty : h
    val pop : h -> (h * el) option
    val push : h -> el -> h
  end
 *)


(*----------------------------------------------------------------------------*]
 Prioritetno vrsto bomo implementirali kot urejen seznam. Napiši funktor, ki
 sprejme [Comparable] modul in s pomočjo primerjave v tem modulu naredi
 prioritetno vrsto preko urejenega seznama. Seznam vsebuje elementa tipa [Cmp.t]
[*----------------------------------------------------------------------------*)

(*
module Sorted_List_Priority_Queue ... = struct

  ...

end
*)


(*----------------------------------------------------------------------------*]
 Za lažjo uporabo, napiši funktor [To_List], ki sprejme implementacijo kopice in
 vrne modul z operacijo [to_list] s katero elemente v kopici shrani v seznam. 
[*----------------------------------------------------------------------------*)

(* module To_List ... *)


(*----------------------------------------------------------------------------*]
 Sedaj lahko lažje testiramo implementacijo modulov.
 Module lahko uporabljamo lokalno, le da pri tem v [let ... in] dodatno dodamo 
 še besedo [module]. Imena globalnih modulov so praviloma daljša in bolj opisna,
 ko pa uporabljamo module lokalno si lahko privoščimo krajša imena
[*----------------------------------------------------------------------------*)
   
(*
let _ =
  let h = List.fold_left IntH.push IntH.empty [1; 0; 9; 2] in
  let module TL = To_List(IntH) in
  TL.to_list h

let _ =
  let module H = Sorted_List_Priority_Queue (Cmp_inv(Cmp_Int)) in
  let module L = To_List(H) in
  let h = List.fold_left H.push H.empty [1; 0; 9; 2] in
  L.to_list h
 *)
