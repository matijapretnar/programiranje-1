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
  let compare x y = if x < y then LT else if x = y then EQ else GT
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

module Cmp_String = struct
  type t = string
  let compare x y =
    match Pervasives.compare x y with
    | v when v < 0 -> LT
    | 0 -> EQ
    | _ -> GT
end

(*----------------------------------------------------------------------------*]
 Funktor je preslikava iz modula v modul. Sedaj definiraj funktor, ki sprejme
 modul, ki ustreza [Comparable] signaturi in vrne nov [Comparable] modul na
 istem osnovnem tipu, vendar z obrnjeno funkcijo primerjanja.

 Spodnja definicija uporabla oznake za tipe. Brez oznak bi bila zgolj
 [module Cmp_inv (Cmp)] vendar z oznako tipov povemo, da se tip končnega
 modula ujema s tipom modula, ki ga podamo kot argument
[*----------------------------------------------------------------------------*)

module Cmp_inv (Cmp : Comparable) : Comparable with type t = Cmp.t  = struct
  type t = Cmp.t
  let compare x y = match Cmp.compare x y with
    | LT -> GT
    | EQ -> EQ
    | GT -> LT
end

(*----------------------------------------------------------------------------*]
 Funktor uporabljamo podobno kot funkcije, le da v tem primeru potrebujemo
 oklepaje okrog argumentov
[*----------------------------------------------------------------------------*)

module Cmp_Int_inv = Cmp_inv (Cmp_Int)
let _ = Cmp_Int.compare (-9000) 42;;
let _ = Cmp_Int_inv.compare (-9000) 42;;

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

module Cmp_lex (A : Comparable) (B : Comparable)
  : Comparable with type t = A.t * B.t
  = struct
    type t = A.t * B.t
    let compare (a1, b1) (a2, b2) =
      match A.compare a1 a2 with
      | (LT | GT) as x -> x
      | EQ -> B.compare b1 b2
end


(*----------------------------------------------------------------------------*]
 Sedaj napišemo signaturo prioritetne vrste. Imamo tip kopice, ki ga označimo s
 [h], tip elementov [el], prazno kopico in pa operacijo [push], ki na kopico
 doda element in operacijo [pop], ki iz kopice odvzame prvi element po
 prioriteti. Ker je kopica lahko prazna, nam [pop] vrne opcijski tip
[*----------------------------------------------------------------------------*)

module type Priority_Queue = sig
  type h
  type el
  val empty : h
  val pop : h -> (h * el) option
  val push : h -> el -> h
end

(*----------------------------------------------------------------------------*]
 Prioritetno vrsto bomo implementirali kot urejen seznam. Napiši funktor, ki
 sprejme [Comparable] modul in s pomočjo primerjave v tem modulu naredi
 prioritetno vrsto preko urejenega seznama. Seznam vsebuje elementa tipa [Cmp.t]
[*----------------------------------------------------------------------------*)

module Sorted_List_Priority_Queue (Cmp : Comparable) : Priority_Queue with type el = Cmp.t = struct

  type h = Cmp.t list
  type el = Cmp.t

  let empty = []

  let pop = function
    | [] -> None
    | x :: xs -> Some (xs, x)

  let rec insert x ord =
    match ord with
    | [] -> [x]
    | y :: rest ->
       (match Cmp.compare x y with
        | LT -> x :: ord
        | EQ -> ord
        | GT -> y :: (insert x rest))

  let push h x = insert x h

end

(*----------------------------------------------------------------------------*]
 Apply your functor to build a priority queue of integers, and a priority queue
 of strings. Write some examples using push and pop!
[*----------------------------------------------------------------------------*)

module IntH = Sorted_List_Priority_Queue (Cmp_Int)
module StringH = Sorted_List_Priority_Queue (Cmp_String)

(*----------------------------------------------------------------------------*]
 Za lažjo uporabo, napiši funktor [To_List], ki sprejme implementacijo kopice in
 vrne modul z operacijo [to_list] s katero elemente v kopici shrani v seznam.
[*----------------------------------------------------------------------------*)

module To_List (H : Priority_Queue) = struct
  let rec to_list h =
    match H.pop h with
    | None -> []
    | Some (h, x) -> x :: (to_list h)
end

(*----------------------------------------------------------------------------*]
 Sedaj lahko lažje testiramo implementacijo modulov.
 Module lahko uporabljamo lokalno, le da pri tem v [let ... in] dodatno dodamo
 še besedo [module]. Imena globalnih modulov so praviloma daljša in bolj opisna,
 ko pa uporabljamo module lokalno si lahko privoščimo krajša imena
[*----------------------------------------------------------------------------*)

let _ =
  let h = List.fold_left IntH.push IntH.empty [1; 0; 9; 2] in
  let module TL = To_List(IntH) in
  TL.to_list h

let _ =
  let module H = Sorted_List_Priority_Queue (Cmp_inv(Cmp_Int)) in
  let module L = To_List(H) in
  let h = List.fold_left H.push H.empty [1; 0; 9; 2] in
  L.to_list h
