### Datoteke `.ml` so moduli, `.mli` pa signature

`prva.ml`
.source[```
let pomozni_racun = 3 + 3
let odgovor = pomozni_racun * succ pomozni_racun
```]

`prva.mli`
.source[```
val odgovor : int
```]

`druga.ml`
.source[```
let boljsi_odgovor = succ Prva.odgovor in
let vzklik = string_of_int boljsi_odgovor ^ "!!!" in
print_endline vzklik
```]

---

class: center, middle, question

# razbitje programov<br>na datoteke

---

class: question

### Kaj je **narobe** s to signaturo?

.bad-example.left[```
module type Mnozica = sig
  type 'a t
  val prazna : 'a t
  val dodaj : 'a -> 'a t -> 'a t
  val vsebuje : 'a -> 'a t -> bool
  val velikost : 'a t -> int
end
```]

--

### Ali znamo narediti implementacijo množice,<br>ki bi delala pri **vsakem** tipu `'a`?

---

### **Strožja signatura** za množice naredi abstrakten tudi tip elementov

```
module type Mnozica =
sig
    type t
    type elt

    val prazna : t
    val dodaj : elt -> t -> t
    val vsebuje : elt -> t -> bool
    val velikost : t -> int
end
```

---

### Iskalna drevesa lahko delamo **samo za** tipe,<br>ki podpirajo **urejenost**

.source[```
type primerjava = Manjsi | Enak | Vecji

module type Urejenost =
sig
    type t
    val primerjaj : t -> t -> primerjava
end
```]

---

class: question

### Urejenost na celih številih

.source[```
module CelaStevila : Urejenost = ...
```]

--

.bad-example[```
let oceni_kakovost x =
  CelaStevila.primerjaj x 42
```]

--

.good-example[```
module CelaStevila :
  Urejenost with type t = int
  = ...
```]

---

### **Funktor** je modul, odvisen od drugih modulov

.source[```
module MnozicaDrevesa (U : Urejenost) = struct
  type elt = U.t

  type t =
    | Prazno
    | Sestavljeno of int * t * elt * t

  let rec vsebuje x = function
    | Prazno -> false
    | Sestavljeno (_, l, y, d) ->
        match U.primerjaj x y with
        ...
```]

---

### Če uporabimo funktor, dobimo nov modul

.source[```
module MnozicaCelihStevil =
  MnozicaDrevesa(CelaStevila)
```]

---

class: question

### Konstrukcija urejenosti na produktu

.source[```
module
  Produkt (U1 : Urejenost) (U2 : Urejenost)
  : Urejenost =
  ...
```]


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


type primerjava = Manjsi | Enak | Vecji
module type Urejenost =
sig
    type t
    val primerjaj : t -> t -> primerjava
end

module CelaStevila : Urejenost with type t = int =
struct
    type t = int
    let primerjaj x y =
        if x < y then
            Manjsi
        else if x > y then
            Vecji
        else
            Enak
end

module type Mnozica = sig
  type elt
  type mn
  val prazna : mn
  val dodaj : elt -> mn -> mn
  val vsebuje : elt -> mn -> bool
  val velikost : mn -> int
end

module MnozicaSeznami (U : Urejenost) : Mnozica = struct
  type elt = U.t
  type mn = elt list

  let prazna = []

  let velikost m = List.length m

  let vsebuje x m = List.exists (fun y -> U.primerjaj x y = Enak) m

  let dodaj x m = if vsebuje x m then m else x :: m
end

module MnozicaDrevesa (U : Urejenost) : Mnozica = struct
  type 'a drevo =
    | Prazno
    | Sestavljeno of 'a drevo * 'a * 'a drevo
  
  type elt = U.t
  type mn = elt drevo

  let prazna = Prazno

  let rec velikost = function
    | Prazno -> 0
    | Sestavljeno (l, _, d) -> 1 + velikost l + velikost d

  let rec vsebuje x = function
    | Prazno -> false
    | Sestavljeno (l, y, d) ->
        begin match U.primerjaj x y with
        | Manjsi -> vsebuje x l
        | Vecji -> vsebuje x d
        | Enak -> true
        end

  let rec dodaj x = function
    | Prazno -> Sestavljeno (Prazno, x, Prazno)
    | Sestavljeno (l, y, d) as drevo ->
        begin match U.primerjaj x y with
        | Manjsi -> Sestavljeno (dodaj x l, y, d)
        | Vecji -> Sestavljeno (l, y, dodaj x d)
        | Enak -> drevo
        end
end

module MnozicaNeumnaAVLDrevesa (U : Urejenost) : Mnozica = struct
    type 'a drevo =
        | Prazno
        | Sestavljeno of 'a drevo * 'a * 'a drevo

    type elt = U.t
    type mn = elt drevo

    let prazna = Prazno

    let rec velikost = function
      | Prazno -> 0
      | Sestavljeno (l, _, d) -> 1 + velikost l + velikost d

    let rec vsebuje x = function
        | Prazno -> false
        | Sestavljeno (l, y, d) ->
            begin match U.primerjaj x y with
            | Manjsi -> vsebuje x l
            | Vecji -> vsebuje x d
            | Enak -> true
            end

    let zavrti_levo = function
        | Sestavljeno (l, x, Sestavljeno (dl, y, dd)) ->
            Sestavljeno (Sestavljeno (l, x, dl), y, dd)
        | _ -> failwith "Tega drevesa ne morem zavrteti"

    let zavrti_desno = function
        | Sestavljeno (Sestavljeno (ll, y, dl), x, d) ->
            Sestavljeno (ll, y, Sestavljeno (dl, x, d))
        | _ -> failwith "Tega drevesa ne morem zavrteti"

    let rec visina = function
        | Prazno -> 0
        | Sestavljeno (l, _, d) -> 1 + max (visina l) (visina d)

    let razlika = function
        | Prazno -> 0
        | Sestavljeno (l, _, d) -> visina l - visina d

    let uravnotezi drevo =
        match drevo with
        | Prazno -> Prazno
        | Sestavljeno (l, x, d)
            when razlika drevo = 2 && razlika l = 1 ->
            zavrti_desno drevo
        | Sestavljeno (l, x, d)
            when razlika drevo = 2  ->
            Sestavljeno (zavrti_levo l, x, d) |> zavrti_desno
        | Sestavljeno (l, x, d)
            when razlika drevo = -2 && razlika d = -1 ->
            zavrti_levo drevo
        | Sestavljeno (l, x, d)
            when razlika drevo = -2 ->
            Sestavljeno (l, x, zavrti_desno d) |> zavrti_levo
        | _ -> drevo

  let rec dodaj x = function
    | Prazno -> Sestavljeno (Prazno, x, Prazno)
    | Sestavljeno (l, y, d) as drevo ->
        begin match U.primerjaj x y with
        | Manjsi -> Sestavljeno (dodaj x l, y, d) |> uravnotezi
        | Vecji -> Sestavljeno (l, y, dodaj x d) |> uravnotezi
        | Enak -> drevo
        end

end


module MnozicaAVLDrevesa (U : Urejenost) : Mnozica = struct
     type 'a drevo =
        | Prazno
        | Sestavljeno of 'a drevo * 'a * 'a drevo * int

    type elt = U.t
    type mn = elt drevo

    let prazna = Prazno

    let rec velikost = function
      | Prazno -> 0
      | Sestavljeno (l, _, d, _) -> 1 + velikost l + velikost d

    let rec vsebuje x = function
        | Prazno -> false
        | Sestavljeno (l, y, d, _) ->
            begin match U.primerjaj x y with
            | Manjsi -> vsebuje x l
            | Vecji -> vsebuje x d
            | Enak -> true
            end

    let rec visina = function
        | Prazno -> 0
        | Sestavljeno (_, _, _, h) -> h

    let sestavljeno (l, x, d) =
        let h = 1 + max (visina l) (visina d) in
        Sestavljeno (l, x, d, h)

    let zavrti_levo = function
        | Sestavljeno (l, x, Sestavljeno (dl, y, dd, _), _) ->
            sestavljeno (sestavljeno (l, x, dl), y, dd)
        | _ -> failwith "Tega drevesa ne morem zavrteti"

    let zavrti_desno = function
        | Sestavljeno (Sestavljeno (ll, y, dl, _), x, d, _) ->
            sestavljeno (ll, y, sestavljeno (dl, x, d))
        | _ -> failwith "Tega drevesa ne morem zavrteti"

    let rec visina = function
        | Prazno -> 0
        | Sestavljeno (_, _, _, h) -> h

    let razlika = function
        | Prazno -> 0
        | Sestavljeno (l, _, d, _) -> visina l - visina d

    let uravnotezi drevo =
        match drevo with
        | Prazno -> Prazno
        | Sestavljeno (l, x, d, _)
            when razlika drevo = 2 && razlika l = 1 ->
            zavrti_desno drevo
        | Sestavljeno (l, x, d, _)
            when razlika drevo = 2  ->
            sestavljeno (zavrti_levo l, x, d) |> zavrti_desno
        | Sestavljeno (l, x, d, _)
            when razlika drevo = -2 && razlika d = -1 ->
            zavrti_levo drevo
        | Sestavljeno (l, x, d, _)
            when razlika drevo = -2 ->
            sestavljeno (l, x, zavrti_desno d) |> zavrti_levo
        | _ -> drevo

    let rec dodaj x = function
        | Prazno -> sestavljeno (Prazno, x, Prazno)
        | Sestavljeno (l, y, d, _) as drevo ->
            begin match U.primerjaj x y with
            | Manjsi -> sestavljeno (dodaj x l, y, d) |> uravnotezi
            | Vecji -> sestavljeno (l, y, dodaj x d) |> uravnotezi
            | Enak -> drevo
            end
end

(* module M = MnozicaSeznami(CelaStevila) *)
(* module M = MnozicaDrevesa(CelaStevila) *)
(* module M = MnozicaNeumnaAVLDrevesa(CelaStevila) *)
module M = MnozicaAVLDrevesa(CelaStevila)

let stevilo_razlicnih xs =
  let rec aux ze_videni = function
    | [] -> M.velikost ze_videni
    | x :: xs -> aux (M.dodaj x ze_videni) xs
  in
  aux M.prazna xs

let nakljucni_seznam m n = List.init n (fun _ -> Random.int m)
let seznam_zaporednih n = List.init n (fun i -> i)

let stopaj f x =
  let zacetek = Sys.time () in
  let y = f x in
  let konec = Sys.time () in
  print_endline ("Porabljen čas: " ^ string_of_float (1000. *. (konec -. zacetek)) ^ "ms");
  y

(* let primer = nakljucni_seznam 10000 10000 *)
let primer = seznam_zaporednih 10000

let n = stopaj stevilo_razlicnih primer

;;

Random.self_init ();
nakljucni_seznam 5000 5000
(* seznam_zaporednih 5000 *)
|> stopaj stevilo_razlicnih
|> string_of_int
|> print_endline
