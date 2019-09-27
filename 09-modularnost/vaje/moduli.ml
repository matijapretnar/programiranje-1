(* ========== Vaja 8: Moduli  ========== *)

(*----------------------------------------------------------------------------*]
 Definirajte signaturo [NAT], ki določa strukturo naravnih števil. Ima osnovni 
 tip, funkcijo enakosti, ničlo in enko, seštevanje, odštevanje in množenje.
 Hkrati naj vsebuje pretvorbe iz in v OCamlov [int] tip.

 Opomba: Funkcije za pretvarjanje ponavadi poimenujemo [to_int] and [of_int],
 tako da skupaj z imenom modula dobimo ime [NAT.of_int], ki nam pove, da 
 pridobivamo naravno število iz celega števila.
[*----------------------------------------------------------------------------*)

module type NAT = sig
  type t

  val eq   : t -> t -> bool
  val zero : t
  val one  : t
  val add  : t -> t -> t
  val sub  : t -> t -> t
  val mul  : t -> t -> t
  val from_int : int -> t
  val to_int   : t -> int
end

(*----------------------------------------------------------------------------*]
 Napišite implementacijo modula [Nat_int], ki zgradi modul s signaturo [NAT],
 kjer kot osnovni tip uporablja OCamlov tip [int].

 Namig: Dokler ne implementirate vse funkcij v [Nat_int] se bo OCaml pritoževal.
 Temu se lahko izognete tako, da funkcije, ki še niso napisane nadomestite z 
 [failwith "later"], vendar to ne deluje za konstante.
[*----------------------------------------------------------------------------*)

module Nat_int : NAT = struct

  type t = int
  let eq = (=)
  let zero = 0
  let one = 1
  let add = (+)
  let sub x y = max 0 (x - y)
  let mul = ( * )
  let from_int n = n
  let to_int i = i

end

(*----------------------------------------------------------------------------*]
 Napišite implementacijo [NAT], ki temelji na Peanovih aksiomih:
 https://en.wikipedia.org/wiki/Peano_axioms
   
 Osnovni tip modula definirajte kot vsotni tip, ki vsebuje konstruktor za ničlo
 in konstruktor za naslednika nekega naravnega števila.
 Večino funkcij lahko implementirate s pomočjo rekurzije. Naprimer, enakost
 števil [k] in [l] določimo s hkratno rekurzijo na [k] in [l], kjer je osnoven
 primer [Zero = Zero].

[*----------------------------------------------------------------------------*)

module Nat_peano : NAT = struct

  type t = Zero | S of t

  let rec eq x y =
    match (x, y) with
    | (Zero, Zero) -> true
    | (S x, S y) -> eq x y
    | _ -> false
  
  let zero = Zero
  
  let one = S Zero
  
  let rec add x = function
    | Zero -> x
    | S y -> S (add x y)
  
  let rec sub x y =
    match (x, y) with
    | (_, Zero) -> x
    | (Zero, _) -> Zero
    | (S x, S y) -> sub x y
  
  let rec mul x y =
    match x with
    | Zero -> Zero
    | S x -> add y (mul x y)
  
  let rec from_int i =
    if i <= 0
    then Zero
    else S (from_int (i-1))
  
  let rec to_int = function
    | Zero -> 0
    | S n -> 1 + (to_int n)

end

(*----------------------------------------------------------------------------*]
 V OCamlu lahko module podajamo kot argumente funkcij, z uporabo besede
 [module]. Funkcijo, ki sprejme modul torej definiramo kot

 # let f (module M : M_sig) = ...

 in ji podajamo argumente kot 

 # f (module M_implementation);;

 Funkcija [sum_nat_100] sprejme modul tipa [NAT] in z uporabo modula sešteje
 prvih 100 naravnih števil. Ker funkcija ne more vrniti rezultata tipa [NAT.t]
 (saj ne vemo, kateremu od modulov bo pripadal, torej je lahko [int] ali pa
  variantni tip) na koncu vrnemo rezultat tipa [int] z uporabo metode [to_int].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # sum_nat_100 (module Nat_int);;
 - : int = 4950
 # sum_nat_100 (module Nat_peano);;
 - : int = 4950
[*----------------------------------------------------------------------------*)

let sum_nat_100 (module Nat : NAT) =
  let hundred = Nat.from_int 100 in
  let rec sum current acc =
    if Nat.eq current hundred then
      acc
    else
      sum (Nat.add current Nat.one) (Nat.add acc current)
  in
  sum Nat.zero Nat.zero |> Nat.to_int


(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Now we follow the fable told by John Reynolds in the introduction.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Definirajte signaturo modula kompleksnih števil.
 Potrebujemo osnovni tip, test enakosti, ničlo, enko, imaginarno konstanto i,
 negacijo, konjugacijo, seštevanje in množenje. 
[*----------------------------------------------------------------------------*)

module type COMPLEX = sig
  type t

  val eq : t -> t -> bool
  val zero : t
  val one : t
  val i : t
  val neg : t -> t
  val conj : t -> t
  val add : t -> t -> t
  val mul : t -> t -> t
end

(*----------------------------------------------------------------------------*]
 Napišite kartezično implementacijo kompleksnih števil, kjer ima vsako
 kompleksno število realno in imaginarno komponento.
[*----------------------------------------------------------------------------*)

module Cartesian : COMPLEX = struct

  type t = {re : float; im : float}

  let eq x y = x.re = y.re && x.im = y.im

  let zero = {re = 0.; im = 0.}
  let one =  {re = 1.; im = 0.}
  let i   =  {re = 0.; im = 1.}

  let neg {re; im} = {re = -. re; im = -. im}
  let conj {re; im} = {re; im = -. im}

  let add x y = {re = x.re +. y.re; im = x.im +. y.im}
  let mul x y = 
    let re = x.re *. y.re -. x.im *. y.im in
    let im = x.im *. y.re +. x.re *. y.im in
    {re; im}

  end

(*----------------------------------------------------------------------------*]
 Sedaj napišite še polarno implementacijo kompleksnih števil, kjer ima vsako
 kompleksno število radij in kot (angl. magnitude in argument).
   
 Priporočilo: Seštevanje je v polarnih koordinatah zahtevnejše, zato si ga 
 pustite za konec (lahko tudi za konec stoletja).
[*----------------------------------------------------------------------------*)

module Polar : COMPLEX = struct

  type t = {magn : float; arg : float}

  let pi = 2. *. acos 0.
  let rad deg = (deg /. 180.) *. pi
  let deg rad = (rad /. pi) *. 180.

  let eq x y =
    x.magn = y.magn &&
      (x.magn = 0. ||
         (mod_float x.arg 360.
          = mod_float y.arg 360.))

  let zero = {magn = 0.; arg = 0.}
  let one = {magn = 1.; arg = 0.}
  let i = {magn = 1.; arg = 90.}

  let neg {magn; arg} = {magn; arg = arg +. 180.}
  let conj {magn; arg} = {magn; arg = (mod_float (arg +. 180.) 360.)}

  let mul x y = {magn = x.magn *. y.magn ; arg = x.arg +. y.arg}

  (* All of this for addition... *)
  let re {magn; arg} = magn *. cos (rad arg)
  let im {magn; arg} = magn *. sin (rad arg)

  let arg re im =
    let rad =
      if re > 0. then atan (im /. re)
      else if re < 0. && im >= 0. then atan (im /. re) +. pi
      else if re < 0. && im < 0. then  atan (im /. re) -. pi
      else if re = 0. && im > 0. then pi /. 2.
      else if re = 0. && im < 0. then -.(pi /. 2.)
      else 0.
    in deg rad

  let magn re im = sqrt (re *. re +. im *. im)

  let add x y =
    let square x = x *. x in
    let magn = sqrt (square x.magn +. square y.magn +. 2. *. x.magn *. y.magn *. cos (y.arg -. x.arg))
    and arg = x.arg +.
                atan2 (y.magn *. sin (y.arg -. x.arg))
                      (x.magn +. y.magn *. cos (y.arg -. x.arg)) in
    {magn; arg}

  let add' x y =
    let z_re, z_im = re x +. re y, im x +. im y in
    let arg = arg z_re z_im
    and magn = magn z_re z_im
    in {arg; magn}

end


(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 SLOVARJI
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Na vajah z iskalnimi drevesi smo definirali tip slovarjev 
 [('key, 'value) dict], ki je implementiral [dict_get], [dict_insert] in
 [print_dict]. Napišite primerno signaturo za slovarje [DICT] in naredite
 implementacijo modula z drevesi (kot na prejšnjih vajah). 
 
 Modul naj vsebuje prazen slovar [empty] in pa funkcije [get], [insert] in
 [print] (print naj ponovno deluje zgolj na [(string, int) t].
[*----------------------------------------------------------------------------*)

module type DICT = sig
  type ('key, 'value) t

  val empty : ('key, 'value) t

  val get : 'key -> ('key, 'value) t -> 'value option
  val insert : 'key -> 'value -> ('key, 'value) t -> ('key, 'value) t
  val print : (string, int) t -> unit
end

module Tree_dict : DICT = struct
  type ('key, 'value) t =
    | D_Empty
    | D_Node of ('key, 'value) t * 'key * 'value * ('key, 'value) t

  let empty = D_Empty

  let d_leaf key value = D_Node (D_Empty, key, value, D_Empty)

  let rec get key = function
    | D_Empty -> None
    | D_Node (d_l, k, value, d_r) ->
      if k = key then
        Some value
      else if key < k then
        get key d_l
      else
      get key d_r

  let rec insert key value = function
    | D_Empty -> d_leaf key value
    | D_Node (d_l, k, v, d_r) ->
      if k = key then
        D_Node (d_l, k, value, d_r)
      else if key < k then
        D_Node (insert key value d_l, k, v, d_r)
      else
        D_Node (d_l, k, v, insert key value d_r)

  let rec print = function
    | D_Empty -> ()
    | D_Node (d_l, k, v, d_r) -> (
      print d_l;
      print_string (k ^ " : "); print_int v; print_string "\n";
      print d_r)

end

(*----------------------------------------------------------------------------*]
 Funkcija [count (module Dict) list] prešteje in izpiše pojavitve posameznih
 elementov v seznamu [list] s pomočjo izbranega modula slovarjev.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # count (module Tree_dict) ["b"; "a"; "n"; "a"; "n"; "a"];;
 a : 3
 b : 1
 n : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

let count (module Dict : DICT) list =
  let rec counter dict = function
    | [] -> Dict.print dict
    | x :: xs -> 
      let new_count = 
        match Dict.get x dict with
        | Some x -> x + 1 
        | None -> 1
      in
      let new_dict = Dict.insert x new_count dict in
      counter new_dict xs
  in
  counter Dict.empty list
