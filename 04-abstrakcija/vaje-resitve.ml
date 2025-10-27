(*----------------------------------------------------------------------------*
   # Abstrakcija
  [*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
   ## Naravna števila
  [*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
   Definirajte signaturo `NAT`, ki določa strukturo naravnih števil. Ima osnovni
   tip, funkcijo enakosti, ničlo in enko, seštevanje, odštevanje in množenje.
   Hkrati naj vsebuje pretvorbe iz in v OCamlov `int` tip. Opomba: Funkcije za
   pretvarjanje ponavadi poimenujemo `to_int` and `of_int`, tako da skupaj z
   imenom modula dobimo ime `NAT.of_int`, ki nam pove, da pridobivamo naravno
   število iz celega števila.
  [*----------------------------------------------------------------------------*)

module type NAT = sig
  type t

  val eq : t -> t -> bool
  val zero : t
  val one : t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val to_int : t -> int
  val of_int : int -> t
  val to_str : t -> string
end

(*----------------------------------------------------------------------------*
   Napišite implementacijo modula `Nat_int`, ki zgradi modul s signaturo `NAT`,
   kjer kot osnovni tip uporablja OCamlov tip `int`. Namig: dokler ne
   implementirate vse funkcij v `Nat_int`, se bo OCaml pritoževal. Temu se lahko
   izognete tako, da funkcije, ki še niso napisane nadomestite z `failwith
   "later"`, vendar to ne deluje za konstante.
  [*----------------------------------------------------------------------------*)

module Nat_int : NAT = struct
  type t = int

  let eq x y = x = y
  let zero = 0
  let one = 1

  (* Ker je let nerekurziven je + v notranjosti leta "drugačen (tisti za vgrajene inte)" *)
  let ( + ) = ( + )
  let ( - ) = ( - )
  let ( * ) = ( * )
  let to_int x = x
  let of_int x = x
  let to_str x = "[N_int]: " ^ (string_of_int x)
end

(*----------------------------------------------------------------------------*
   Napišite implementacijo `NAT`, ki temelji na [Peanovih
   aksiomih](https://en.wikipedia.org/wiki/Peano_axioms). Osnovni tip modula
   definirajte kot naštevni tip, ki vsebuje konstruktor za ničlo in konstruktor za
   naslednika nekega naravnega števila. Večino funkcij lahko implementirate s
   pomočjo rekurzije. Naprimer, enakost števil `k` in `l` določimo s hkratno
   rekurzijo na `k` in `l`, kjer je osnoven primer `Zero = Zero`.
  [*----------------------------------------------------------------------------*)

module Nat_peano : NAT = struct
  type t = Zero | Suc of t

  let rec eq x y =
    match (x, y) with
    | Zero, Zero -> true
    | Suc x', Suc y' -> eq x' y'
    | _ -> false

  let zero = Zero
  let one = Suc Zero
  let rec ( + ) x y = 
    match x with 
      | Zero -> y 
      | Suc x' -> Suc (x' + y)

  let rec ( - ) x y =
    match (x, y) with
    | x, Zero -> x
    | Suc x', Suc y' -> x' - y'
    | Zero, _ -> Zero (* y > x, vrnemo kar 0 *)

  let rec ( * ) x y =
    match x with
    | Zero -> Zero
    | Suc x' -> y + (x' * y)

  let rec to_int = function Zero -> 0 | Suc x -> Int.add 1 (to_int x)
  let rec of_int = function 0 -> Zero | n -> Suc (of_int (Int.sub n 1))

  let to_str n =
    let aux = function
      | Zero -> "Zero"
      | Suc m -> "Succ(-" ^ string_of_int ((to_int (m+one))) ^ "-(Zero)--)"
    in
    "[N_peano]: " ^ aux n
end

(*----------------------------------------------------------------------------*
 Ocaml omogoča sestavljanje modulov iz drugih modulov z uporabo [funktorjev]
 (https://ocaml.org/docs/functors). 
 Funktor je tako preslikava med moduli, zapišemo jo v obliki modula, ki kot
 argumente sprejme druge module. Tako uporabi zapis naslednjo strukturo
 `module Ime_funktorja (Ime_modula : SIG1) : SIG2 = struct ... end`.

 Modul za računanje z naravnimi števili ima signaturo `CALC` (vanjo lahko 
 dodate še kakšne funkcije). Module s to signaturo bomo zgradili z uporabo 
 funktorja `Nat_calculations`, ki sprejme argument modula s signaturo `NAT` 
 in računal z operacijami, ki jih ima ta signatura. 
 Napišite funkciji fakultete in vsote prvih 100 naravnih števil, ki jih signatura 
 `CALC` zahteva.
[*----------------------------------------------------------------------------*)
module type CALC = sig
  type t

  val factorial : t -> t
  val sum_100 : t
end

module Nat_calculations (N: NAT) : CALC with type t := N.t = struct
  let rec factorial n = 
    if N.eq n N.zero then N.one
    else N.( * ) n (factorial (N.( - ) n N.one))

  let sum_100 = 
    let rec sum_first n acc =
      if N.eq n N.zero then acc
      else sum_first (N.( - ) n N.one) (N.( + ) n acc)
    in
    sum_first (N.of_int 100) N.zero
end

(*----------------------------------------------------------------------------*
 Z moduli funktorja `Nat_calculations` lahko sedaj preverimo pravilnost
 implementacij `Nat_int` in `Nat_peano`. Definirajte modula `Nat_int_calc` in
 `Nat_peano_calc`, ki sta aplikaciji funktorja `Nat_calculations` na argumentih
 `Nat_int` in `Nat_peano`. Nato za oba primera izračunajte vsoti prvih 100 števil
  in fakulteto 5.
[*----------------------------------------------------------------------------*)

module Nat_int_calc = Nat_calculations (Nat_int)
module Nat_peano_calc = Nat_calculations (Nat_peano)

let sum_100_int = 
  Nat_int_calc.sum_100 |> Nat_int.to_int
let fact_5_int = 
  Nat_int_calc.factorial (Nat_int.of_int 5) |> Nat_int.to_int

let sum_100_peano = 
  Nat_peano_calc.sum_100 |> Nat_peano.to_int
let fact_5_peano =
  Nat_peano_calc.factorial (Nat_peano.of_int 5) |> Nat_peano.to_int

(* val sum_100_int : int = 5050 *)
(* val sum_100_peano : int = 5050 *)
(* val fact_5_int : int = 120 *)
(* val fact_5_peano : int = 120 *)

(*----------------------------------------------------------------------------*
 Funktor lahko sprejme tudi več modulov, njegova končna signatura pa je poljubna,
 torej je lahko enaka signaturi modulov, ki ju sprejme kot argumenta.
 
 Napišite funktor `Nat_pair`, ki sprejme dva modula s signaturo `NAT` in vrne
 modul s signaturo `NAT`. Osnovni tip definiranega modula naj bo par števil 
 tipov modulov iz argumentov. Računske operacije naj delujejo po komponentah.
 Pretvorjanje iz in v `int` pa definirajte poljubno.
[*----------------------------------------------------------------------------*)

module Nat_pair (A: NAT) (B: NAT) : NAT = struct
  type t = A.t * B.t

  let eq (x1, y1) (x2, y2) = A.eq x1 x2 && B.eq y1 y2
  let zero = (A.zero, B.zero)
  let one = (A.one, B.one)
  let ( + ) (x1, y1) (x2, y2) = (A.( + ) x1 x2, B.( + ) y1 y2)
  let ( - ) (x1, y1) (x2, y2) = (A.( - ) x1 x2, B.( - ) y1 y2)
  let ( * ) (x1, y1) (x2, y2) = (A.( * ) x1 x2, B.( * ) y1 y2)
  let to_int (a, b) = Int.add (A.to_int a) (B.to_int b)
  let of_int n =
    let half = n / 2 in
    (A.of_int half, B.of_int (Int.sub n half))
  let to_str (a, b) = "[N_pair]: ( " ^ A.to_str a ^ ", " ^ B.to_str b ^ " )"
end

module Nat_pair_int_peano = Nat_pair (Nat_int) (Nat_peano)
let primer_pair = 
  let open Nat_pair_int_peano in
  let a = of_int 5 in
  let b = of_int 10 in
  let c = (a + b) in
  let () = Printf.printf "Pair a: %s\n" (to_str a) in
  let () = Printf.printf "Pair b: %s\n" (to_str b) in
  let () = Printf.printf "Pair c (a + b): %s\n" (to_str c) in
  let () = Printf.printf "Pair c as int: %d\n" (to_int c) in
  ()

(*----------------------------------------------------------------------------*
   ## Kompleksna števila
  [*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
   > Once upon a time, there was a university with a peculiar tenure
   > policy. All faculty were tenured, and could only be dismissed for
   > moral turpitude. What was peculiar was the definition of moral
   > turpitude: making a false statement in class. Needless to say, the
   > university did not teach computer science. However, it had a renowned
   > department of mathematics.
   >
   > One Semester, there was such a large enrollment in complex variables
   > that two sections were scheduled. In one section, Professor Descartes
   > announced that a complex number was an ordered pair of reals, and that
   > two complex numbers were equal when their corresponding components
   > were equal. He went on to explain how to convert reals into complex
   > numbers, what "i" was, how to add, multiply, and conjugate complex
   > numbers, and how to find their magnitude.
   >
   > In the other section, Professor Bessel announced that a complex number
   > was an ordered pair of reals the first of which was nonnegative, and
   > that two complex numbers were equal if their first components were
   > equal and either the first components were zero or the second
   > components differed by a multiple of 2π. He then told an entirely
   > different story about converting reals, "i", addition, multiplication,
   > conjugation, and magnitude.
   >
   > Then, after their first classes, an unfortunate mistake in the
   > registrar's office caused the two sections to be interchanged. Despite
   > this, neither Descartes nor Bessel ever committed moral turpitude,
   > even though each was judged by the other's definitions. The reason was
   > that they both had an intuitive understanding of type. Having defined
   > complex numbers and the primitive operations upon them, thereafter
   > they spoke at a level of abstraction that encompassed both of their
   > definitions.
   >
   > The moral of this fable is that: Type structure is a syntactic
   > discipline for enforcing levels of abstraction.
   >
   > John C. Reynolds, _Types, Abstraction, and Parametric Polymorphism_, IFIP83
  [*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
   Definirajte signaturo modula kompleksnih števil. Potrebujemo osnovni tip, test
   enakosti, ničlo, enko, imaginarno konstanto i, negacijo, konjugacijo,
   seštevanje in množenje.
  [*----------------------------------------------------------------------------*)

module type COMPLEX = sig
  type t

  val eq : t -> t -> bool
  val zero : t
  val one : t
  val i : t
  val neg : t -> t
  val conj : t -> t
  val ( ++ ) : t -> t -> t
  val ( ** ) : t -> t -> t
  val to_string : t -> string
end

(*----------------------------------------------------------------------------*
   Napišite kartezično implementacijo kompleksnih števil, kjer ima vsako
   kompleksno število realno in imaginarno komponento.
  [*----------------------------------------------------------------------------*)

module Cartesian : COMPLEX = struct

  type t = {re : float; im : float}
  
  let zero = {re = 0.; im = 0.}
  let one = {re = 1.; im = 0.}
  let i = {re = 0.; im = 1.}
  
  let eq x y = x = y
  let neg x = {re = -. x.re; im = -. x.im}
  let conj x = { x with im = -. x.im }
  let ( ++ ) x y = { re = x.re +. y.re; im = x.im +. y.im }
  let ( ** ) x y = 
    { re = (x.re *. y.re) -. (x.im *. y.im);
      im = (x.re *. y.im) +. (x.im *. y.re) }

  let to_string x = "{re = " ^ string_of_float x.re ^ "; im = " ^ string_of_float x.im ^ "}"
end

let primer_complex_cartesian = 
  let () = print_endline "Example in Cartesian." in
  let i = Cartesian.i in
  let one = Cartesian.one in 
  let one_onei = Cartesian.( ++ ) i one in
  let () = Cartesian.to_string one_onei |> print_endline in
  let () = Cartesian.neg one_onei |> Cartesian.to_string |> print_endline in
  let () = Cartesian.conj one_onei |> Cartesian.to_string |> print_endline in
  let () = Cartesian.( ** ) one_onei one_onei |> Cartesian.to_string |> print_endline in
  ()

(*----------------------------------------------------------------------------*
   Sedaj napišite še polarno implementacijo kompleksnih števil, kjer ima vsako
   kompleksno število radij in kot (angl. magnitude in argument). Priporočilo:
   Seštevanje je v polarnih koordinatah zahtevnejše, zato si ga pustite za konec
   (lahko tudi za konec stoletja).
  [*----------------------------------------------------------------------------*)

module Polar : COMPLEX = struct

  type t = {magn : float; arg : float}

  (* Pomožne funkcije za lažje življenje. *)
  let pi = 2. *. acos 0.
  let rad_of_deg deg = (deg /. 180.) *. pi
  let deg_of_rad rad = (rad /. pi) *. 180.
  let normalize_arg arg = if arg >= 2. *. pi then arg -. (2. *. pi) else arg

  let zero = {magn = 0.; arg = 0.}
  let one = {magn = 1.; arg = 0.}
  let i = {magn = 1.; arg = pi /. 2.}

  let eq x y = x = y
  let neg x = { x with arg = normalize_arg (x.arg +. pi) }
  let conj x = { x with arg = (2. *. pi) -. x.arg}
  
  let ( ++ ) x _ = x  (* Implementacija, ki si zasluži čast in slavo *)
  let ( ** ) x y = 
    { magn = x.magn *. y.magn; arg = normalize_arg (x.arg +. y.arg) }
  
  let to_string x = "{magn = " ^ string_of_float x.magn ^ "; arg = " ^ string_of_float (deg_of_rad x.arg) ^ "°}"
end

let primer_complex_polar = 
  let () = print_endline "Example in polar." in
  let i = Polar.i in
  let minus_one = Polar.( ** ) i i in
  let () = Polar.to_string minus_one |> print_endline in
  let () = Polar.neg minus_one |> Polar.to_string |> print_endline in
  ()
