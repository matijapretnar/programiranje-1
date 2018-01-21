(* Zgodba, ki naj ostane neprevedena:

"Once upon a time, there was a university with a peculiar tenure policy. All
 faculty were tenured, and could only be dismissed for moral turpitude. What
 was peculiar was the definition of moral turpitude: making a false statement
 in class. Needless to say, the university did not teach computer science.
 However, it had a renowned department of mathematics.

 One Semester, there was such a large enrollment in complex variables that two
 sections were scheduled. In one section, Professor Descartes announced that a
 complex number was an ordered pair of reals, and that two complex numbers were
 equal when their corresponding components were equal. He went on to explain
 how to convert reals into complex numbers, what "i" was, how to add, multiply,
 and conjugate complex numbers, and how to find their magnitude.

 In the other section, Professor Bessel announced that a complex number was an
 ordered pair of reals the first of which was nonnegative, and that two complex
 numbers were equal if their first components were equal and either the first
 components were zero or the second components differed by a multiple of 2π. He
 then told an entirely different story about converting reals, "i", addition,
 multiplication, conjugation, and magnitude.

 Then, after their first classes, an unfortunate mistake in the registrar's
 office caused the two sections to be interchanged. Despite this, neither
 Descartes nor Bessel ever committed moral turpitude, even though each was
 judged by the other's definitions. The reason was that they both had an
 intuitive understanding of type. Having defined complex numbers and the
 primitive operations upon them, thereafter they spoke at a level of
 abstraction that encompassed both of their definitions.

 The moral of this fable is that:
   Type structure is a syntactic discipline for enforcing levels of
   abstraction."

 from:
 John C. Reynolds, "Types, Abstraction, and Parametric Polymorphism", IFIP83

 (optional) homework: read the rest of the introduction of the paper at
 https://people.mpi-sws.org/~dreyer/tor/papers/reynolds.pdf
 *)


(* Kompleksna števila so zahtevnejša. Pričnimo z Nat. *)


(* Definirajmo signaturo "NAT", ki določa strukturo naravnih števil. Ima
   osnovni tip, funkcijo enakosti, ničlo in enko, seštevanje, odštevanje in
   množenje. Hkrati naj vsebuje pretvorbe iz in v OCamlov "int" tip.*)
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

(* Napiši modul, ki zgradi modul s podpisom tipa NAT z uporabo OCamlovega
   "int" tipa.

   Opozorilo: Dokler ni implementiranih vse funkcij v Nat_int se bo OCaml
   pritoževal. Temu se lahko izogneš tako, da funkcije, ki jih še niso napisane
   nadomestiš s 'failwith "later"'. *)

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

(* Sedaj naravna števila naredi s pomočjo Peanovih aksiomov
   https://en.wikipedia.org/wiki/Peano_axioms

   Osnovni tip modula podaj kot vsotni tip, kjer imaš ničlo in pa
   naslednjika nekega naravnega števila [Zero in Successor n].

   Funkcije implementiraj s pomočjo rekurzije. Števili m in n sta enaki, če
   sta obe 0, ali pa sta naslednika k in l, kjer sta k in l enaki števili. *)


module Nat_peano = struct
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

(* Definiraj signaturo modula kompleksnih števil.
   Potrebujemo osnovni tip, test enakosti, ničlo, enko, imaginarno konstanto i,
   negacijo števila, konjugacijo, seštevanje, množenje, deljenje in inverz. *)

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
  val div : t -> t -> t
  val inv : t -> t
end
(* Napiši kartezično implementacijo kompleksnih števil (torej z = x + iy).
   Deljenje je zahtevnejše, zato si ga lahko s 'failwith' trikom pustiš za kasneje.
 *)

module Cartesian : COMPLEX = struct

  type t = {re : float; im : float}

  let eq x y = x.re = y.re && x.im = y.im

  let zero = {re = 0.; im = 0.}
  let one =  {re = 1.; im = 0.}
  let i   =  {re = 0.; im = 1.}

  let neg {re; im} = {re = -. re; im = -. im}
  let conj {re; im} = {re; im = -. im}

  let add x y = {re = x.re +. y.re; im = x.im +. y.im}

  let mul x y = {re = x.re *. y.re -. x.im *. y.im;
                 im = x.im *. y.re +. x.re *. y.im}

  let div x y =
    let denominator = (y.re *. y.re +. y.im *. y.im) in
    let re = (x.re *. y.re +. x.im *. y.im) /. denominator
    and im = (x.im *. y.re -. x.re *. y.im) /. denominator
    in {re; im}

  let inv = div one

end


(* Sedaj napiši še polarno implementacijo kompleksnih števil (torej z = r e^(i*fi) ).

   Seštevanje je v polarnih koordinatah zahtevnejše, zato najprej napiši druge reči. *)

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

  let re {magn; arg} = magn *. cos (rad arg)
  let im {magn; arg} = magn *. sin (rad arg)

  let mul x y = {magn = x.magn *. y.magn ; arg = x.arg +. y.arg}
  let div x y = {magn = x.magn /. y.magn ; arg = x.arg -. y.arg}

  let inv = div one

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
