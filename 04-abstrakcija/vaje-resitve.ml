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
  let rec ( + ) x y = match x with Zero -> y | Suc x' -> Suc (x' + y)

  let rec ( - ) x y =
    match (x, y) with
    | x, Zero -> x
    | Suc x', Suc y' -> x' - y'
    | Zero, _ -> Zero (* y > x, vrnemo kar 0 *)

  let rec ( * ) x y =
    match (x, y) with
    | Zero, _ | _, Zero -> Zero
    | x, Suc Zero -> x
    | x, Suc y' -> x + (x * y')

  let rec to_int = function Zero -> 0 | Suc x -> Int.add 1 (to_int x)
  let rec of_int = function 0 -> Zero | n -> Suc (of_int (Int.sub n 1))
end

(*----------------------------------------------------------------------------*
   Z ukazom `let module ImeModula = ... in ...` lahko modul definiramo samo
   lokalno. To bomo uporabili za to, da bomo lahko enostavno preklapljali med
   moduloma `Nat_int` in `Nat_peano`, saj bomo enega ali drugega shranili pod ime
   `Nat`. OCaml sicer pozna tudi ustrezne abstrakcije, ki omogočijo preklapljanje
   med moduli, na primer [funktorje](https://ocaml.org/docs/functors) ali
   [prvorazredne module](https://ocaml.org/manual/5.2/firstclassmodules.html), a
   bomo uporabili preprostejšo rešitev.

   Spodnji izračun dopolnite tako, da sešteje prvih 100 naravnih števil. Ker bo
   taka vsota tipa `NAT.t`, ki je abstrakten, končni rezultat pretvorite v tip
   `int` z uporabo funkcije `Nat.to_int`. Če ste oba modula implementirali
   pravilno, bi morali dobiti enak rezultat ne glede na to, katerega poimenujete
   `Nat`.
  [*----------------------------------------------------------------------------*)

let sum_nat_100 =
  (* let module Nat = Nat_int in *)
  let module Nat = Nat_peano in
  let rec sum n =
    if Nat.eq n Nat.zero then n else Nat.( + ) n (sum (Nat.( - ) n Nat.one))
  in
  sum (Nat.of_int 100) |> Nat.to_int

let () = assert (sum_nat_100 = 5050)

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
  (* Dodajte manjkajoče! *)
end

(*----------------------------------------------------------------------------*
   Napišite kartezično implementacijo kompleksnih števil, kjer ima vsako
   kompleksno število realno in imaginarno komponento.
  [*----------------------------------------------------------------------------*)

module Cartesian : COMPLEX = struct
  type t = { re : float; im : float }

  let eq x y = failwith "later"
  (* Dodajte manjkajoče! *)
end

(*----------------------------------------------------------------------------*
   Sedaj napišite še polarno implementacijo kompleksnih števil, kjer ima vsako
   kompleksno število radij in kot (angl. magnitude in argument). Priporočilo:
   Seštevanje je v polarnih koordinatah zahtevnejše, zato si ga pustite za konec
   (lahko tudi za konec stoletja).
  [*----------------------------------------------------------------------------*)

module Polar : COMPLEX = struct
  type t = { magn : float; arg : float }

  (* Pomožne funkcije za lažje življenje. *)
  let pi = 2. *. acos 0.
  let rad_of_deg deg = deg /. 180. *. pi
  let deg_of_rad rad = rad /. pi *. 180.
  let eq x y = failwith "later"
  (* Dodajte manjkajoče! *)
end
