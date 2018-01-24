type vektor = int * int
type matrika = int * int * int * int

module type Linearna = sig
  (* Osnovni tip modula. *)
  type t
  (* Identiteta. *)
  val id : t
  (* Izračun funkcije na podatkih. *)
  val uporabi : vektor -> t -> vektor
  (* Funkcija, ki sprejme matriko in jo pretvori v osnovni tip modula.
     Če je osnovni tip modula matrika, pretvori matriko v matriko. *)
  val iz_matrike : matrika -> t
  (* Funkcija, ki sprejme funkcijo in jo pretvori v osnovni tip modula.
     Če je osnovni tip modula matrika, pretvori funkcijo v matriko. *)
  val iz_funkcije : (vektor -> vektor) -> t
  (* Vrne kompozitum dveh preslikav. *)
  val kompozitum : t -> t -> t
end

module Matrika : Linearna = struct
  (* Dopolni me! *)
end

module Funkcija : Linearna = struct
  (* Dopolni me! *)
end
