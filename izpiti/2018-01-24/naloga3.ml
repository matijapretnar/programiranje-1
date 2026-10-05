type vektor = int * int
type matrika = int * int * int * int

module type Linearna = sig
  (* Tip linearnih preslikav *)
  type t
  (* Identiteta *)
  val id : t
  (* Dano preslikavo uporabi na vektorju *)
  val uporabi : t -> vektor -> vektor
  (* Vrne linearno preslikavo, določeno z matriko *)
  val iz_matrike : matrika -> t
  (* Vrne linearno preslikavo, določeno s funkcijo
     (predpostavite lahko, da je funkcija linearna) *)
  val iz_funkcije : (vektor -> vektor) -> t
  (* Vrne kompozitum danih preslikav. *)
  val kompozitum : t -> t -> t
end
