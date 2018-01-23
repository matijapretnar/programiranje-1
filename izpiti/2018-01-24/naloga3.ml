module type Linear = sig
  (* Osnovni tip modula. *)
  type t
  (* Identiteta. *)
  val id : t
  (* Izračun funkcije na podatkih. *)
  val apply : int*int -> t -> int*int
  (* Funkcija, ki sprejme matriko in jo pretvori v osnovni tip modula.
     Če je osnovni tip modula matrika, pretvori matriko v matriko. *)
  val of_matrix : int*int*int*int -> t
  (* Funkcija, ki sprejme funkcijo in jo pretvori v osnovni tip modula.
     Če je osnovni tip modula matrika, pretvori funkcijo v matriko. *)
  val of_function : ((int*int) -> (int*int)) -> t
  (* Vrne kompozitum dveh preslikav. *)
  val compose : t -> t -> t
end

module Matrix : Linear = struct
  (* Dopolni me! *)
end

module Function : Linear = struct
  (* Dopolni me! *)
end
