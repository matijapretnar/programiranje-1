
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
  type t = int*int*int*int

  let id = (1, 0, 0, 1)
  let apply (x, y) (a, b, c, d) = (x*a + y*b, x*c + y*d)
  let of_matrix m = m
  let of_function f =
    let (a, c) = f (1, 0) in
    let (b, d) = f (0, 1) in
    (a, b, c, d)
  let compose (a, b, c, d) (e, f, g, h) =
    (a*e + b*g, a*f + b*h, c*e + d*g, c*f + c*h)
end


module Function : Linear = struct
  type t = int*int -> int*int

  let id = (fun x -> x)
  let apply v f = f v
  let of_matrix (a, b, c, d) = fun (x, y) -> (x*a + y*b, x*c + y*d)
  let of_function f = f
  let compose f g = fun x -> f (g x)
end
