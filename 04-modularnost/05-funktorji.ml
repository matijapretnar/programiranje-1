module type KOLOBAR = sig
  type elt

  val ( + ) : elt -> elt -> elt
  val ( - ) : elt -> elt -> elt
  val ( * ) : elt -> elt -> elt
  val ( ! ) : int -> elt
  val nic : elt
  val ena : elt
  val izpis : elt -> string
end

module CelaStevila : KOLOBAR = struct
  type elt = int

  let ( + ) = ( + )
  let ( - ) = ( - )
  let ( * ) = ( * )
  let nic = 0
  let ena = 1
  let ( ! ) n = n
  let izpis n = string_of_int n
end

module RealnaStevila : KOLOBAR = struct
  type elt = float

  let ( + ) = ( +. )
  let ( - ) = ( -. )
  let ( * ) = ( *. )
  let nic = 0.
  let ena = 1.
  let ( ! ) n = float_of_int n
  let izpis x = string_of_float x
end

module type POLINOM = sig
  type polinom
  type kolobar

  val vrednost : polinom -> kolobar -> kolobar
  (** [vrednost p x] po Hornerjevem algoritmu izračuna vrednost
        polinoma [p] v točki [x].
    *)

  val odvod : polinom -> polinom
  val izpis : polinom -> string
  val ( + ) : polinom -> polinom -> polinom
  val ( * ) : polinom -> polinom -> polinom
  val x : polinom
  val ( !! ) : int -> polinom
  val ( ! ) : kolobar -> polinom
  val ( ** ) : polinom -> int -> polinom
  val iz_koeficientov : kolobar list -> polinom
  val v_koeficiente : polinom -> kolobar list
end

module Polinom (K : KOLOBAR) : POLINOM = struct
  type polinom = K.elt list
  type kolobar = K.elt

  let stevke b n =
    let rec aux acc n = if n = 0 then acc else aux ((n mod b) :: acc) (n / b) in
    aux [] n

  let rec drop_while p = function
    | x :: xs when p x -> drop_while p xs
    | xs -> xs

  let filter_mapi f xs =
    let rec aux i = function
      | [] -> []
      | x :: xs -> (
          match f i x with
          | None -> aux (i + 1) xs
          | Some y -> y :: aux (i + 1) xs)
    in
    aux 0 xs

  let pocisti p =
    List.fold_right
      (fun x acc ->
        match (x, acc) with a, [] when a = K.nic -> [] | _ -> x :: acc)
      p []

  let sestej p1 p2 =
    let rec aux = function
      | [], p2 -> p2
      | p1, [] -> p1
      | a1 :: p1, a2 :: p2 -> K.(a1 + a2) :: aux (p1, p2)
    in
    aux (p1, p2) |> pocisti

  let zmnozi p1 p2 =
    p1
    |> List.mapi (fun k a ->
           List.init k (fun _ -> K.nic) @ List.map (K.( * ) a) p2)
    |> List.fold_left sestej []

  let rec potenca p = (* uporabimo hitro potenciranje *)
    function
    | 0 -> [ K.ena ] (* p⁰ = 1 *)
    | n ->
        let p2 = zmnozi p p in
        if n mod 2 = 0 then potenca p2 (n / 2) (* p²ⁿ = (p²)ⁿ *)
        else zmnozi (potenca p2 (n / 2)) p (* p²ⁿ⁺¹ = (p²)ⁿ ⋅ p *)

  let vrednost p x = List.fold_right (fun a v -> K.((v * x) + a)) p K.nic

  let odvod = function
    | [] -> []
    | _ :: p -> List.mapi (fun k a -> K.(a * (!k + !1))) p

  let izpis p =
    let potenca k =
      stevke 10 k
      |> List.map
           (Array.get [| "⁰"; "¹"; "²"; "³"; "⁴"; "⁵"; "⁶"; "⁷"; "⁸"; "⁹" |])
      |> String.concat ""
    in
    let clen k a =
      let predznak, abs_a =
        if a >= K.nic then ("+", a) else ("-", K.(nic - a))
      in
      let monom =
        match k with
        | 0 -> K.izpis abs_a
        | 1 when abs_a = K.ena -> "x"
        | 1 -> K.izpis abs_a ^ " x"
        | k when abs_a = K.ena -> "x" ^ potenca k
        | k -> K.izpis abs_a ^ " x" ^ potenca k
      in
      if a = K.nic then None else Some (predznak, monom)
    in
    let cleni = p |> filter_mapi clen |> List.rev in
    match cleni with
    | [] -> "0"
    | (predznak, monom) :: cleni ->
        let vodilni = if predznak = "+" then monom else "-" ^ monom in
        let ostali =
          List.map (fun (predznak, monom) -> predznak ^ " " ^ monom) cleni
        in
        String.concat " " (vodilni :: ostali)

  let ( + ) = sestej
  let ( * ) = zmnozi
  let x = [ K.nic; K.ena ]
  let ena = [ 1 ]
  let ( ** ) = potenca
  let iz_koeficientov p = p
  let v_koeficiente p = p
  let ( ! ) n = [ n ]
  let ( !! ) n = [ K.( ! ) n ]
end

module CelostevilskiPolinom : POLINOM = Polinom (CelaStevila)
module RealniPolinom : POLINOM = Polinom (RealnaStevila)

let _ = CelostevilskiPolinom.((x + !!1) ** 3 |> odvod |> izpis |> print_endline)
let _ = RealniPolinom.((x + !!1) ** 3 |> odvod |> izpis |> print_endline)
