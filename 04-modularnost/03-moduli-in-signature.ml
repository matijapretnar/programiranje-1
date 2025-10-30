module type POLINOM = sig
  type polinom = int list

  val vrednost : polinom -> int -> int
  (** [vrednost p x] po Hornerjevem algoritmu izračuna vrednost
        polinoma [p] v točki [x].
    *)

  val odvod : polinom -> polinom
  val izpis : polinom -> string
  val ( + ) : polinom -> polinom -> polinom
  val ( * ) : polinom -> polinom -> polinom
  val x : polinom
  val ena : polinom
  val ( ** ) : polinom -> int -> polinom
end

module Polinom : POLINOM = struct
  type polinom = int list

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
      (fun x acc -> match (x, acc) with 0, [] -> [] | _ -> x :: acc)
      p []

  let sestej p1 p2 =
    let rec aux = function
      | [], p2 -> p2
      | p1, [] -> p1
      | a1 :: p1, a2 :: p2 -> (a1 + a2) :: aux (p1, p2)
    in
    aux (p1, p2) |> pocisti

  let zmnozi p1 p2 =
    p1
    |> List.mapi (fun k a -> List.init k (fun _ -> 0) @ List.map (( * ) a) p2)
    |> List.fold_left sestej []

  let rec potenca p = (* uporabimo hitro potenciranje *)
    function
    | 0 -> [ 1 ] (* p⁰ = 1 *)
    | n ->
        let p2 = zmnozi p p in
        if n mod 2 = 0 then potenca p2 (n / 2) (* p²ⁿ = (p²)ⁿ *)
        else zmnozi (potenca p2 (n / 2)) p (* p²ⁿ⁺¹ = (p²)ⁿ ⋅ p *)

  let vrednost p x = List.fold_right (fun a v -> (v * x) + a) p 0

  let odvod = function
    | [] -> []
    | _ :: p -> List.mapi (fun k a -> a * (k + 1)) p

  let izpis p =
    let potenca k =
      stevke 10 k
      |> List.map
           (Array.get [| "⁰"; "¹"; "²"; "³"; "⁴"; "⁵"; "⁶"; "⁷"; "⁸"; "⁹" |])
      |> String.concat ""
    in
    let clen k a =
      let predznak = if a >= 0 then "+" else "-" in
      let monom =
        match (abs a, k) with
        | a, 0 -> string_of_int a
        | 1, 1 -> "x"
        | a, 1 -> string_of_int a ^ " x"
        | 1, k -> "x" ^ potenca k
        | a, k -> string_of_int a ^ " x" ^ potenca k
      in
      if a = 0 then None else Some (predznak, monom)
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
  let x = [ 0; 1 ]
  let ena = [ 1 ]
  let ( ** ) = potenca
end

let _ =
  let x = [ 0; 1 ] in
  let x_plus_ena = Polinom.( + ) x [ 1 ] in
  Polinom.( ** ) x_plus_ena 3 |> Polinom.odvod |> Polinom.izpis |> print_endline

let _ =
  let open Polinom in
  let x = [ 0; 1 ] in
  let x_plus_ena = x + [ 1 ] in
  x_plus_ena ** 3 |> odvod |> izpis |> print_endline

let _ = Polinom.((x + ena) ** 3 |> odvod |> izpis |> print_endline)
