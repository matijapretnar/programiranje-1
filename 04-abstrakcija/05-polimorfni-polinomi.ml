module type KOLOBAR = sig
  type elt

  val ( + ) : elt -> elt -> elt
  val ( * ) : elt -> elt -> elt
  val nic : elt
end

module CelaStevila : KOLOBAR = struct
  type elt = int
  let ( + ) = ( + )
  let ( * ) = ( * )
  let nic = 0
end

module RealnaStevila : KOLOBAR = struct
  type elt = float
  let ( + ) = ( +. )
  let ( * ) = ( *. )
  let nic = 0.
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
    val ( ! ) : kolobar -> polinom
    val ( ** ) : polinom -> int -> polinom
    val iz_koeficientov : kolobar list -> polinom
    val v_koeficiente : polinom -> kolobar list
  end


module CelostevilskiPolinom : POLINOM = struct
  type polinom = int list
  type kolobar = int

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
  let iz_koeficientov p = p
  let v_koeficiente p = p

  let (!) n = [n]
end

let _ =
  let open CelostevilskiPolinom in
  x ** 3 |> odvod |> izpis |> print_endline

let _ = CelostevilskiPolinom.((x) ** 3 |> odvod |> izpis |> print_endline)


module PolinomNadDanimKolobarjem (K : KOLOBAR) : POLINOM = struct
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

  let vrednost p x = List.fold_right (fun a v -> K.((v * x) + a)) p K.nic

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
  let iz_koeficientov p = p
  let v_koeficiente p = p

  let (!) n = [n]
end