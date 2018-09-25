(* ==========================================================================
   NALOGA 1.1

   Definirajte funkcijo, ki sprejme seznam celih števil in zaporedoma izpiše
   vse elemente seznama in vrne unit.

   Primer: print_all [1; 2; 3; 4] = () in izpiše 1234
   ========================================================================== *)

let rec print_all l =
  match l with
  | [] -> ()
  | hd::tl -> print_int hd; print_all tl

(* ==========================================================================
   NALOGA 1.2

   Definirajte funkcijo, ki sprejme dva seznama in funkcijo dveh argumentov.
   Vrne naj seznam rezultatov, če funkcijo f zaporedoma uporabimo na elementih
   seznamov. Želimo, da funkcija deluje zgolj kadar sta oba vhodna seznama
   enake dolžine zato uporabite tip option.

   Za maksimalno število točk naj bo funkcija repno rekurzivna.

   Primer: map2_opt [1; 2; 3] [7; 5; 3] (+) = Some [8; 7; 6]
           map2_opt [1; 2; 3] [3; 2] (+) = None
   ========================================================================== *)

let rec map2_opt l1 l2 f =
  let rec aux l1 l2 f acc =
    match l1, l2 with
    | [], [] -> Some (List.rev acc)
    | [], _ -> None
    | _, [] -> None
    | x::xs, y::ys ->
      let a = f x y in
      aux xs ys f (a::acc)
  in
  aux l1 l2 f []
