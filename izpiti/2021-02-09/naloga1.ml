
(* a *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo, ki za trojico celih števil preveri ali tvorijo
  pitagorejsko trojico. Trojica [(a, b, c)] je pitagorejska, če je [a^2 + b^2]
  enako [c^2].

    pitagorejska_trojica : int * int * int -> bool

    # pitagorejska_trojica (3, 4, 5);;
    - : bool = true
    # pitagorejska_trojica (5, 4, 3);;
    - : bool = false

[*----------------------------------------------------------------------------*)

let pitagorejska_trojica (a, b, c) = (a*a + b*b) = c*c

(* b *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo, ki za celo število [x] vrne celo število [a], kjer velja,
  da koren števila [x] leži na intervalu [a, a+1).

    priblizek_korena : int -> int

      # priblizek_korena 9;;
    - : int = 3
    # priblizek_korena 17;;
    - : int = 4

[*----------------------------------------------------------------------------*)

let priblizek_korena x =
  let rec search y = if y * y > x then y else search (y+1) in
  search 0 - 1

(* c *)
(*----------------------------------------------------------------------------*]
  Definirajte funkcijo, ki sprejme seznam celih števil in najprej IZPIŠE vsa
  soda števila v seznamu, nato pa IZPIŠE še vsa liha števila v seznamu.
  Števila naj bodo izpisana v isti vrstici in med njimi ne želimo presledkov.

    izpisi_soda_liha : int list -> unit

    # izpisi_soda_liha [3; 1; 4; 1; 5; 9; 2];;
    4231159- : unit = ()
    # izpisi_soda_liha [2; 7; 1; 8; 2; 8; 1];;
    2828711- : unit = ()

[*----------------------------------------------------------------------------*)

let izpisi_soda_liha list =
  list |> List.filter (fun x -> x mod 2 == 0) |> List.iter print_int;
  list |> List.filter (fun x -> x mod 2 == 1) |> List.iter print_int

(* d *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo, ki sprejme seznam elementov tipa [option] in preveri, da
  si v seznamu izmenično sledijo konstruktorji [None] in [Some].

    alternirajoci_konstruktorji : 'a option list -> bool

    # alternirajoci_konstruktorji [None; Some 1; None; Some 100; None];;
    - : bool = true
    # alternirajoci_konstruktorji [None; Some 1; Some 10];;
    - : bool = false
    # alternirajoci_konstruktorji [Some 1; None; Some 10; None];;
    - : bool = true

[*----------------------------------------------------------------------------*)

let rec alternirajoci_konstruktorji = function
  | [] | _ :: [] -> true
  | Some _ :: None :: bs -> alternirajoci_konstruktorji (None :: bs)
  | None :: Some x :: bs -> alternirajoci_konstruktorji (Some x :: bs)
  | _ -> false

(* e *)
(*----------------------------------------------------------------------------*]
  Funkcija [najmanjsi_rezultat] naj za element [x] in seznam funkcij [fs] vrne
  INDEKS funkcije, ki ima pri argumentu [x] najmanjšo vrednost izmed vseh
  funkcij v seznamu [fs]. Ker je seznam morda prazen, naj bo rezultat tipa
  [option].

  Za vse točke naj bo funkcija repno rekurzivna.

    najmanjsi_rezultat : 'a -> ('a -> 'b) list -> int option

    # najmanjsi_rezultat (-10) [(fun x -> 10 * x); succ; (fun y -> -10 * y)];;  
    - : int option = Some 0
    # najmanjsi_rezultat 10 [(fun x -> 10 * x); succ; (fun y -> -10 * y)];;    
    - : int option = Some 2
    # najmanjsi_rezultat 30 [];;
    - : int option = None

[*----------------------------------------------------------------------------*)

let najmanjsi_rezultat x fs =
  let rec aux acc i fs =
    match acc, fs with
    | acc, [] -> Option.map snd acc
    | None, f :: fs -> aux (Some (f x, i)) (i+1) fs
    | Some (y, j), f :: fs -> aux (Some (min (y, j) (f x, i))) (i+1) fs
  in
  aux None 0 fs
