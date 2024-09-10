(* 1. a *)
(*----------------------------------------------------------------------------*]
  Napišite predikat `obrnljiva : (int * int) * (int * int) -> bool`, ki za dano 
  dvodimenzionalno matriko $A$ pove, ali je obrnljiva, torej da njena determinanta
  ni enaka 0. (Determinanta matrike ((a, b), (c, d)) je enaka ad - bc.)

  # obrnljiva ((3, 4), (0, 0));;
  - : bool = false
  # obrnljiva ((1, 0), (0, 1));;
  - : bool = true
[*----------------------------------------------------------------------------*)

let obrnljiva ((a, b), (c, d)) = a * d - b * c <> 0

(* b *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo `natanko_en : int option -> int option -> int option`, 
  ki sprejme morebitna elementa ter vrne element natanko tedaj,
  ko dobi ravno enega.
  
    # natanko_en (Some 1) (Some 2);;
    - : int option = None
    # natanko_en (Some 1) None;;
    - : int option = Some 1
    # natanko_en None (Some 2);;
    - : int option = Some 2
    # natanko_en None None;;
    - : int option = None
[*----------------------------------------------------------------------------*)

let natanko_en a b = 
  match a, b with
  | Some _, Some _ -> None
  | Some x, None -> Some x
  | None, Some x -> Some x
  | None, None -> None

(* c *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo `aritmeticno : int list -> int option`, ki pove, ali je 
  zaporedje števil aritmetično. Če je, vrne razliko, sicer pa `None`. 
  Zaporedja z enim elementom ali manj niso aritmetična.

    # aritmeticno [];;
    - : int option = None
    # aritmeticno [1; 5; 9];;
    - : int option = Some 4
    # aritmeticno [3; 4; 5; 2];;
    - : int option = None
[*----------------------------------------------------------------------------*)

let aritmeticno = function
  | [] | _ :: [] -> None
  | x :: y :: zs -> 
    let razlika = y - x in
    let rec aux = function
      | x :: y :: [] when y - x = razlika -> Some razlika
      | x :: y :: zs when y - x = razlika -> aux (y :: zs)
      | _ -> None
    in
    aux (y :: zs)

(* d *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo `filtriraj : 'a list -> bool list -> 'a list`, ki sprejme
  seznam elementov in seznam logičnih vrednosti. Vrne naj seznam tistih elementov
  prvega seznama, pri katerih se v drugem seznamu na istem mestu nahaja `true`.
  Funkcija naj ignorira logične vrednosti na koncu seznama, če jih je preveč.
  Če je seznam logičnih vrednosti prekratek, naj se obnaša, kot da so manjkajoče 
  vrednosti `true`.

    # filtriraj [1; 2; 3; 4; 5] [true; false; true; false; true];;
    - : int list = [1; 3; 5]
    # filtriraj [1; 2] [false; true; false; true];;
    - : int list = [2]
    # filtriraj [1; 2; 3; 4; 5] [false];;
    - : int list = [2; 3; 4; 5]
[*----------------------------------------------------------------------------*)

let rec filtriraj xs bs = 
  match xs, bs with
  | [], [] | [], _ -> []
  | xs, [] -> xs
  | x :: xs, b :: bs -> if b then x :: (filtriraj xs bs) else filtriraj xs bs