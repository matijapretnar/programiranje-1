(* a *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo, ki vrne razliko med produktom in vsoto dveh celih števil.

    razlika_produkta_in_vsote : int -> int -> int

[*----------------------------------------------------------------------------*)

let razlika_produkta_in_vsote x y = (x+y) - (x*y)

(* b *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo, ki združi dva para v četverico.
    
    zlimaj_para : 'a * 'b -> 'c * 'd -> 'a * 'b * 'c * 'd

[*----------------------------------------------------------------------------*)

let zlimaj_para (x, y) (z, w) = (x, y, z, w)

(* c *)
(*----------------------------------------------------------------------------*]
  Imamo podatke tipa [int option * int option * int option], ki jih želimo
  grafično predstaviti. Napišite funkcijo [trojica_graficno], ki sprejme takšno
  trojico in vrne niz, kjer so ``manjkajoči'' elementi nadomeščeni z [-].
  Primer vrnjenega niza je ["(1, 2, -)"]

    trojica_graficno : int option * int option * int option -> string

[*----------------------------------------------------------------------------*)

let trojica_graficno (x, y, z) =
  let niz = function
    | None -> "-"
    | Some x -> string_of_int x
  in
  "(" ^ niz x ^ ", " ^ niz y ^ ", " ^ niz z ^ ")"

(* d *)
(*----------------------------------------------------------------------------*]
  Klic funkcije [nedeljivo_do x n] preveri, da število [x] ni deljivo z nobenim
  naravnim številom od 2 do vključno [n]. Število 73859 je praštevilo, torej
  mora [nedeljivo_do 73859 73858] vrniti [true].

    nedeljivo_do : int -> int -> bool

[*----------------------------------------------------------------------------*)

let rec nedeljivo_do x n =
  if n <= 1 then true else (x mod n <> 0) && undivisible_up_to_n x (n-1)

(* e *)
(*----------------------------------------------------------------------------*]
  Seznam elementov tipa ['a option] želimo razdeliti na podsezname glede na
  pojavitve vrednosti [None].

    razcepi_pri_None : 'a option list -> 'a list list.

  Kot primer, funkcija seznam

    [Some 1; None; Some 2; Some 3; None; None; Some 4; None]

  razcepi v [[1]; [2;3]; []; [4]; []]. Funkcija naj bo repno rekurzivna.
[*----------------------------------------------------------------------------*)

let razcepi_pri_None list =
  let rec razcepi acc_done acc_current = function
    | [] -> (List.rev acc_current) :: acc_done |> List.rev
    | Some x :: xs -> razcepi acc_done (x :: acc_current) xs
    | None :: xs -> razcepi ((List.rev acc_current) :: acc_done) [] xs
  in
  razcepi [] [] list
