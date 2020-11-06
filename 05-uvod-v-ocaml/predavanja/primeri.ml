let podvoji x = 2 * x

(* let pomnozi_s_pi x = 3.14 * x *)

let ali_je_tri_vecje_od_stiri = 3 > 4

let najboljse_stevilo = 42 + 1

let pomnozi_med_sabo x y = x * y

let uporabi_dvakrat_na_nic f = f "niz" ^ f "niz"

let pozdravi ime =
    match ime with
    | "Matija" -> "Pozdravljeni, gospod profesor!" 
    | "Filip" | "Ziga" -> "Pozdravljeni, gospod asistent!"
    | "" -> "Pozdravljen, človek brez imena!"
    | "*" -> "Zdravo zvezdica!"
    | _ -> "Živjo, " ^ ime

let moja_logicna_operacija x y z =
    match (x, y, z) with
    | (true, false, true) -> true
    | (false, drugi, _) -> drugi
    | (true, true, _) -> true
    | (true, false, false) -> false

let gnezdeni_match x y z =
    match ((x, y), z) with
    | (_, true) -> true
    | ((true, _), tretji) -> tretji
    | (_, false) -> false

let je_seznam_prazen sez =
    match sez with
    | [] -> true
    | _ :: _ -> false

let rec vsota sez =
    match sez with
    | [] -> 0
    | glava :: rep -> glava + vsota rep

let rec dolzina sez =
    match sez with
    | [] -> 0
    | glava :: rep -> 1 + dolzina rep

let rec stakni sez1 sez2 =
    match sez1 with
    | [] -> sez2
    | glava1 :: rep1 -> glava1 :: stakni rep1 sez2

let rec preslikaj f sez =
    match sez with
    | [] -> []
    | glava :: rep -> f glava :: preslikaj f rep
