let rec fakulteta n =
    if n = 0 then 1 else n * fakulteta (n - 1)

let fakulteta' n =
    let rec aux acc n =
        if n = 0 then acc else aux (n * acc) (n - 1)
    in
    aux 1 n

(* 
fakulteta 10 =
10 * fakulteta 9 =
10 * (9 * fakulteta 8) =
 *)

(* 
fakulteta' 1 10 =
fakulteta' 10 9 =
fakulteta' 90 8 =
fakulteta' 720 7 =
fakulteta' 5040 6 =
fakulteta' 5040 6 =
 *)

let rec dolzina sez =
    match sez with
    | [] -> 0
    | glava :: rep -> 1 + dolzina rep

let rec dolzina sez =
    let rec aux acc sez =
        match sez with
        | [] -> acc
        | glava :: rep -> aux (1 + acc) rep
    in
    aux 0 sez
