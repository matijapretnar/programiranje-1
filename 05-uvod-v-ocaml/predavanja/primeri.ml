let x = 1 + 1

let podvoji x = 2 * x

let zmnozi x y = x *. y

let razdalja (x1, y1) (x2, y2) =
    let dx = x1 -. x2
    and dy = y1 -. y2 in
    (dx ** 2.0 +. dy ** 2.0) ** 0.5

let pozdrav ime =
    if ime = "Matija" then
        "Pozdravljeni, gospod profesor!"
    else if ime = "Katja" || ime = "Filip" then
        "Dober dan, asistent!"
    else
        "Živjo, " ^ ime ^ "!"

let pozdrav' ime =
    match ime with
    | "Matija" -> "Pozdravljeni, gospod profesor!"
    | "Katja" | "Filip" -> "Dober dan!"
    | _ -> "Živjo, " ^ ime ^ "!"
   
let pozdrav'' =
    function
    | "Matija" -> "Pozdravljeni, gospod profesor!"
    | "Katja" | "Filip" -> "Dober dan!"
    | "" -> "Oj!"
    | "*" -> "Živjo zvezdica!"
    | ime -> "Živjo, " ^ ime ^ "!"

let je_seznam_skoraj_prazen sez =
    match sez with
    | [] -> true
    | 0 :: [] -> true
    | 0 :: 0 :: [] -> true
    | x :: [] -> true
    | _ :: _ -> false

let rec vsota sez =
    match sez with
    | [] -> 0
    | glava :: rep -> glava + vsota rep

(* 
dolzina : int list -> int
dolzina : bool list -> int
dolzina : string list -> int
dolzina : (string list) list -> int
...
dolzina : ∀α. α list -> int
dolzina : 'a list -> int

*)

let rec dolzina sez =
    match sez with
    | [] -> 0
    | _ :: rep -> 1 + dolzina rep

(* 
    map : ('a -> 'b) -> 'a list -> 'b list
*)

let rec map f sez =
    match sez with
    | [] -> []
    | glava :: rep -> f glava :: map f rep


let rec filter p =
    function
    | [] -> []
    | glava :: rep ->
        let rep' = filter p rep in
        if p glava then glava :: rep' else rep'

let rec (@) xs ys =
    match xs with
    | [] -> ys
    | x :: xs' -> x :: (xs' @ ys)
