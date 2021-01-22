let stakni_z_123 (sez : 'a list) =
    sez @ [1; 2; 3]

let uredi sez =
    List.sort compare sez

let uredi_po f sez =
    let primerjaj x y = compare (f x) (f y) in
    List.sort primerjaj sez

let uredi_po' f sez =
    let naredi_par i x = (f x, i, x) in
    let preslikani_sez = List.mapi naredi_par sez in
    let urejeni_pari = uredi preslikani_sez in
    let tretja_komponenta (_, _, x) = x in
    List.map tretja_komponenta urejeni_pari

let uredi_po'' f sez =
    let preslikani_sez = List.mapi (fun i x -> (f x, i, x)) sez in
    let urejeni_pari = uredi preslikani_sez in
    List.map (fun (_, _, x) -> x) urejeni_pari

(* int -> (int -> int) *)
let sestej x y = x + y

(* (int -> int) -> int *)
let izracunaj_dvakrat_na_0 f = f (f 0)

(* f''(x) - 2 f'(x) + sin(x) = 0 *)

let polinom x = 2.0 *. x ** 3. -. x +. 1.0
let poisci_niclo polinom =
   if polinom 0. = 0. then 0. else 1.

let odvod f = f
let tile_odvodi_zgoraj f = fun x -> (odvod (odvod f)) x -. 2. *. (odvod f x) +. sin x

let sestej' (x, y) = x + y

let uredi_po'' f sez =
    sez
    |> List.mapi (fun i x -> (f x, i, x))
    |> uredi
    |> List.map (fun (_, _, x) -> x)

let (+++) x y = 3 * (x + y)
let (|>) x f = f x

let (>>) f g = fun x -> x |> f |> g
let (>>) f g = fun x -> g (f x)
let (>>) f g x = g (f x)

let uredi_po'' f =
    List.mapi (fun i x -> (f x, i, x))
    >> uredi
    >> List.map (fun (_, _, x) -> x)
