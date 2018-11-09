let rec vsota sez =
    match sez with
    | [] -> 0
    | glava :: rep -> glava + vsota rep

let rec dolzina = function
    | [] -> 0
    | _ :: rep -> 1 + dolzina rep

let rec preslikaj f sez =
    match sez with
    | [] -> []
    | glava :: rep -> f glava :: preslikaj f rep

let repno_rekurzivna_dolzina sez =
    let rec dolzina' acc = function
        | [] -> acc
        | _ :: rep -> dolzina' (acc + 1) rep
    in
    dolzina' 0 sez

let repno_rekurzivna_vsota sez =
    let rec vsota' acc = function
        | [] -> acc
        | glava :: rep -> vsota' (acc + glava) rep
    in
    vsota' 0 sez

let obrni sez =
    let rec obrni' acc = function
    | [] -> acc
    | glava :: rep -> obrni' (glava :: acc) rep
    in
    obrni' [] sez


let repno_rekurzivni_preslikaj f sez =
    let rec preslikaj' acc = function
        | [] -> obrni acc
        | glava :: rep -> preslikaj' (f glava :: acc) rep
    in
    preslikaj' [] sez
