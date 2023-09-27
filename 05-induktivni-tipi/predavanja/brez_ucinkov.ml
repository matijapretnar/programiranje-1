let sez = [1; 2; 3]

let f x =
    let sez = x :: sez in
    List.length sez

let g x =
    f x + f x
