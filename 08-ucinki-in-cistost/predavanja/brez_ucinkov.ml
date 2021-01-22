let f sez =
    let sez = 1 :: sez in
    List.length sez

let g sez =
    f sez + f sez
