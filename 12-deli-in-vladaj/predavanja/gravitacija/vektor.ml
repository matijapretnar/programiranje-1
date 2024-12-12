type t = { x : float; y : float }

let ( **. ) k vec = { x = k *. vec.x; y = k *. vec.y }
let ( ++. ) vec1 vec2 = { x = vec1.x +. vec2.x; y = vec1.y +. vec2.y }
let ( --. ) vec1 vec2 = vec1 ++. (-1. **. vec2)
let dolzina vec = sqrt ((vec.x ** 2.) +. (vec.y ** 2.))
let normiraj vec = (1. /. dolzina vec) **. vec

let sredina ?(lambda = 0.5) zacetek konec =
  ((1. -. lambda) **. zacetek) ++. (lambda **. konec)

let razdalja zacetek konec = dolzina (zacetek --. konec)
let izhodisce = { x = 0.; y = 0. }

let koreni_enote n =
  let pi = 4. *. atan 1. in
  List.init n (fun i ->
      let kot = 2. *. pi *. float_of_int i /. float_of_int n in
      { x = cos kot; y = sin kot })
