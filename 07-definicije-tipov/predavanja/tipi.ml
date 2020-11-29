type r3 = float * float * float

let vsota ((x1, y1, z1) : r3) ((x2, y2, z2) : r3) : r3 =
  (x1 +. x2, y1 +. y2, z1 +. z2)

type kompleksno = float * float

let absolutna_vrednost (z : kompleksno) : float =
  let (re, im) = z in
  (re ** 2. +. im ** 2.) ** 0.5
  
let absolutna_vrednost (z : kompleksno) : float =
  let (abs, arg) = z in
  abs

type kompleksno' = {
  re : float;
  im : float
}

type kompleksno'' = {
  abs : float;
  arg : float
}

let absolutna_vrednost (z : kompleksno') : float =
  (z.re ** 2. +. z.im ** 2.) ** 0.5

let konjugiraj z =
  { re = z.re ; im = -.z.im}

let absolutna_vrednost (z : kompleksno'') : float =
  z.abs
