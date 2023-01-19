type ('kljuc, 'vrednost) slovar = ('kljuc * 'vrednost) list

type kompleksno = {
  re : float;
  im : float
}

type polarno_kompleksno = {
  r : float;
  arg : float
}

type student = {
  ime : string;
  priimek : string;
  vpisna : int
}

let i = { re = 0.; im = 1. }

let abs z =
  (z.re ** 2. +. z.im ** 2.) ** 0.5

let abs' {re = x; im = y} =
  (x ** 2. +. y ** 2.) ** 0.5

let abs'' {r = x; arg = _} = x

let abs''' {re; im} =
  (re ** 2. +. im ** 2.) ** 0.5

let abs'''' {r; _} = r

let abs''''' {r} = r

let konj z = {re = z.re; im = -. z.im}

let konj' z = {z with im = -. z.im}
