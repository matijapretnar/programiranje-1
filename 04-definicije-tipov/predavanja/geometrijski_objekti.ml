type geometrijski_objekt =
  | Tocka
  | Krog of float
  | Pravokotnik of { sirina : float; visina : float }

let ploscina =
  function
  | Tocka -> 0.
  | Krog r -> 3.14 *. r ** 2.
  | Pravokotnik {sirina; visina} -> sirina *. visina

