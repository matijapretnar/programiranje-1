type 'a neprazen_sez = Konec of 'a | Sestavljen of 'a * 'a neprazen_sez

let prvi = function Konec x -> x | Sestavljen (x, _) -> x
let rec zadnji = function Konec x -> x | Sestavljen (_, t) -> zadnji t

let rec dolzina = function
  | Konec _ -> 1
  | Sestavljen (_, t) -> 1 + dolzina t

let rec pretvori_v_seznam = function
  | Konec x -> [x]
  | Sestavljen (x, xs) -> x :: (pretvori_v_seznam xs)

let rec zlozi f s = function
  | Konec x -> f s x
  | Sestavljen (x, l) -> zlozi f (f s x) l
