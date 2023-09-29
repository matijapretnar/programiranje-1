type leto = Leto of int
type visina = Visina of int
type stevilo = Stevilo of int

let ustanovljen = Leto 1859
let nadmorska = Visina 8463

let vsota = ustanovljen + nadmorska

type metri = Meter of float
type cevlji = Cevelj of float

let dolzina1 = Meter 1.
let dolzina2 = Cevelj 3.

let vsota_metrov (Meter m1) (Meter m2) = Meter (m1 +. m2)

let cevlji_v_metre (Cevelj f) = Meter (0.3 *. f)

let vsota_dolzin = vsota_metrov dolzina1 (cevlji_v_metre dolzina2)
