type oblika_drevesa =
  | Prazno
  | Sestavljeno of oblika_drevesa * oblika_drevesa

type drevo_celih_stevil =
  | Prazno
  | Sestavljeno of drevo_celih_stevil * int * drevo_celih_stevil

type drevo_booleovih_vrednosti =
  | Prazno
  | Sestavljeno of drevo_booleovih_vrednosti * bool * drevo_booleovih_vrednosti

type 'a drevo =
  | Prazno
  | Sestavljeno of 'a drevo * 'a * 'a drevo

let pozagaj _ = Prazno

let rec stevilo_elementov = function
  | Prazno -> 0
  | Sestavljeno (l, _, d) -> 1 + stevilo_elementov l + stevilo_elementov d

let rec seznam_elementov = function
  | Prazno -> []
  | Sestavljeno (l, x, d) -> seznam_elementov l @ [x] @ seznam_elementov d
