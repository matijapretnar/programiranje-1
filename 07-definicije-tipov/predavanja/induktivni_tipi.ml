type naravno =
  | Nic
  | Naslednik of naravno

let rec vsota m n =
  match n with
  | Nic -> m
  | Naslednik n' -> Naslednik (vsota m n')


type 'a seznam =
  | Prazen
  | Sestavljen of 'a * 'a seznam

let rec stakni sez1 sez2 =
  match sez1 with
  | Prazen -> sez2
  | Sestavljen (glava1, rep1) -> Sestavljen (glava1, stakni rep1 sez2)

type 'a drevo =
  | Prazno
  | Sestavljeno of 'a drevo * 'a * 'a drevo

let list x = Sestavljeno (Prazno, x, Prazno)

let testno_drevo = Sestavljeno (list 4, 5, Sestavljeno(list 3, 10, Prazno))

let rec prezrcali = function
  | Prazno -> Prazno
  | Sestavljeno (l, x, d) -> Sestavljeno (prezrcali d, x, prezrcali l)

let rec elementi = function
  | Prazno -> []
  | Sestavljeno (l, x, d) -> elementi l @ [x] @ elementi d
