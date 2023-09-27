type 'a drevo =
  | List
  | Koren of 'a drevo * 'a * 'a drevo

type naravno =
  | Nic
  | Naslednik of naravno

let rec vsota m n =
  match m with
  | Nic -> n
  | Naslednik m' -> Naslednik (vsota m' n)

type 'a seznam =
  | Prazen
  | Sestavljen of 'a * 'a seznam
