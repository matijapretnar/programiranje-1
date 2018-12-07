type ('k, 'v) t =
    | Prazno
    | Sestavljeno of {
        visina : int;
        levo : ('k, 'v) t;
        kljuc : 'k;
        vrednost : 'v;
        desno : ('k, 'v) t
    }

let prazen = Prazno

let rec poisci kljuc = function
  | Prazno -> None
  | Sestavljeno drevo ->
      if kljuc < drevo.kljuc then
        poisci kljuc drevo.levo
      else if drevo.kljuc < kljuc then
        poisci kljuc drevo.desno
      else
        Some drevo.vrednost

let rec visina = function
  | Prazno -> 0
  | Sestavljeno drevo -> drevo.visina

let sestavi l k v d =
  let h = 1 + max (visina l) (visina d) in
  Sestavljeno {
    visina = h;
    levo = l;
    kljuc = k;
    vrednost = v;
    desno = d
  }

let zavrti_levo = function
  | Sestavljeno ({desno = Sestavljeno desno} as drevo) ->
      let novo_levo = sestavi drevo.levo drevo.kljuc drevo.vrednost desno.levo in
      sestavi novo_levo desno.kljuc desno.vrednost desno.desno
  | _ -> assert false

let zavrti_desno = function
  | Sestavljeno ({levo = Sestavljeno levo} as drevo) ->
      let novo_desno = sestavi levo.desno drevo.kljuc drevo.vrednost drevo.desno in
      sestavi levo.levo levo.kljuc levo.vrednost novo_desno
  | _ -> assert false

let razlika = function
  | Prazno -> 0
  | Sestavljeno drevo -> visina drevo.levo - visina drevo.desno

let uravnotezi drevo =
  match drevo with
  | Prazno -> Prazno
  | Sestavljeno {levo} when razlika drevo = 2 && razlika levo = 1 ->
      zavrti_desno drevo
  | Sestavljeno {levo; kljuc; vrednost; desno} when razlika drevo = 2  ->
      sestavi
        (zavrti_levo levo)
        kljuc
        vrednost
        desno
      |> zavrti_desno
  | Sestavljeno {desno} when razlika drevo = -2 && razlika desno = -1 ->
      zavrti_levo drevo
  | Sestavljeno {levo; kljuc; vrednost; desno} when razlika drevo = -2 ->
      sestavi
        levo
        kljuc
        vrednost
        (zavrti_desno desno)
      |> zavrti_levo
  | _ -> drevo

let rec dodaj kljuc vrednost = function
  | Prazno -> sestavi Prazno kljuc vrednost Prazno
  | Sestavljeno drevo ->
      if kljuc < drevo.kljuc then
        sestavi
          (dodaj kljuc vrednost drevo.levo)
          drevo.kljuc
          drevo.vrednost
          drevo.desno
        |> uravnotezi
      else if drevo.kljuc < kljuc then
        sestavi
          drevo.levo
          drevo.kljuc
          drevo.vrednost
          (dodaj kljuc vrednost drevo.desno)
        |> uravnotezi
      else
        Sestavljeno drevo
