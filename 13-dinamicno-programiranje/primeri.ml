module type SLOVAR = sig
  type ('k, 'v) t

  val poisci : ('k, 'v) t -> 'k -> 'v option
  val prazen : ('k, 'v) t
  val dodaj : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
end

module Slovar : SLOVAR = struct
  type ('k, 'v) t = Prazno | Sestavljeno of int * ('k, 'v) t * 'k * 'v * ('k, 'v) t

  let rec poisci sl k =
    match sl with
    | Prazno -> None
    | Sestavljeno (_, l, k', v, d) when k = k' -> Some v
    | Sestavljeno (_, l, k', _, d) when k < k' -> poisci l k
    | Sestavljeno (_, l, k', _, d) when k > k' -> poisci d k
    | _ -> assert false

  let prazen = Prazno

  let visina drevo =
    match drevo with
    | Prazno -> 0
    | Sestavljeno (h, _, _, _, _) -> h

  let sestavljeno (l, k, v, d) =
    Sestavljeno (1 + max (visina l) (visina d), l, k, v, d)

  let zavrti_levo = function
    | Sestavljeno (_, l, k, v, Sestavljeno (_, dl, k', v', dd)) ->
        sestavljeno (sestavljeno (l, k, v, dl), k', v', dd)
    | _ -> failwith "Tega drevesa ne morem zavrteti"

  let zavrti_desno = function
    | Sestavljeno (_, Sestavljeno (_, ll, k, v, ld), k', v', d) ->
        sestavljeno (ll, k, v, sestavljeno (ld, k', v', d))
    | _ -> failwith "Tega drevesa ne morem zavrteti"

  let razlika = function
    | Prazno -> 0
    | Sestavljeno (_, l, _, _, d) -> visina l - visina d

  let uravnotezi drevo =
    match drevo with
    | Sestavljeno (_, l, k, v, d) when razlika drevo = 2 && razlika l = 1 ->
        zavrti_desno drevo
    | Sestavljeno (_, l, k, v, d) when razlika drevo = 2 ->
        sestavljeno (zavrti_levo l, k, v, d) |> zavrti_desno
    | Sestavljeno (_, l, k, v, d) when razlika drevo = -2 && razlika d = -1 ->
        zavrti_levo drevo
    | Sestavljeno (_, l, k, v, d) when razlika drevo = -2 ->
        sestavljeno (l, k, v, zavrti_desno d) |> zavrti_levo
    | _ -> drevo

  let rec dodaj k v drevo =
    match drevo with
    | Prazno -> Sestavljeno (1, Prazno, k, v, Prazno)
    | Sestavljeno (h, l, k', v', d) ->
        if k < k' then sestavljeno (dodaj k v l, k', v', d) |> uravnotezi
        else if k > k' then
          sestavljeno (l, k', v', dodaj k v d) |> uravnotezi
        else
          sestavljeno (l, k', v, d)
end

let rec stolpi =
  function
  | 0 -> 1
  | n when n < 0 -> 0
  | n -> stolpi (n - 1) + stolpi (n - 2) + stolpi (n - 3)

let rec stolpi_s_slovarjem s n =
  match Slovar.poisci s n with
  | Some x -> x, s
  | None ->
    match n with
    | 0 -> 1, s
    | n when n < 0 -> 0, s
    | n ->
        let x1, s1 = stolpi_s_slovarjem s (n - 1) in
        let x2, s2 = stolpi_s_slovarjem s1 (n - 2) in
        let x3, s3 = stolpi_s_slovarjem s2 (n - 3) in
        let y = x1 + x2 + x3 in
        y, Slovar.dodaj n y s3

let rec stolpi_z_referenco r n =
  match Slovar.poisci !r n with
  | Some x -> x
  | None ->
    match n with
    | 0 -> 1
    | n when n < 0 -> 0
    | n ->
        let y = stolpi_z_referenco r (n - 1) + stolpi_z_referenco r (n - 2) + stolpi_z_referenco r (n - 3) in
        r := Slovar.dodaj n y !r;
        y
  

let rec stolpi_s_slovarjem s =
  function
  | 0 -> 1, s
  | n when n < 0 -> 0, s
  | n ->
      let x1, s1 = stolpi_s_slovarjem s (n - 1) in
      let x2, s2 = stolpi_s_slovarjem s1 (n - 2) in
      let x3, s3 = stolpi_s_slovarjem s2 (n - 3) in
      let y = x1 + x2 + x3 in
      y, Slovar.dodaj n y s3
(* def stolpi(n, ze_izracunane_vrednosti={}):
    if n in ze_izracunane_vrednosti:
        return ze_izracunane_vrednosti[n]
    else:
      print(n)
      if n < 0:
          s = 0
      elif n == 0:
          s = 1
      else:
          s = stolpi(n - 1) + stolpi(n - 2) + stolpi(n - 3)
      ze_izracunane_vrednosti[n] = s
      return s *)