module IskalnoDrevo = struct
    type 'a t =
    | Prazno
    | Sestavljeno of int * 'a t * 'a * 'a t

    let prazna = Prazno

    let visina = function
      | Prazno -> 0
      | Sestavljeno (h, _, _, _) -> h

    let sestavljeno (l, x, d) =
        let h = 1 + max (visina l) (visina d) in
        Sestavljeno (h, l, x, d)

    let rec velikost = function
      | Prazno -> 0
      | Sestavljeno (_, l, _, d) -> 1 + velikost l + velikost d

    let rec vsebuje x =
      function
      | Prazno -> false
      | Sestavljeno (_, l, y, d) when x = y -> true
      | Sestavljeno (_, l, y, d) ->
          if x < y then vsebuje x l else vsebuje x d

    let razlika = function
      | Prazno -> 0
      | Sestavljeno (_, l, _, d) -> visina l - visina d

    let zavrti_levo = function
      | Sestavljeno (_, l, x, Sestavljeno (_, dl, y, dd)) ->
          sestavljeno (sestavljeno (l, x, dl), y, dd)
      | _ -> failwith "Tega drevesa ne morem zavrteti"

    let zavrti_desno = function
      | Sestavljeno (_, Sestavljeno (_, ll, y, ld), x, d) ->
          sestavljeno (ll, y, sestavljeno (ld, x, d))
      | _ -> failwith "Tega drevesa ne morem zavrteti"

    let uravnotezi drevo =
      match drevo with
      | Sestavljeno (_, l, _, _) when razlika drevo = 2 && razlika l = 1 ->
          zavrti_desno drevo
      | Sestavljeno (_, l, x, d) when razlika drevo = 2 ->
          sestavljeno (zavrti_levo l, x, d) |> zavrti_desno
      | Sestavljeno (_, _, _, d) when razlika drevo = -2 && razlika d = -1 ->
          zavrti_levo drevo
      | Sestavljeno (_, l, x, d) when razlika drevo = -2 ->
          sestavljeno (l, x, zavrti_desno d) |> zavrti_levo
      | _ -> drevo

    let rec dodaj x drevo =
      match drevo with
      | Prazno -> sestavljeno (Prazno, x, Prazno)
      | Sestavljeno (_, l, y, d) ->
          if x < y then sestavljeno (dodaj x l, y, d) |> uravnotezi
          else if x > y then sestavljeno (l, y, dodaj x d) |> uravnotezi
          else drevo
  end