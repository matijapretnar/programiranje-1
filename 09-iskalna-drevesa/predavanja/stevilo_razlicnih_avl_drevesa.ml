let zavrti_levo = function
  | Sestavljeno (l, x, Sestavljeno (dl, y, dd)) ->
      Sestavljeno (Sestavljeno (l, x, dl), y, dd)
  | _ -> failwith "Tega drevesa ne morem zavrteti"

let zavrti_desno = function
  | Sestavljeno (Sestavljeno (ll, y, ld), x, d) ->
      Sestavljeno (ll, y, Sestavljeno (ld, x, d))
  | _ -> failwith "Tega drevesa ne morem zavrteti"

let uravnotezi drevo =
  match drevo with
  | Prazno -> Prazno
  | Sestavljeno (l, x, d) when razlika drevo = 2 && razlika l = 1 ->
      zavrti_desno drevo
  | Sestavljeno (l, x, d) when razlika drevo = 2 ->
      Sestavljeno (zavrti_levo l, x, d) |> zavrti_desno
  | Sestavljeno (l, x, d) when razlika drevo = -2 && razlika d = -1 ->
      zavrti_levo drevo
  | Sestavljeno (l, x, d) when razlika drevo = -2 ->
      Sestavljeno (l, x, zavrti_desno d) |> zavrti_levo
  | _ -> drevo
