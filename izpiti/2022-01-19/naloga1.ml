let sta_pravokotna (x1, y1, z1) (x2, y2, z2) =
  (x1 * x2) + (y1 * y2) + (z1 * z2) = 0

let postkompozicija f g x = g (f x)

let dopolni privzeta_vrednost seznam =
  seznam |> List.rev_map (Option.value ~default:privzeta_vrednost) |> List.rev

let pretvori baza stevke =
  let _, stevilo =
    List.fold_right
      (fun stevka (potenca, stevilo) ->
        (baza * potenca, (stevka * potenca) + stevilo))
      stevke (1, 0)
  in
  stevilo
