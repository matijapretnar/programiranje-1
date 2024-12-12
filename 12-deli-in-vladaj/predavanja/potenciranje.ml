let rec potenciraj m =
  function
  | 0 -> 1
  | n -> m * potenciraj m (n - 1)

let rec hitro_potenciraj m =
  function
  | 0 -> 1
  | n when n mod 2 = 0 ->
      let k = hitro_potenciraj m (n / 2) in
      k * k
  | n (* when n mod 2 = 1 *) ->
      let k = hitro_potenciraj m (n / 2) in
      m * k * k