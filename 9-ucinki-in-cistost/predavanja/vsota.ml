let rec range a b =
  if a >= b then [] else a :: range (succ a) b
in

let ustrezen n =
  n mod 3 = 0 || n mod 5 = 0
in

for _ = 1 to 100 do 
  range 1 100000
  |> List.filter ustrezen
  |> List.length
  |> ignore
done
