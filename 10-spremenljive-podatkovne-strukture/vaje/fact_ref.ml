(* A cute little example that shows how to "tie the know" to get recursive
   functions using references *)

let rec fact n = if n <= 0 then 1 else n * (fact (n-1))

let f0 = ref (fun x -> x)
let f x = if x <= 0 then 1 else x * !f0 (x-1)
let () = f0 := f

let fact_ref =
  let f0 = ref (fun x -> x) in
  let f = fun x -> if x <= 0 then 1 else x * !f0 (x-1) in
  f0 := f; f
