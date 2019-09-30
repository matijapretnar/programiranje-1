let rec odstej_trojici (x1, x2, x3) (y1, y2, y3) =
  (x1 - y1, x2 - y2, x3 - y3)

let rec max_rezultat_do_n f n =
  if n <= 0 then
    f 0
  else
    max (f n) (max_rezultat_do_n f (n-1))

let rec pocisti_seznam list =
  let rec pocisti acc = function
    | None :: xs -> pocisti acc xs
    | Some x :: xs -> pocisti (x :: acc) xs
    | [] -> List.rev acc
  in
  pocisti [] list

let rec preveri_urejenost list =
  let rec narasca = function
    | [] | _ :: [] -> true
    | x1 :: x2 :: xs -> if x1 < x2 then narasca (x2 :: xs) else false
  in
  (list |> List.filter (fun x -> x mod 2 == 0) |> narasca)
  && (list |> List.filter (fun x -> x mod 2 == 1) |> List.rev |> narasca)

