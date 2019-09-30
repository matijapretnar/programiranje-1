let uporabi f x = f x

let ibaropu x f = f x

let rec zacetnih n xs =
  if n <= 0 then
    Some []
  else
    match xs with
    | [] -> None
    | x::xs ->
      begin match zacetnih (n-1) xs with
       | None -> None
       | Some t -> Some (x :: t)
      end

let zacetnih_tailrec n xs =
  let rec aux n acc xs =
    if n <= 0 then
      Some (List.rev acc)
    else
      match xs with
      | [] -> None
      | x::xs -> aux (n-1) (x::acc) xs
  in
  aux n [] xs
