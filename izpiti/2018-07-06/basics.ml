let apply f x = f x

let revapply x f = f x

let rec drop n xs =
  if n <= 0 then
    Some xs
  else
    match xs with
    | [] -> None
    | _ :: xs -> drop (n-1) xs

let rec take n xs =
  if n <= 0 then
    Some []
  else
    match xs with
    | [] -> None
    | x::xs ->
      begin match take (n-1) xs with
       | None -> None
       | Some t -> Some (x :: t)
      end

let take_tailrec n xs =
  let rec aux n acc xs =
    if n <= 0 then
      Some (List.rev acc)
    else
      match xs with
      | [] -> None
      | x::xs -> aux (n-1) (x::acc) xs
  in
  aux n [] xs
