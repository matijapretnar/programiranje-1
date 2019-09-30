let rec print_all l =
  match l with
  | [] -> ()
  | hd::tl -> print_int hd; print_all tl


let rec map2_opt l1 l2 f =
  let rec aux l1 l2 f acc =
    match l1, l2 with
    | [], [] -> Some (List.rev acc)
    | [], _ -> None
    | _, [] -> None
    | x::xs, y::ys ->
      let a = f x y in
      aux xs ys f (a::acc)
  in
  aux l1 l2 f []
