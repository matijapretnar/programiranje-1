let opt_sum o1 o2 =
  match o1, o2 with
  | Some a, Some b -> Some (a + b)
  | _ -> None

let strange_map f l r x = let (a, b) = f x in (l a, r b)

let rec function_repeat f list =
  let rec repeater x acc n = 
    if n > 0 then repeater x (x::acc) (n-1) else acc 
  in
  let rec extender acc = function
    | [] -> List.rev acc
    | x :: xs -> extender (repeater x acc (f x)) xs
  in
  extender [] list

let rec iterate f cond x =
  print_float x;
  print_newline ();
  if cond x then 
    x
  else
    iterate f cond (f x)