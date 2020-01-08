(* a *)
let option_sum opt1 opt2 =
  match opt1, opt2 with
  | Some a, Some b -> Some (a + b)
  | _ -> None

(* b *)
let twostep_map f l r x = 
  let (a, b) = f x in 
  (l a, r b)

(* c *)
let rec function_repeat f list =
  let rec extender x acc n = 
    if n > 0 then extender x (x::acc) (n-1) else acc 
  in
  let rec repeater acc = function
    | [] -> List.rev acc
    | x :: xs -> repeater (extender x acc (f x)) xs
  in
  repeater [] list

(* d *)
let rec iterate f cond x =
  if cond x then 
    x
  else
    iterate f cond (f x)