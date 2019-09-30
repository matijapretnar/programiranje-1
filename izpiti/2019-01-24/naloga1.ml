let podvoji_vsoto x y = 2 * (x + y)

let povsod_vecji (a, b, c) (x, y, z) = a > x && b > y && c > z

let uporabi_ce_lahko f = function
  | None -> None
  | Some x -> Some (f x)

let rec pojavi_dvakrat x lst =
  let rec prestej = function
  | [] -> 0
  | y :: ys -> if x = y then 1 + prestej ys else prestej ys
  in
  prestej lst = 2

let izracunaj_v_tocki x f_lst =
  let rec racunaj acc = function
    | [] -> List.rev acc
    | f :: fs -> racunaj (f x :: acc) fs
  in
  racunaj [] f_lst

let rec eksponent x pow = 
  let rec exp x pow acc =
    if pow <= 0 then acc else exp x (pow - 1) (x * acc)
  in
  exp x pow 1
