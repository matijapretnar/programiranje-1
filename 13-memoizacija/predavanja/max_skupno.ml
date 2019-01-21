let rec max_skupno_z_dolzino xs ys =
  match xs, ys with
  | [], _ | _, [] -> [], 0
  | x :: xs', y :: ys' when x = y ->
    let max_v_preostanku, d = max_skupno_z_dolzino xs' ys' in
    x :: max_v_preostanku, d + 1
  | x :: xs', y :: ys' ->
      let max1, d1 = max_skupno_z_dolzino xs' ys
      and max2, d2 = max_skupno_z_dolzino xs ys' in
      if d1 >= d2 then max1, d1 else max2, d2

let rec odviti_max_skupno_z_dolzino f (xs, ys) =
  match xs, ys with
  | [], _ | _, [] -> [], 0
  | x :: xs', y :: ys' when x = y ->
    let max_v_preostanku, d = f (xs', ys') in
    x :: max_v_preostanku, d + 1
  | x :: xs', y :: ys' ->
      let max1, d1 = f (xs', ys)
      and max2, d2 = f (xs, ys') in
      if d1 >= d2 then max1, d1 else max2, d2

let memoiziraj_rec odviti_f =
  let rezultati = Hashtbl.create 512 in
  let rec mem_f x =
    match Hashtbl.find_opt rezultati x with
    | None ->
        let y = odviti_f mem_f x in
        Hashtbl.add rezultati x y;
        y
    | Some y ->
        y
  in
  mem_f

let max_skupno_z_dolzino = memoiziraj_rec odviti_max_skupno_z_dolzino

let izpisi_seznam = function
  | [] -> print_endline "[]"
  | x :: xs ->
      print_string "[";
      print_int x;
      List.iter (fun x -> print_string (", " ^ string_of_int x)) xs;
      print_endline "]"

;;

max_skupno_z_dolzino (
    [4; 5; 2; 1; 9; 0; 4; 0; 1; 0; 7; 5; 9; 8; 5; 2; 1; 6; 8],
    [9; 3; 4; 3; 3; 4; 6; 3; 0; 0; 9; 2; 5; 1; 7; 7; 7; 0; 3]
)
|> fst
|> izpisi_seznam
