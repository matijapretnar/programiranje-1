(* Tudi če "zanemarimo" eksponentno zahtevnost, ki jo povzroči brezglava
   rekurzija, ta rešitev ni najbolj učinkovita, ker vedno znova računamo
   dolžine verižnih seznamov. *)
let rec najdaljse_skupno_podzaporedje xs ys =
  match xs, ys with
  | [], _ | _, [] -> []
  | x :: xs', y :: ys' when x = y -> x :: najdaljse_skupno_podzaporedje xs' ys'
  | x :: xs', y :: ys' ->
      let ce_ni_x = najdaljse_skupno_podzaporedje xs' ys
      and ce_ni_y = najdaljse_skupno_podzaporedje xs ys' in
      if List.length ce_ni_x >= List.length ce_ni_y then ce_ni_x else ce_ni_y

(* Pri tej rešitvi hkrati vodimo tudi dolžine najdaljših podzaporedij. *)
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
  
let max_skupno xs ys = max_skupno_z_dolzino xs ys |> fst

let dolzina_max_skupnega xs ys = max_skupno_z_dolzino xs ys |> snd

let izpisi_seznam = function
  | [] -> print_endline "[]"
  | x :: xs ->
      print_string "[";
      print_int x;
      List.iter (fun x -> print_string (", " ^ string_of_int x)) xs;
      print_endline "]"

;;

max_skupno [1; 3; 2; 5; 10; 20; 30; 5; 4; 2; 7] [1; 2; 3; 5; 10; 30; 20; 30; 1; 9; 3; 13]
|> izpisi_seznam
