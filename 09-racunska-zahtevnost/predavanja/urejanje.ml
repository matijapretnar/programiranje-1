(*---------------------------------------------------------------------------
UREJANJE Z VSTAVLJANJEM
---------------------------------------------------------------------------*)
let rec vstavi y = function
  | [] -> [ y ]
  | x :: xs when y > x -> x :: vstavi y xs
  | x :: xs -> y :: x :: xs

let uredi_z_vstavljanjem sez =
  List.fold_left (fun ze_urejen x -> vstavi x ze_urejen) [] sez

(*---------------------------------------------------------------------------
UREJANJE Z ZLIVANJEM
---------------------------------------------------------------------------*)

(* let razdeli xs =
  let rec aux na_sodih na_lihih = function
    | [] -> (na_sodih, na_lihih)
    | x :: xs' -> aux na_lihih (x :: na_sodih) xs'
  in
  aux [] [] xs *)

let rec razdeli = function
  | [] -> ([], [])
  | x :: xs ->
      let xs1, xs2 = razdeli xs in
      (x :: xs2, xs1)

let rec zlij xs ys =
  match (xs, ys) with
  | [], _ -> ys
  | _, [] -> xs
  | x :: xs', y :: ys' -> if x <= y then x :: zlij xs' ys else y :: zlij xs ys'

let rec uredi_z_zlivanjem = function
  | [] -> []
  | [ x ] -> [ x ]
  | xs ->
      let xs1, xs2 = razdeli xs in
      let xs1' = uredi_z_zlivanjem xs1 and xs2' = uredi_z_zlivanjem xs2 in
      zlij xs1' xs2'

(*---------------------------------------------------------------------------
HITRO UREJANJE
---------------------------------------------------------------------------*)

let pivotiraj pivot xs = List.partition (fun x -> x <= pivot) xs

(* TO ISTO BI ZNAL NAPISATI TUDI KOT:
   let pivotiraj pivot = List.partition ((>=) pivot)
*)

let rec hitro_uredi = function
  | [] -> []
  | x :: xs ->
      let xs1, xs2 = pivotiraj x xs in
      let xs1' = hitro_uredi xs1 and xs2' = hitro_uredi xs2 in
      xs1' @ (x :: xs2')

(*---------------------------------------------------------------------------
ŠTOPANJE
---------------------------------------------------------------------------*)

let nakljucni_seznam n =
  let rec aux acc = function
    | 0 -> acc
    | m -> aux (Random.int n :: acc) (m - 1)
  in
  aux [] n

let stopaj f x =
  let zacetek = Sys.time () in
  let y = f x in
  let konec = Sys.time () in
  let cas = 1000. *. (konec -. zacetek) in
  (cas, y)

let preveri_urejanje opis urejanje velikost =
  let sez = nakljucni_seznam velikost in
  let cas, urejeni_sez = stopaj urejanje sez in
  let res_urejeni_sez = List.sort compare sez in
  print_endline (String.make 80 '=');
  print_endline ("Urejanje: " ^ opis);
  print_endline ("Velikost seznama: " ^ string_of_int velikost);
  print_endline ("Porabljen čas: " ^ string_of_float cas ^ "ms");
  print_endline
    ("Pravilno: " ^ if urejeni_sez = res_urejeni_sez then "DA" else "NE")

(*---------------------------------------------------------------------------*)

let _ =
  preveri_urejanje "Brez urejanja" (fun sez -> sez) 1000;
  preveri_urejanje "Brez urejanja" (fun sez -> sez) 10000;
  preveri_urejanje "Brez urejanja" (fun sez -> sez) 100000;
  preveri_urejanje "Urejanje z vstavljanjem" uredi_z_vstavljanjem 1000;
  preveri_urejanje "Urejanje z vstavljanjem" uredi_z_vstavljanjem 10000;
  preveri_urejanje "Vgrajeno urejanje" (List.sort compare) 1000;
  preveri_urejanje "Vgrajeno urejanje" (List.sort compare) 10000;
  preveri_urejanje "Vgrajeno urejanje" (List.sort compare) 100000;
  preveri_urejanje "Urejanje z zlivanjem" uredi_z_zlivanjem 1000;
  preveri_urejanje "Urejanje z zlivanjem" uredi_z_zlivanjem 10000;
  preveri_urejanje "Urejanje z zlivanjem" uredi_z_zlivanjem 100000;
  preveri_urejanje "Hitro urejanje" hitro_uredi 1000;
  preveri_urejanje "Hitro urejanje" hitro_uredi 10000;
  preveri_urejanje "Hitro urejanje" hitro_uredi 100000
