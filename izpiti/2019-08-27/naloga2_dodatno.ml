let vecji_opt x = function
| None -> true
| Some y -> x > y

let manjsi_opt x = function
| None -> true
| Some y -> x < y

let preveri_urejenost_z_mejami l = 
  let rec aux soda_meja liha_meja = function
  | [] -> true
  | x::xs -> (
    if x mod 2 = 0 then (
      vecji_opt x soda_meja && aux (Some x) liha_meja xs
    )
    else (
      manjsi_opt x liha_meja && aux soda_meja (Some x) xs
    )
  ) 
in aux None None l
 

(* Pretvarjanje v float in uporaba neskončnosti je lahko problematična, saj v splošnem celih števil ne moremo enoznačno predstaviti s plavajočo vejico

  let unsafe_int = Int.shift_left 1 53
  let unsafe_int2 = unsafe_int + 1
  let convert x = int_of_float (float_of_int x)

  let result = (unsafe_int = unsafe_int2, (convert unsafe_int) = (convert unsafe_int2) )
*)

let unsafe_int = Int.shift_left 1 53
let unsafe_int2 = unsafe_int + 1
let convert x = int_of_float (float_of_int x)

let result = (unsafe_int = unsafe_int2, (convert unsafe_int) = (convert unsafe_int2) )

let preveri_urejenost l = 
  let rec aux min_sodo max_liho = function
    | [] -> true
    | x::xs -> if x mod 2 = 0 then x> min_sodo && aux x max_liho xs else x < max_liho && aux min_sodo x xs
  in 
  aux (-9999) 99999 l
