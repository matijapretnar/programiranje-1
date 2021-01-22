let prazna_vreca = []

let velikost m = List.length m

let vsebuje x m = List.mem x m

let dodaj x m = if vsebuje x m then m else x :: m

(* ------------------------------------------------------------------------- *)

let stevilo_razlicnih xs =
  let rec aux ze_videni = function
    | [] -> velikost ze_videni
    | x :: xs -> aux (dodaj x ze_videni) xs
  in
  aux prazna_vreca xs

let seznam_zaporednih n = List.init n (fun i -> i)
let nakljucni_seznam m n = List.init n (fun _ -> Random.int m)

let stopaj f x =
  let zacetek = Sys.time () in
  let y = f x in
  let konec = Sys.time () in
  print_endline ("Porabljen čas: " ^ string_of_float (1000. *. (konec -. zacetek)) ^ "ms");
  y

let _ = Random.self_init ()

let primer = nakljucni_seznam 1000 1000
(* let primer = seznam_zaporednih 10000 *)

let n = stopaj stevilo_razlicnih primer

let _ = print_endline ("Število različnih: " ^ string_of_int n)
