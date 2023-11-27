let prazna_mnozica = []

let velikost m = List.length m

let dodaj x m = if List.mem x m then m else x :: m

(* ------------------------------------------------------------------------- *)

let stevilo_razlicnih xs =
  let rec aux ze_videni = function
    | [] -> velikost ze_videni
    | x :: xs -> aux (dodaj x ze_videni) xs
  in
  aux prazna_mnozica xs

let nakljucni_seznam m n = List.init n (fun _ -> Random.int m)

let seznam_zaporednih n = List.init n (fun i -> i)

let stopaj f x =
  let zacetek = Sys.time () in
  let y = f x in
  let konec = Sys.time () in
  Printf.printf "Porabljen čas: %f ms\n" (1000. *. (konec -. zacetek));
  y

let _ = Random.self_init ()

let primer = nakljucni_seznam 40000 40000

(* let primer = seznam_zaporednih 10000 *)

let n = stopaj stevilo_razlicnih primer

let _ = Printf.printf "Število različnih: %d\n" n
