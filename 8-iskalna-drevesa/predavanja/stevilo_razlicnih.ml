Random.self_init ()

;;

let velikost m = List.length m
let dodaj x m = if List.mem x m then m else x :: m

let stevilo_razlicnih xs =
  let rec aux ze_videni = function
    | [] -> velikost ze_videni
    | x :: xs -> aux (dodaj x ze_videni) xs
  in
  aux [] xs

let nakljucni_seznam m n = List.init n (fun _ -> Random.int m)

let stopaj f x =
  let zacetek = Sys.time () in
  let y = f x in
  let konec = Sys.time () in
  print_endline ("Porabljen čas: " ^ string_of_float (1000. *. (konec -. zacetek)) ^ "ms");
  y

let primer = nakljucni_seznam 10000 10000

let n = stopaj stevilo_razlicnih primer

;;

print_endline ("Število različnih: " ^ string_of_int n)
