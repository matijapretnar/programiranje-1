Random.self_init ()

;;

let nakljucni_seznam m n = List.init n (fun _ -> Random.int m)

let stevilo_razlicnih xs =
  let rec aux ze_videni = function
    | [] -> List.length ze_videni
    | x :: xs ->
        if List.mem x ze_videni
        then aux ze_videni xs
        else aux (x :: ze_videni) xs
  in
  aux [] xs

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
