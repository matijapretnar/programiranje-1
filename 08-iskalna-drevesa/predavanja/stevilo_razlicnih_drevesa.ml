type 'a drevo =
  | Prazno
  | Sestavljeno of 'a drevo * 'a * 'a drevo


let rec velikost = function
  | Prazno -> 0
  | Sestavljeno (l, _, d) -> 1 + velikost l + velikost d

let rec dodaj x = function
  | Prazno -> Sestavljeno (Prazno, x, Prazno)
  | Sestavljeno (l, y, d) when x < y -> Sestavljeno (dodaj x l, y, d)
  | Sestavljeno (l, y, d) when x > y -> Sestavljeno (l, y, dodaj x d)
  | drevo -> drevo

let rec vsebuje x = function
  | Prazno -> false
  | Sestavljeno (l, y, d) when x < y -> vsebuje x l
  | Sestavljeno (l, y, d) when x > y -> vsebuje x d
  | drevo -> true

let stevilo_razlicnih xs =
  let rec aux ze_videni = function
    | [] -> velikost ze_videni
    | x :: xs -> aux (dodaj x ze_videni) xs
  in
  aux Prazno xs

let nakljucni_seznam m n = List.init n (fun _ -> Random.int m)
let seznam_zaporednih n = List.init n (fun i -> i)

let stopaj f x =
  let zacetek = Sys.time () in
  let y = f x in
  let konec = Sys.time () in
  print_endline ("Porabljen čas: " ^ string_of_float (1000. *. (konec -. zacetek)) ^ "ms");
  y

(* let primer = nakljucni_seznam 10000 10000 *)
let primer = seznam_zaporednih 10000

let n = stopaj stevilo_razlicnih primer

;;

Random.self_init ();
print_endline ("Število različnih: " ^ string_of_int n)
