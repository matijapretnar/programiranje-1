type 'a drevo =
  | Prazno
  | Sestavljeno of 'a drevo * 'a * 'a drevo

let prazna_mnozica = Prazno

let rec velikost =
  function
  | Prazno -> 0
  | Sestavljeno (l, _, d) -> 1 + velikost l + velikost d

let rec dodaj x =
  function
  | Prazno -> Sestavljeno (Prazno, x, Prazno)
  | Sestavljeno (l, y, d) when x < y ->
      Sestavljeno (dodaj x l, y, d)
  | Sestavljeno (l, y, d) when x > y ->
      Sestavljeno (l, y, dodaj x d)
  | Sestavljeno (_, y, _) as drevo ->
      (* če pridemo do tega primera, ne velja ne x < y ne y > x,
         zato sta x in y enaka *)
      assert (x = y);
      drevo

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

(* let primer = nakljucni_seznam 5000 5000 *)

let primer = seznam_zaporednih 10000

let n = stopaj stevilo_razlicnih primer

let _ = Printf.printf "Število različnih: %d\n" n
