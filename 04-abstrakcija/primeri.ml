module type MNOZICA = sig
  type 'a t
  val vsebuje : 'a t -> 'a -> bool
  val prazna : 'a t
  val velikost : 'a t -> int
end

module Mnozica : MNOZICA = struct
  type 'a t = 'a list
  let vsebuje mn x = List.mem x mn
  let prazna = []
  let velikost = List.length
end

let stevilo_razlicnih xs =
  let rec aux ze_videni = function
    | [] -> List.length ze_videni
    | x :: xs ->
        if List.mem x ze_videni
        then aux ze_videni xs
        else aux (x :: ze_videni) xs
  in
  aux [] xs
  
let seznam_nakljucnih m n = List.init n (fun _ -> Random.int m)

let seznam_zaporednih n = List.init n (fun i -> i)

let stopaj f x =
  let zacetek = Sys.time () in
  let y = f x in
  let konec = Sys.time () in
  let izpis = 
    Printf.sprintf "Porabljen čas: %f ms\n" (1000. *. (konec -. zacetek))
  in
  print_endline izpis;
  y

let rezultati =
  [1000; 2000; 4000; 8000; 16000]
  |> List.map (seznam_nakljucnih 50)
  |> List.map (stopaj stevilo_razlicnih)
    
