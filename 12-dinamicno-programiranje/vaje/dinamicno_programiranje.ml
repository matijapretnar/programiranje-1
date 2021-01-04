(* ========== Vaje 6: Dinamično programiranje  ========== *)

(*----------------------------------------------------------------------------*]
 Požrešna miška se nahaja v zgornjem levem kotu šahovnice. Premikati se sme
 samo za eno polje navzdol ali za eno polje na desno in na koncu mora prispeti
 v desni spodnji kot. Na vsakem polju šahovnice je en sirček. Ti sirčki imajo
 različne (ne-negativne) mase. Miška bi se rada kar se da nažrla, zato jo
 zanima, katero pot naj ubere.

 Funkcija [max_cheese cheese_matrix], ki dobi matriko [cheese_matrix] z masami
 sirčkov in vrne največjo skupno maso, ki jo bo miška požrla, če gre po
 optimalni poti.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_cheese test_matrix;;
 - : int = 13
[*----------------------------------------------------------------------------*)

let test_matrix = 
  [| [| 1 ; 2 ; 0 |];
     [| 2 ; 4 ; 5 |];
     [| 7 ; 0 ; 1 |] |]

let max_cheese cheese_matrix =
  let dimx = Array.length cheese_matrix in
  let dimy = Array.length cheese_matrix.(0) in
  let rec best_path x y =
    let current_cheese = cheese_matrix.(x).(y) in
    let best_right = if x + 1 = dimx then 0 else best_path (x + 1) y in
    let best_down = if y + 1 = dimy then 0 else best_path x (y + 1) in
    current_cheese + max best_right best_down
  in
  best_path 0 0

let max_cheese_bottom cheese_matrix =
  let bottom = List.init (Array.length cheese_matrix.(0)) (fun _ -> 0) in
  let lst = List.rev (List.map Array.to_list (Array.to_list cheese_matrix)) in
  let rec best_path bottom current =
    match (bottom, current) with
    | [ b ], [ c ] -> [ b + c ]
    | b :: bs, c :: cs -> (
        let right = best_path bs cs in
        match right with
        | r :: rest -> (c + max b r) :: right
        | _ -> assert false)
    | _ -> assert false
  in
  match List.fold_left best_path bottom lst with
  | result :: _ -> result
  | _ -> assert false

(*----------------------------------------------------------------------------*]
 Poleg količine sira, ki jo miška lahko poje, jo zanima tudi točna pot, ki naj 
 jo ubere, da bo prišla do ustrezne pojedine.

 Funkcija [optimal_path] naj vrne optimalno pot, ki jo mora miška ubrati, da se
 čim bolj nažre. Ker je takih poti lahko več, lahko funkcija vrne poljubno. 
 Pripravite tudi funkcijo [extract_path], ki pot pretvori v seznam tež
 sirčkov na poti.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # optimal_path_bottom test_matrix;;
 - : mouse_direction list = [Right; Down; Down; Right; Down]
 # optimal_path test_matrix |> convert_path test_matrix;;
 - : int list = [1; 2; 4; 5; 1]
[*----------------------------------------------------------------------------*)

type mouse_direction = Down | Right

let optimal_path cheese_matrix =
  let dimx = Array.length cheese_matrix in
  let dimy = Array.length cheese_matrix.(0) in
  let rec best_path x y =
    let current_cheese = cheese_matrix.(x).(y) in
    let best_right, path_right =
      if x + 1 = dimx then (0, []) else best_path (x + 1) y
    in
    let best_down, path_down =
      if y + 1 = dimy then (0, []) else best_path x (y + 1)
    in
    let best, step =
      if best_right >= best_down then (best_right, Right :: path_right)
      else (best_down, Down :: path_down)
    in
    (current_cheese + best, step)
  in
  best_path 0 0 |> snd

let optimal_path_bottom cheese_matrix =
  let bottom = List.init (Array.length cheese_matrix.(0)) (fun _ -> (0, [])) in
  let lst = List.rev (List.map Array.to_list (Array.to_list cheese_matrix)) in
  let rec best_path bottom current =
    match (bottom, current) with
    | [ (b, b_dirs) ], [ c ] -> [ (b + c, Down :: b_dirs) ]
    | (b, b_dirs) :: bs, c :: cs -> (
        let right = best_path bs cs in
        match right with
        | (r, r_dirs) :: rest ->
            let s, dir =
              if b >= r then (b, Down :: b_dirs) else (r, Right :: r_dirs)
            in
            (c + s, dir) :: right
        | _ -> assert false)
    | _ -> assert false
  in
  match List.fold_left best_path bottom lst with
  | result :: _ -> snd result
  | _ -> assert false

let convert_path cheese_matrix path =
  let rec walk x y = function
    | [] -> []
    | dir :: xs ->
        let r, d = match dir with Right -> (1, 0) | Down -> (0, 1) in
        cheese_matrix.(x).(y) :: walk (x + r) (y + d) xs
  in
  walk 0 0 path

(*----------------------------------------------------------------------------*]
 Rešujemo problem sestavljanja alternirajoče obarvanih stolpov. Imamo štiri
 različne tipe gradnikov, dva modra in dva rdeča. Modri gradniki so višin 2 in
 3, rdeči pa višin 1 in 2.

 Funkcija [alternating_towers] za podano višino vrne število različnih stolpov
 dane višine, ki jih lahko zgradimo z našimi gradniki, kjer se barva gradnikov
 v stolpu izmenjuje (rdeč na modrem, moder na rdečem itd.). Začnemo z gradnikom
 poljubne barve.

 Namig: Uporabi medsebojno rekurzivni pomožni funkciji z ukazom [and].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # alternating_towers 10;;
 - : int = 35
[*----------------------------------------------------------------------------*)

let alternating_towers height =
  let rec redtop height =
    if height <= 0 then 0
    else if height <= 2 then 1
    else bluetop (height - 1) + bluetop (height - 2)
  and bluetop height =
    if height <= 0 then 0
    else if height = 2 then 1
    else if height = 3 then 2
    else redtop (height - 2) + redtop (height - 3)
  in
  redtop height + bluetop height

(* Optimal general implementation (for an arbitrary sized blocks) 
   seems to be some sort of circular buffer *)
let alternating_towers_bottom level =
  let rec alternate (r, b) (r1, b1) (r2, b2) level =
    if level == 0 then r + b
    else alternate (r1 + b, b1) (r2 + b, b2 + r) (0, r) (level - 1)
  in
  if level == 0 then 1 else alternate (1, 1) (0, 0) (0, 0) level

(*----------------------------------------------------------------------------*]
 Izračunali smo število stolpov, a naše vrle gradbince sedaj zanima točna 
 konfiguracija. Da ne pride do napak pri sestavljanju, bomo stolpe predstavili
 kar kot vsotne tipe. 

 Stolp posamezne barve so temelji (Bottom), ali pa kot glava bloka pripadajoče 
 barve in preostanek, ki je stolp nasprotne barve.

 Definirajte funkcijo [enumerate_towers], ki vrne seznam vseh stolpov podane 
 dolžine. Stolpe lahko vrne v poljubnem vrstnem redu. Funkcija naj hitro 
 (in brez) prekoračitve sklada deluje vsaj do višine 20.

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # enumerate_towers 4;;
 - : tower list = 
[Red (TopRed (Red2, TopBlue (Blue2, RedBottom)));
 Red (TopRed (Red1, TopBlue (Blue3, RedBottom)));
 Red (TopRed (Red1, TopBlue (Blue2, TopRed (Red1, BlueBottom))));
 Blue (TopBlue (Blue3, TopRed (Red1, BlueBottom)));
 Blue (TopBlue (Blue2, TopRed (Red2, BlueBottom)))]
[*----------------------------------------------------------------------------*)

type blue_block = Blue3 | Blue2

type red_block = Red2 | Red1

type red_tower = TopRed of red_block * blue_tower | RedBottom

and blue_tower = TopBlue of blue_block * red_tower | BlueBottom

type tower = Red of red_tower | Blue of blue_tower

let rec enumerate_towers max_level =
  let rec alternate (r, b) (r1, b1) (r2, b2) level =
    (* Move step by step and keep the towers of height, height+1 and height+2 *)
    if level == 0 then
      List.map (fun r -> Red r) r @ List.map (fun b -> Blue b) b
    else
      alternate
        (r1 @ List.map (fun b -> TopRed (Red1, b)) b, b1)
        (* In new loop, correct towers are the ones that were to high by 1 block or current blue towers with one more red block *)
        ( r2 @ List.map (fun b -> TopRed (Red2, b)) b,
          b2 @ List.map (fun r -> TopBlue (Blue2, r)) r )
        ([], List.map (fun r -> TopBlue (Blue3, r)) r)
        (level - 1)
  in
  (* Special case *)
  if max_level == 0 then []
  else alternate ([ RedBottom ], [ BlueBottom ]) ([], []) ([], []) max_level

(* Test *)
let test_towers x =
  List.init x (fun x ->
      (x, enumerate_towers x |> List.length = alternating_towers x))
  |> List.filter (fun x -> not (snd x))
  |> List.map fst

(*----------------------------------------------------------------------------*]
 Vdrli ste v tovarno čokolade in sedaj stojite pred stalažo kjer so ena ob 
 drugi naložene najboljše slaščice. Želite si pojesti čim več 
 sladkorja, a hkrati poskrbeti, da vas ob pregledu tovarne ne odkrijejo. Da vas 
 pri rednem pregledu ne odkrijejo, mora biti razdalija med dvema zaporednima 
 slaščicama, ki ju pojeste vsaj `k`.

 Napišite funkcijo [ham_ham], ki sprejme seznam naravnih števil dolžine `n`, ki 
 predstavljajo količino sladkorja v slaščicah v stalaži in parameter `k`, najmanjšo
 razdalijo med dvema slaščicama, ki ju še lahko varno pojeste. Funkcija naj vrne 
 seznam zastavic `bool`, kjer je `i`-ti prižgan natanko tedaj ko v optimalni požrtiji 
 pojemo `i`-to slaščico.

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # ham_ham test_shelf 1;;
 - : bool list = [false; true; false; true; false; true; false; true; false]
 # ham_ham test_shelf 2;;
 - : bool list = [false; true; false; false; false; true; false; false; false]
[*----------------------------------------------------------------------------*)

let test_shelf = [ 1; 2; -5; 3; 7; 19; -30; 1; 0 ]

let ham_ham l k =
  let n = List.length l in
  let arr = Array.of_list l in
  let memo = Array.init n (fun x -> (arr.(x), -1)) in
  memo.(n - 1) <- (arr.(n - 1), -1);
  for i = n - 1 downto 0 do
    let cu = fst memo.(i) in
    for j = i - k - 1 downto 0 do
      let cur = fst memo.(j) in
      if cur < cu + arr.(j) then memo.(j) <- (cu + arr.(j), i)
    done
  done;
  let _, max_ind =
    Array.fold_left
      (fun (ma, ind) (i, (x, _)) -> if x > ma then (x, i) else (ma, ind))
      (-99, -1)
      (Array.mapi (fun i x -> (i, x)) memo)
  in
  let mask = Array.init n (fun _ -> false) in
  let i = ref max_ind in
  while 0 <= !i && !i < n do
    mask.(!i) <- true;
    i := snd memo.(!i)
  done;
  mask |> Array.to_list
