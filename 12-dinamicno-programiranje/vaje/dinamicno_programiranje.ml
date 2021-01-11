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
  let dimy = Array.length cheese_matrix in
  let dimx = Array.length cheese_matrix.(0) in
  let rec best_path y x =
    let current_cheese = cheese_matrix.(y).(x) in
    let best_right = if x + 1 = dimx then 0 else best_path y (x + 1) in
    let best_down = if y + 1 = dimy then 0 else best_path (y + 1) x in
    current_cheese + max best_right best_down
  in
  best_path 0 0

(*----------------------------------------------------------------------------*]
 Poleg količine sira, ki jo miška lahko poje, jo zanima tudi točna pot, ki naj
 jo ubere, da bo prišla do ustrezne pojedine.

 Funkcija [optimal_path] naj vrne optimalno pot, ki jo mora miška ubrati, da se
 čim bolj nažre. Ker je takih poti lahko več, lahko funkcija vrne poljubno.
 Pripravite tudi funkcijo [convert_path], ki pot pretvori v seznam tež sirčkov
 na poti.
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
  let rec best_path y x =
    let current_cheese = cheese_matrix.(y).(x) in
    let best_right, path_right =
      if x + 1 = dimx then (0, []) else best_path y (x + 1)
    in
    let best_down, path_down =
      if y + 1 = dimy then (0, []) else best_path (y + 1) x
    in
    let best, step =
      if best_right >= best_down
      then (best_right, Right :: path_right)
      else (best_down, Down :: path_down)
    in
    (current_cheese + best, step)
  in
  best_path 0 0 |> snd


let convert_path cheese_matrix path =
  let rec walk y x = function
    | [] -> []
    | dir :: xs ->
        let r, d = 
          match dir with Right -> (0, 1) | Down -> (1, 0) 
        in
        cheese_matrix.(y).(x) :: walk (y + d) (x + r) xs
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

(*----------------------------------------------------------------------------*]
 Izračunali smo število stolpov, a naše vrle gradbince sedaj zanima točna
 konfiguracija. Da ne pride do napak pri sestavljanju, bomo stolpe predstavili
 kar kot vsotne tipe. 

 Stolp posamezne barve so temelji (Bottom), ali pa kot glava bloka pripadajoče
 barve in preostanek, ki je stolp nasprotne barve.

 Definirajte funkcijo [enumerate_towers], ki vrne seznam vseh stolpov podane
 dolžine. Stolpe lahko vrne v poljubnem vrstnem redu. Funkcija naj hitro (in
 brez) prekoračitve sklada deluje vsaj do višine 20.
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


let rec add_red red_block = List.map (fun t -> TopRed (red_block, t))
let rec add_blue blue_block = List.map (fun t -> TopBlue (blue_block, t))

let enumerate_towers height =
  let rec redtop height =
    if height < 0 then []
    else if height = 0 then [RedBottom]
    else
        add_red Red1 (bluetop (height - 1)) 
        @ add_red Red2 (bluetop (height - 2))
  and bluetop height =
    if height < 0 || height = 1 then [] 
    else if height = 0 then [BlueBottom]
    else 
      add_blue Blue2 (redtop (height - 2)) 
      @  add_blue Blue3 (redtop (height - 3))
  in
  List.map (fun t -> Red t) (redtop height)
  @ List.map (fun t -> Blue t) (bluetop height)


(* Alternative solution *)
let rec enumerate_towers2 max_level =
  let rec alternate (r, b) (r1, b1) (r2, b2) level =
    (* Move step by step and keep the towers of height, height+1 and height+2 *)
    if level == 0 then
      List.map (fun r -> Red r) r @ List.map (fun b -> Blue b) b
    else
      alternate
        (r1 @ List.map (fun b -> TopRed (Red1, b)) b, b1)
        (* In new loop, correct towers are the ones that were to high by 1
        block or current blue towers with one more red block *)
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
 drugi naložene najboljše slaščice. Želite si pojesti čim več sladkorja, a
 hkrati poskrbeti, da vas ob pregledu tovarne ne odkrijejo. Da vas pri rednem
 pregledu ne odkrijejo, mora biti razdalija med dvema zaporednima slaščicama,
 ki ju pojeste vsaj `k`.

 Napišite funkcijo [ham_ham], ki sprejme seznam naravnih števil dolžine `n`, ki
 predstavljajo količino sladkorja v slaščicah v stalaži in parameter `k`,
 najmanjšo razdalijo med dvema slaščicama, ki ju še lahko varno pojeste.
 Funkcija naj vrne seznam zastavic `bool`, kjer je `i`-ti prižgan natanko tedaj
 ko v optimalni požrtiji pojemo `i`-to slaščico.

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # ham_ham test_shelf 1;;
 - : bool list = [false; true; false; true; false; true; false; true; false]
 # ham_ham test_shelf 2;;
 - : bool list = [false; true; false; false; false; true; false; false; false]
[*----------------------------------------------------------------------------*)

let test_shelf = [ 1; 2; -5; 3; 7; 19; -30; 1; 0 ]

(* It is easier to switch to an array, but not necessary *)

let ham_ham list k =
  (* Naive version (needs memoisation) *)
  let rec skip k sweets skips =
    (* Skips k sweets and also gives us instructions skipping it (takes care of
    premature list endings) *)
    match k, sweets with
    | 0, _ -> sweets, skips
    | k, [] -> [], skips
    | k, s :: sws -> skip (k-1) sws (false :: skips)
  in
  let rec nom_nom = function
    (* Returns the optimal value and the instructions to achieve it *)
    | [] -> 0, []
    | s :: sws -> 
        let nom =
          (* eat this one *)
          let sws_after_skip, skips = skip k sws [] in
          let v_nom, instr_nom = nom_nom sws_after_skip in
          v_nom + s, true :: skips @ instr_nom
        in
        let no_nom =
          (* do not eat this one *)
          let v_no_nom, instr_no_nom = nom_nom sws in
          v_no_nom, false :: instr_no_nom
        in
        max nom no_nom
  in
  let v_opt, instr_opt = nom_nom list in
  instr_opt

