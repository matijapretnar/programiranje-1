(* ========== Vaje 6: Dinamično programiranje  ========== *)

(* Dekorator, ki doda memoizacijo (iz predavanj) *)
let dodaj_spomin_rekurzivni odviti_f =
  let spomin = Hashtbl.create 0 in
  let rec f_s_spominom x =
    match Hashtbl.find_opt spomin x with
    | Some y -> y
    | None ->
        let y = odviti_f f_s_spominom x in
        Hashtbl.add spomin x y;
        y
  in
  f_s_spominom

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

let max_cheese matrix =
  let visina = Array.length matrix in
  let sirina = Array.length matrix.(0) in
  let odvita_miska f =
    function
    | (j, i) when j = visina - 1 && i = sirina - 1 -> matrix.(j).(i)
    | (j, i) when i = sirina - 1 -> matrix.(j).(i) + f (j + 1, i)
    | (j, i) when j = visina - 1 -> matrix.(j).(i) + f (j, i + 1)
    | (j, i) ->
        let spodaj = f (j + 1, i) in
        let desno = f (j, i + 1) in
        matrix.(j).(i) + max spodaj desno
    in
  let f_s_spominom = dodaj_spomin_rekurzivni odvita_miska in
  f_s_spominom (0, 0)


let test_matrix = 
  [| [| 1 ; 2 ; 0 |];
     [| 2 ; 4 ; 5 |];
     [| 7 ; 0 ; 1 |] |]

let cheese_test = max_cheese test_matrix
  
(*----------------------------------------------------------------------------*]
 Poleg količine sira, ki jo miška lahko poje, jo zanima tudi točna pot, ki naj
 jo ubere, da bo prišla do ustrezne pojedine.

 Funkcija [optimal_path] naj vrne optimalno pot, ki jo mora miška ubrati, da se
 čim bolj nažre. Ker je takih poti lahko več, lahko funkcija vrne poljubno.
 Pripravite tudi funkcijo [convert_path], ki pot pretvori v seznam tež sirčkov
 na poti.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # optimal_path test_matrix;;
 - : mouse_direction list = [Right; Down; Right; Down]
 # optimal_path test_matrix |> convert_path test_matrix;;
 - : int list = [1; 2; 4; 5; 1]
[*----------------------------------------------------------------------------*)

type mouse_direction = Down | Right

let optimal_path matrix =
  let visina = Array.length matrix in
  let sirina = Array.length matrix.(0) in
  let odvita_miska f =
    function
    | (r, c) when r = visina - 1 && c = sirina - 1 -> (matrix.(r).(c), [])
    | (r, c) when c = sirina - 1 ->
        let (dol_v, dol_pot) = f (r + 1, c) in
        (matrix.(r).(c) + dol_v, Down :: dol_pot)
    | (r, c) when r = visina - 1 ->
        let (desno_v, desno_pot) = f (r, c + 1) in
        (matrix.(r).(c) + desno_v, Right :: desno_pot)
    | (r, c) ->
        let (dol_v, dol_pot) = f (r + 1, c) in
        let (desno_v, desno_pot) = f (r, c + 1) in
        if dol_v > desno_v then
          (matrix.(r).(c) + dol_v, Down :: dol_pot)
        else
          (matrix.(r).(c) + desno_v, Right :: desno_pot)
  in
  let f_s_spominom = dodaj_spomin_rekurzivni odvita_miska in
  let (_, path) = f_s_spominom (0, 0) in
  path

let convert_path matrix path =
  let rec walk r c = function
    | [] -> [matrix.(r).(c)]
    | Down :: rest -> matrix.(r).(c) :: walk (r + 1) c rest
    | Right :: rest -> matrix.(r).(c) :: walk r (c + 1) rest
  in
  walk 0 0 path

let path_test = optimal_path test_matrix
let path_matrix_test = optimal_path test_matrix |> convert_path test_matrix

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

(* En spomin *)
type block_colour = Red | Blue
let alternating_towers visina =
  let odviti_stolp f =
    function
      | (_, 0) -> 1
      | (_, h) when h < 0 -> 0
      | (Red, h) ->
          f (Blue, h - 1) + f (Blue, h - 2)
      | (Blue, h) ->
          f (Red, h - 2) + f (Red, h - 3)
  in
  let f_s_spominom = dodaj_spomin_rekurzivni odviti_stolp in
  f_s_spominom (Red, visina) + f_s_spominom (Blue, visina)

(* Dva spomina, enaka struktura funkcije kot 
`stolpi_s_spominom_ki_se_ga_od_zunaj_ne_vidi` iz predavanj *)
let alternating_towers_two_mem visina =
  let spomin_rdeci = Hashtbl.create 0 in
  let spomin_modri = Hashtbl.create 0 in
  let rec rdeci_stolp h =
    match Hashtbl.find_opt spomin_rdeci h with
    | Some y -> y
    | None ->
        let y =
          match h with
          | 0 -> 1
          | h when h < 0 -> 0
          | _ -> modri_stolp (h - 1) + modri_stolp (h - 2)
        in
        Hashtbl.add spomin_rdeci h y;
        y
  and modri_stolp h =
    match Hashtbl.find_opt spomin_modri h with
    | Some y -> y
    | None ->
        let y =
          match h with
          | 0 -> 1
          | h when h < 0 -> 0
          | _ -> rdeci_stolp (h - 2) + rdeci_stolp (h - 3)
        in
        Hashtbl.add spomin_modri h y;
        y
  in
  rdeci_stolp visina + modri_stolp visina

  
let towers_test = alternating_towers 10
let towers_test_two_mem = alternating_towers_two_mem 10
(* # alternating_towers 1000;;
- : int = 2211784341800390547 *)

(*----------------------------------------------------------------------------*]
 Izračunali smo število stolpov, a naše vrle gradbince sedaj zanima točna
 konfiguracija. Da ne pride do napak pri sestavljanju, bomo stolpe predstavili
 kar kot naštevne tipe. 

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

(* Posodobimo funkcijo z enim spominom *)
type internal_color = R | B
let enumerate_towers visina =
  let odviti_enumerate f = function
    | (_, h) when h < 0 -> []
    | (R, 0) -> [Red RedBottom]
    | (B, 0) -> [Blue BlueBottom]
    | (R, h) ->
        let red1 = f (B, h - 1) |> List.filter_map (function Blue t -> Some (Red (TopRed (Red1, t))) | _ -> None) in
        let red2 = f (B, h - 2) |> List.filter_map (function Blue t -> Some (Red (TopRed (Red2, t))) | _ -> None) in
        red1 @ red2
    | (B, h) ->
        let blue2 = f (R, h - 2) |> List.filter_map (function Red t -> Some (Blue (TopBlue (Blue2, t))) | _ -> None) in
        let blue3 = f (R, h - 3) |> List.filter_map (function Red t -> Some (Blue (TopBlue (Blue3, t))) | _ -> None) in
        blue2 @ blue3
  in
  let f_s_spominom = dodaj_spomin_rekurzivni odviti_enumerate in
  f_s_spominom (R, visina) @ f_s_spominom (B, visina)

let towers_test_enumerate = enumerate_towers 4
let towers_same_value_test = List.length (enumerate_towers 20) = alternating_towers 20

(*----------------------------------------------------------------------------*]
 Vdrli ste v tovarno čokolade in sedaj stojite pred stalažo kjer so ena ob
 drugi naložene najboljše slaščice. Želite si pojesti čim več sladkorja, a
 hkrati poskrbeti, da vas ob pregledu tovarne ne odkrijejo. Da vas pri rednem
 pregledu ne odkrijejo, mora biti razdalja med dvema zaporednima slaščicama,
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

let ham_ham shelf k =
  let n = List.length shelf in
  let shelf_array = Array.of_list shelf in

  (* Vrne rezultat od indeksa i naprej *)
  let odviti_ham f i =
    if i >= n then (0, [])
    else
      (* NE pojemo i-te cokolade *)
      let (rez_ne, bools_ne) = f (i + 1) in
      (* Pojemo i-to cokolado *)
      let (rez_ja, bools_ja) = f (i + k + 1) in
      let rez_skupaj = shelf_array.(i) + rez_ja in
      
      if rez_skupaj >= rez_ne then
        (rez_skupaj, true :: (List.init k (fun _ -> false)) @ bools_ja)
      else
        (rez_ne, false :: bools_ne)
  in
  let f_s_spominom = dodaj_spomin_rekurzivni odviti_ham in
  
  (* Zanima nas le seznam resnicnostnih vrednosti *)
  let (_, full_path) = f_s_spominom 0 in
  
  (* Seznam je lahko predolg *)
  let rec take n = function
    | [] -> []
    | _ when n <= 0 -> []
    | x :: xs -> x :: take (n - 1) xs
  in
  take n full_path


let test_shelf = [1;2;-5;3;7;19;-30;1;0]
let test_ham_1 = ham_ham test_shelf 1
let test_ham_2 = ham_ham test_shelf 2
(* Zadnjo slascico lahko (v tem primeru) pojemo ali ne *)