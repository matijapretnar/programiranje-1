(*----------------------------------------------------------------------------*
 # Rekurzija
[*----------------------------------------------------------------------------*)

(* 
type 'a list =
| []
| 'a :: 'a list
*)

(* dva seznama, ki bi ju radi združili v enega (v pythonu je to append())*)

let rec app l1 l2 =
  match l1 with
  | [] -> l2
  | x :: xs -> x :: (app xs l2)

(*----------------------------------------------------------------------------*
 Napišite spodaj opisane funkcije, ne da bi uporabljali funkcije iz standardne
 knjižnice. Kjer to kažejo primeri, napišite tudi repno rekurzivne različice z
 imenom `ime_funkcije_tlrec`.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Funkcija `reverse`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Definirajte pomožno funkcijo za obračanje seznamov. Funkcija naj bo repno
 rekurzivna.
[*----------------------------------------------------------------------------*)

let reverse sez = 
  let rec aux obrnjen_seznam =
    function
    | [] -> obrnjen_seznam
    | x :: xs -> aux (x :: obrnjen_seznam) xs
  in 
  aux [] sez

(*----------------------------------------------------------------------------*
 ## Funkcija `repeat`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `repeat x n` vrne seznam `n` ponovitev vrednosti `x`. Za neprimerne
  vrednosti `n` funkcija vrne prazen seznam.
[*----------------------------------------------------------------------------*)

(* Tole ne deluje pravilno za negativna števila*)
let rec repeat niz n =
  match n with 
  | 0 -> []
  | n -> niz :: (repeat niz (n-1))

let rec repeat niz n = 
  if n <= 0 then 
    []
  else 
    niz :: (repeat niz (n-1))

let primer_repeat_1 = repeat "A" 5
(* val primer_repeat_1 : string list = ["A"; "A"; "A"; "A"; "A"] *)

let primer_repeat_2 = repeat "A" (-2)
(* val primer_repeat_2 : string list = [] *)

(*----------------------------------------------------------------------------*
 ## Funkcija `range`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `range` naj sprejme število in vrne seznam vseh celih števil od 0 do
 vključno danega števila. Za neprimerne argumente naj funkcija vrne prazen
 seznam. Funkcija naj bo repno rekurzivna. Pri tem ne smete uporabiti vgrajene
 funkcije `List.init`.
[*----------------------------------------------------------------------------*)

(* Ni najboljša funkcija zaradi @ *)
let rec range n = 
  if n < 0 then
    []
  else 
    range (n-1) @ [n]

(* Od profesorja koda *)
let range n =
   let rec range_aux n acc =
     if n < 0 then acc else range_aux (n - 1) (n :: acc)
   in
   range_aux n []

let range n = 
  let rec aux zgrajen_seznam =
  function
  | (-1) -> zgrajen_seznam
  | 0 -> 0 :: zgrajen_seznam (* nepotrebno *)
  | n -> aux (n :: zgrajen_seznam) (n - 1)
  in
  aux [] n

let primer_range = range 10
(* val primer_range : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10] *)

(*----------------------------------------------------------------------------*
 ## Funkcija `map`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `map f list` naj sprejme seznam `list` oblike `x0; x1; x2; ...` in
 funkcijo `f` ter vrne seznam preslikanih vrednosti, torej `f x0; f x1; f x2;
 ...`. Pri tem ne smete uporabiti vgrajene funkcije `List.map`.
[*----------------------------------------------------------------------------*)

let rec map f sez = 
  match sez with
    | [] -> []
    | x :: xs -> f x :: map f xs

let primer_map_1 =
  let plus_two = (+) 2 in
  map plus_two [0; 1; 2; 3; 4]
(* val primer_map_1 : int list = [2; 3; 4; 5; 6] *)

(*----------------------------------------------------------------------------*
 Napišite še funkcijo `map_tlrec`, ki naj bo repno rekurzivna različica funkcije
 `map`.
[*----------------------------------------------------------------------------*)
(* Gradim v obrnjen seznam. Da bi bilo pravilno, bi potrebovala uporabiti @, katere uporaba je draga, zato na koncu seznam obrnem List.rev/reverse *)

let map_tlrec f sez = 
  let rec aux preslikan_seznam sez =
    match sez with
    | [] -> reverse preslikan_seznam
    | x :: xs -> aux (f x :: preslikan_seznam) xs
  in
  aux [] sez

(* Tu na koncu rabiš obrniti seznam, tudi Pretnar ima tako *)

let primer_map_2 =
  let plus_two = (+) 2 in
  map_tlrec plus_two [0; 1; 2; 3; 4]
(* val primer_map_2 : int list = [2; 3; 4; 5; 6] *)

(*----------------------------------------------------------------------------*
 ## Funkcija `mapi`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `mapi` naj bo ekvivalentna Python kodi:

 ```python
 def mapi(f, lst):
     mapi_lst = []
     index = 0
     for x in lst:
         mapi_lst.append(f(index, x))
         index += 1
     return mapi_lst
 ```

 Pri tem ne smete uporabiti vgrajene funkcije `List.mapi`.
[*----------------------------------------------------------------------------*)

let mapi f sez = 
  let rec aux acc index = 
    function
    | [] -> reverse acc
    | x :: xs -> aux ((f index x) :: acc) (index + 1) xs
  in
  aux [] 0 sez

(* Pretnar koda; akumulatorja za seznam ne potrebuješ, potrebuješ le akumulator za index *)
let mapi f lst =
  let rec mapi_aux i =
    function
    | [] -> []
    | x :: xs -> (f i x) :: (mapi_aux (succ i) xs)
  in
  mapi_aux 0 lst

(* Spodnji primer vsaki vredosti v seznamu prišteje njen index *)
let primer_mapi = mapi (+) [0; 0; 0; 2; 2; 2]
(* val primer_mapi : int list = [0; 1; 2; 5; 6; 7] *)

(*----------------------------------------------------------------------------*
 ## Funkcija `zip`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `zip` naj sprejme dva seznama in vrne seznam parov istoležnih
 elementov podanih seznamov. Če seznama nista enake dolžine, naj vrne napako.
 Pri tem ne smete uporabiti vgrajene funkcije `List.combine`.
[*----------------------------------------------------------------------------*)

let rec zip l1 l2 =
  match (l1, l2) with
  | [], [] -> []
  | x :: xs, y :: ys -> (x, y) :: zip xs ys
  | _ -> failwith "Seznama nista enake dolzine"

let primer_zip_1 = zip [1; 1; 1; 1] [0; 1; 2; 3]
(* val primer_zip_1 : (int * int) list = [(1, 0); (1, 1); (1, 2); (1, 3)] *)

(* let primer_zip_2 = zip [1; 1; 1; 1] [1; 2; 3; 4; 5] *)

(*----------------------------------------------------------------------------*
 ## Funkcija `unzip`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `unzip` je inverz funkcije `zip`, torej sprejme seznam parov
  `(x0, y0); (x1, y1); ...` in vrne par seznamov (`x0; x1; ...`, `y0; y1; ...`).
  Pri tem ne smete uporabiti vgrajene funkcije `List.split`.
[*----------------------------------------------------------------------------*)

let rec unzip sez_parov = 
  match sez_parov with
  | [] -> ([], [])
  | (x, y) :: xs -> (x :: fst (unzip xs), y :: snd (unzip xs))

(* Pretnarjeva koda *)
let rec unzip = function
  | [] -> ([], [])
  | (x, y) :: tl -> let (list1, list2) = unzip tl in (x :: list1, y :: list2)

let primer_unzip_1 = unzip [(0,"a"); (1,"b"); (2,"c")]
(* val primer_unzip_1 : int list * string list = ([0; 1; 2], ["a"; "b"; "c"]) *)

(*----------------------------------------------------------------------------*
 Funkcija `unzip_tlrec` je repno rekurzivna različica funkcije `unzip`.
[*----------------------------------------------------------------------------*)

let unzip_tlrec sez_parov =
  let rec unzip_aux sez1 sez2 = 
    function
    | [] -> (reverse sez1, reverse sez2)
    | (x, y) :: xs -> unzip_aux (x :: sez1) (y :: sez2) xs
  in
    unzip_aux [] [] sez_parov 

let primer_unzip_2 = unzip_tlrec [(0,"a"); (1,"b"); (2,"c")]
(* val primer_unzip_2 : int list * string list = ([0; 1; 2], ["a"; "b"; "c"]) *)

(*----------------------------------------------------------------------------*
 ## Zanke
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `loop condition f x` naj se izvede kot python koda:

 ```python
 def loop(condition, f, x):
     while condition(x):
         x = f(x)
     return x
 ```
[*----------------------------------------------------------------------------*)

let rec loop condition f x =
  if condition x then
    loop condition f (f x)
  else
    x

let primer_loop = loop (fun x -> x < 10) ((+) 4) 4
(* val primer_loop : int = 12 *)

(*----------------------------------------------------------------------------*
 ## Funkcija `fold_left`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `fold_left_no_acc f list` naj sprejme seznam `x0; x1; ...; xn` in
 funkcijo dveh argumentov `f` in vrne vrednost izračuna `f(... (f (f x0 x1) x2)
 ... xn)`. V primeru seznama z manj kot dvema elementoma naj vrne napako.
[*----------------------------------------------------------------------------*)

let rec fold_left_no_acc f sez = 
    match sez with
    | [x; y] -> f x y
    | x :: y :: xs -> fold_left_no_acc f ((f x y) :: xs)
    | _ -> failwith "Seznam nima dovolj elementov"

let primer_fold_left_no_acc =
  fold_left_no_acc (^) ["F"; "I"; "C"; "U"; "S"]
(* val primer_fold_left_no_acc : string = "FICUS" *)

(*----------------------------------------------------------------------------*
 ## Funkcija `apply_sequence`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `apply_sequence f x n` naj vrne seznam zaporednih uporab funkcije `f`
 na vrednosti `x` do vključno `n`-te uporabe, torej `[x; f x; f (f x); ...; fⁿ
 x]`. Funkcija naj bo repno rekurzivna.
[*----------------------------------------------------------------------------*)

(* Zakaj ta funkcija ne deluje za prvi primer? Za primer, ko je n = - 1 seveda ne deluje. *)
let apply_sequence f x n = 
  let rec seq_aux fun_na_fun sez n =
    match n with
    | -1 -> reverse sez
    | n -> let nov_sez = fun_na_fun :: sez in seq_aux (f fun_na_fun) nov_sez (n - 1)
  in
  seq_aux x [] n

let apply_sequence f x n = 
  let rec seq_aux fun_na_fun sez n =
    if n < 0 then 
      reverse sez
    else
    let nov_sez = fun_na_fun :: sez in seq_aux (f fun_na_fun) nov_sez (n - 1)
  in
  seq_aux x [] n
 
let primer_apply_sequence_1 = apply_sequence (fun x -> x * x) 2 5
(* val primer_apply_sequence_1 : int list = [2; 4; 16; 256; 65536; 4294967296] *)

let primer_apply_sequence_2 = apply_sequence (fun x -> x * x) 2 (-5)
(* val primer_apply_sequence_2 : int list = [] *)


(*----------------------------------------------------------------------------*
 ## Funkcija `filter`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `filter f list` vrne seznam elementov `list`, pri katerih funkcija `f`
  vrne vrednost `true`.
  Pri tem ne smete uporabiti vgrajene funkcije `List.filter`.
[*----------------------------------------------------------------------------*)

let rec filter pogoj sez = 
  match sez with
  | [] -> []
  | x :: xs -> if pogoj x then x :: filter pogoj xs else filter pogoj xs

let primer_filter = filter ((<)3) [0; 1; 2; 3; 4; 5]
(* val primer_filter : int list = [4; 5] *)

(*----------------------------------------------------------------------------*
 ## Funkcija `exists`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `exists` sprejme seznam in funkcijo, ter vrne vrednost `true` čim
  obstaja element seznama, za katerega funkcija vrne `true` in `false` sicer.
  Funkcija je repno rekurzivna.
  Pri tem ne smete uporabiti vgrajene funkcije `List.find` ali podobnih.
[*----------------------------------------------------------------------------*)

let exists pogoj sez =
  let rec aux resnicnost sez =
  match sez with
  | [] -> resnicnost
  | x :: xs -> if pogoj x then aux true xs else aux resnicnost xs
  in
  aux false sez

(* Pretnarjeva koda *)
let rec exists f = function
  | [] -> false
  | x :: xs -> if f x then true else exists f xs

let primer_exists_1 = exists ((<) 3) [0; 1; 2; 3; 4; 5]
(* val primer_exists_1 : bool = true *)

let primer_exists_2 = exists ((<) 8) [0; 1; 2; 3; 4; 5]
(* val primer_exists_2 : bool = false *)

(*----------------------------------------------------------------------------*
 ## Funkcija `first`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `first f default list` vrne prvi element seznama, za katerega
  funkcija `f` vrne `true`. Če takšnega elementa ni, vrne `default`.
  Funkcija je repno rekurzivna.
  Pri tem ne smete uporabiti vgrajene funkcije `List.find` ali podobnih.
[*----------------------------------------------------------------------------*)

(* Trik, a funkcija NI rekurzivna. Dobra ideja Ajda :) *)
let first pogoj default sez = 
  match filter pogoj sez with
  | [] -> default
  | x :: xs -> x

(* Rekurzivna funkcija *)
let rec first pogoj default sez = 
  match sez with
  | [] -> default
  | x :: xs -> if pogoj x then x else first pogoj default xs

let primer_first_1 = first ((<) 3) 0 [1; 1; 2; 3; 5; 8]
(* val primer_first_1 : int = 5 *)

let primer_first_2 = first ((<) 8) 0 [1; 1; 2; 3; 5; 8]
(* val primer_first_2 : int = 0 *)
