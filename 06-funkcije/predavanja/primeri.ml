(* Funkcije višjega reda *)

(* 1. red *)

let f x = x + 1

(* 2. red *)

let g f = f (f 0)

let g f x = f (f x)

let map = List.map

(* reševanje običajnih enačb *)

(* 3. red *)

let h g = g (succ)


(* reševanje diferencialnih enačb *)





(* Anonimne funkcije *)


(* matematika: x |-> x^2 + 1 *)
(* OCaml: fun x -> x^2 + 1 *)
(* Python: lambda x: x^2 + 1 *)




(* Repni klici *)

let f x = x + 10

let g x = f (x + 10)

let h x = (f x) + 10



(* Repna rekurzija *)

let rec vsota_bedna n =
  if n = 0 then 0 else n + vsota_bedna (n - 1)

let vsota n =
  let rec vsota' acc n =
    if n = 0 then acc else vsota' (n + acc) (n - 1)
  in
  vsota' 0 n


let rec bedna_vsota_seznama sez =
  match sez with
  | [] -> 0
  | glava :: rep -> glava + bedna_vsota_seznama rep

let vsota_seznama sez =
  let rec aux acc =
    function
    | [] -> acc
    | glava :: rep -> aux (glava + acc) rep
  in
  aux 0 sez

let rec map f sez =
  match sez with
  | [] -> []
  | glava :: rep -> f glava :: map f rep
  
let obrni sez =
  let rec aux acc =
    function
    | [] -> acc
    | x :: xs -> aux (x :: acc) xs
  in
  aux [] sez

let map' f sez =
  let rec aux acc =
    function
    | [] -> obrni acc
    | glava :: rep -> aux (f glava :: acc) rep
  in
  aux sez
  
  


(* Curryjiranje *)

(*
# let zmnozi x y = x * y;;
val zmnozi : int -> (int -> int) = <fun>
val uporabi_dvakrat_na_nic : (int -> int) -> int = <fun>
*)

let zmnozi x y = x * y

let zmnozi' (x, y) = x * y

let curryiraj brezvezna_funkcija_ki_vzame_par =
  let dobra_funkcija_ki_jo_lahko_delno_uporabimo x y =
    let par = (x, y) in
    brezvezna_funkcija_ki_vzame_par par
  in
  dobra_funkcija_ki_jo_lahko_delno_uporabimo

let curryiraj f =
  fun x y -> f (x, y)
  
(*
 (a * b -> c) -> (a -> (b -> c))
 *)
