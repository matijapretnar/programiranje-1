(* 20 *)

(* 1. a) *)

(*
Napišite funkcijo `je_pravokotni : int -> int -> int -> bool`, ki preveri, ali podane stranice (v poljubnem vrstnem redu) ustrezajo Pitagorovemu izreku.
*)

let je_pravokotni a b c =
  let [ a; b; c ] = List.sort compare [ a; b; c ] in
  (a * a) + (b * b) = c * c

(* 1. b) *)

(*
Napišite funkcijo `geometrijsko : int -> int -> int list`, ki sprejma naravna števila `a`, `q` in `n`, ter vrne prvih `n` členov geometrijske vrste s prvim členom `a` in kvocientom `q`.
*)

let rec geometrijsko a q n =
  if n = 0 then [] else a :: geometrijsko (a * q) q (n - 1)

(* 1. c) *)

(*
   Napišite funkcijo tipa \verb|premesaj : ('a * 'b * ('c * 'd * 'e)) -> ('c * 'd * ('a * 'e * 'c))|.
*)

let premesaj (a, b, (c, d, e)) = (c, d, (a, e, b))

(* 1. d) *)

(*
Napišite funkcijo \verb|odberi : 'a list -> int list -> 'a list|, ki sprejme seznam elementov in seznam indeksov ter vrne seznam elementov, ki se nahajajo na mestih, ki so podani z indeksi.
Seznam indeksov bo urejen naraščajoče, ne bo vseboval ponovitev in vsi indeksi bodo veljavni glede na dolžino seznama elementov.
Vaša funkcija mora delovati učinkovito (torej v času, ki je linearen v dolžini vhodnega seznama elementov).
*)

let odberi sez indexi =
  let rec odberi offset sez indeksi =
    match (sez, indeksi) with
    | _, [] -> []
    | [], _ -> failwith "Indeks je prevelik"
    | x :: xs, i :: is ->
        if i == offset then x :: odberi (offset + 1) xs is
        else odberi (offset + 1) xs indeksi
  in
  odberi 0 sez indexi

let example = odberi [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] [ 0; 2; 4; 6; 8 ]
