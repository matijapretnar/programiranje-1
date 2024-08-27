(* 1. a) *)
(*
Napišite funkcijo `je_pravokotni : int -> int -> int -> bool`, ki preveri, ali podane stranice (v poljubnem vrstnem redu) ustrezajo Pitagorovemu izreku.   
*)

(* 1. b) *)
(*
Napišite funkcijo `geometrijsko : int -> int -> int -> int list`, ki sprejma naravna števila `a`, `q` in `n`, ter vrne prvih `n` členov geometrijske vrste s prvim členom `a` in kvocientom `q`.   
*)

(* 1. c) *)
(*
Napišite funkcijo `premesaj : ('a * 'b * ('c * 'd * 'e)) -> ('c * 'd * ('a * 'e * 'c))`.   
*)

(* 1. d) *)
(*
Napišite funkcijo `odberi : 'a list -> int list -> 'a list`, ki sprejme seznam elementov in seznam indeksov ter vrne seznam elementov, ki se nahajajo na mestih, ki so podani z indeksi.
Seznam indeksov bo urejen naraščajoče, ne bo vseboval ponovitev in vsi indeksi bodo veljavni glede na dolžino seznama elementov.
Vaša funkcija mora delovati učinkovito (torej v času, ki je linearen v dolžini vhodnega seznama elementov).

# odberi [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] [ 0; 2; 4; 6; 8 ];;
- : int list = [1; 3; 5; 7; 9]
*)
