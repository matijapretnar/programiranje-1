(* a *)
(*----------------------------------------------------------------------------*]
  Napišite predikat, ki pove, ali je podana trojica celih števil urejena strogo 
  naraščajoče.

    je_urejena : int * int * int -> bool

[*----------------------------------------------------------------------------*)

let je_urejena = ()


(* b *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo, ki sprejme deljenec in delitelj ter vrne rezultat deljenja 
  če se to da, ali pa `None`, če bi prišlo do deljenja z 0.

    poskusi_deljenje : int option -> int option -> int option

[*----------------------------------------------------------------------------*)

let poskusi_deljenje = ()


(* c *)
(*----------------------------------------------------------------------------*]
  Definirajte funkcijo, ki seznam zavrti `n` krat (v vsaki rotaciji se prvi 
  element prestavi na konec seznama).

    zavrti : 'a list -> int -> 'a list

[*----------------------------------------------------------------------------*)

let zavrti = ()


(* d *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo `razdeli : ('a -> int) -> 'a list -> ('a list *  'a list * 'a list)|`,
  ki sprejme primerjalno funkcijo in seznam elementov. Vrne naj trojico, kjer so 
  na prvem mestu vsi elementi za katere je primerjalna funkcija negativna, na 
  drugem vsi, kjer je enaka 0, preostali elementi pa so na tretjem mestu. 
  Elementi naj v seznamih nastopajo v enakem vrstnem redu, kot v prvotnem seznamu. 
  Za vse točke naj bo funkcija repno rekurzivna.

  Kot primer: `razdeli ((-) 3) [1;2;3;4;5;6]`` vrne
  `([4; 5; 6], [3], [1; 2])`

[*----------------------------------------------------------------------------*)

let razdeli = ()
