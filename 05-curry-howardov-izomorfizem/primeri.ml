type ('p, 'q) ali = Levi of 'p | Desni of 'q
type prazen
type 'p ne = 'p -> prazen

let iz_p_sledi_ne_ne_p : 'p -> 'p ne ne = fun h_p h_ne_p -> h_ne_p h_p

let iz_ne_p_ali_q_sledi_ne_p_in_ne_q : ('p, 'q) ali ne -> 'p ne * 'q ne =
  fun h_ne_p_ali_q -> (
    (fun h_p -> h_ne_p_ali_q (Levi h_p)),
    (fun h_q -> h_ne_p_ali_q (Desni h_q))
  )
  

let iz_ne_p_in_ne_q_sledi_ne_p_ali_q : 'p ne * 'q ne -> ('p, 'q) ali ne =
 fun (h_ne_p, h_ne_q) -> function
  | Levi h_p -> h_ne_p h_p
  | Desni h_q -> h_ne_q h_q


let rec iz_p_sledi_q : 'p -> 'q = fun h_p -> iz_p_sledi_q h_p

let iz_ne_p_sledi_p : 'p ne -> 'p = fun h_ne_p -> iz_p_sledi_q h_ne_p
