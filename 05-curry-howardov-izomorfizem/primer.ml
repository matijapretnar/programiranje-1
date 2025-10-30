type ('p, 'q) ali =
  | Levi of 'p
  | Desni of 'q

type prazen

type 'p ne = 'p -> prazen

let iz_p_sledi_ne_ne_p : 'p -> 'p ne ne =
  fun h_p -> ((fun h_ne_p -> h_ne_p h_p))

let iz_ne_p_sledi_p : 'p ne -> 'p =
  fun h_ne_p -> failwith "TODO"
