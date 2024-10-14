type t

val prazen_avtomat : Stanje.t -> t
val dodaj_nesprejemno_stanje : Stanje.t -> t -> t
val dodaj_sprejemno_stanje : Stanje.t -> t -> t
val dodaj_prehod : Stanje.t -> char -> Stanje.t -> t -> t
val prehodna_funkcija : t -> Stanje.t -> char -> Stanje.t option
val zacetno_stanje : t -> Stanje.t
val seznam_stanj : t -> Stanje.t list
val seznam_prehodov : t -> (Stanje.t * char * Stanje.t) list
val je_sprejemno_stanje : t -> Stanje.t -> bool
val enke_1mod3 : t
val preberi_niz : t -> Stanje.t -> string -> Stanje.t option
