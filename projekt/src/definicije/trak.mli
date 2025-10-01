type t

val prazen : t
val trenutni_znak : t -> char
val je_na_koncu : t -> bool
val premakni_naprej : t -> t
val iz_niza : string -> t
val v_niz : t -> string
val prebrani : t -> string
val neprebrani : t -> string
