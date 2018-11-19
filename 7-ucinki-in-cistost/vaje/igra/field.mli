(* ===== DEFINICIJA TIPOV ===== *)
type symbol = Empty | Circle | Cross
type field

exception Coordinate_out_of_bounds
exception Nonempty_value_at_coordinates

(* ===== IZGRADNJA IGRALNEGA POLJA ===== *)
val empty_field : unit -> field

(* ===== PRIDOBIVANJE PODATKOV IZ POLJA ===== *)
val get_symbol : int -> int -> field -> symbol

(* ===== PISANJE PODATKOV V POLJE ===== *)
val set_symbol : int -> int -> symbol -> field -> field

(* ===== PREVERJANJE ZMAGE ===== *)
(* Preveri ali je polje že zapolnjeno. *)
val no_more_moves : field -> bool
(* V primeru zmage vrne simbol zmagovalca. *)
val victory : field -> symbol option

(* ===== PRINTER ===== *)
val symbol_to_string : symbol -> string
val print_field : field -> unit
