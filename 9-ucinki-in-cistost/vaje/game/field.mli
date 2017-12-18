(* ===== TYPE DEFINITIONS ===== *)
type symbol = Empty | Circle | Cross
type field

exception Coordinate_out_of_bounds
exception Nonempty_value_at_coordinates

(* ===== FIELD CREATION ===== *)
val empty_field : unit -> field

(* ===== GET VALUES ===== *)
val get_symbol : int -> int -> field -> symbol

(* ===== SET VALUES ===== *)
val set_symbol : int -> int -> symbol -> field -> field

(* ===== VICTORY ===== *)
(* Checks if the field has any possible moves left. *)
val no_more_moves : field -> bool
(* Returns the symbol of the winner, if there is a winner. *)
val victory : field -> symbol option

(* ===== PRINTER ===== *)
val symbol_to_string : symbol -> string
val print_field : field -> unit
