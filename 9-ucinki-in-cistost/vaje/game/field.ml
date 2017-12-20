(* ===== TYPE DEFINITIONS ===== *)
type symbol = Empty | Circle | Cross
type row = symbol*symbol*symbol
type field = row*row*row

exception Coordinate_out_of_bounds
exception Nonempty_value_at_coordinates

(* ===== FIELD CREATION ===== *)
let empty_field () =
  let empty_row = (Empty, Empty, Empty) in
  (empty_row, empty_row, empty_row)

(* ===== GET VALUES ===== *)
let get_row n (r1, r2, r3) =
  match n with
  | 0 -> r1
  | 1 -> r2
  | 2 -> r3
  | _ -> raise Coordinate_out_of_bounds

let get_column n (c1, c2, c3) =
  match n with
  | 0 -> c1
  | 1 -> c2
  | 2 -> c3
  | _ -> raise Coordinate_out_of_bounds

let get_symbol r c field = field |> get_row r |> get_column c

(* ===== SET VALUES ===== *)

(* Set the n-th row of the field (r1,r2,r3) to new_r. *)
let set_row n new_r (r1, r2, r3) =
  match n with
  | 0 -> (new_r, r2, r3)
  | 1 -> (r1, new_r, r3)
  | 2 -> (r1, r2, new_r)
  | _ -> raise Coordinate_out_of_bounds

(* Set the n-th column of the row (c1,c2,c3) to new_c. *)
let set_column n new_c (c1, c2, c3) =
  match n with
  | 0 -> (new_c, c2, c3)
  | 1 -> (c1, new_c, c3)
  | 2 -> (c1, c2, new_c)
  | _ -> raise Coordinate_out_of_bounds

(* Set the value of the c-th column of the r-th row of the field. *)
let set_symbol r c value field =
  match get_symbol r c field with
  | Circle | Cross -> raise Nonempty_value_at_coordinates
  | Empty ->
    let row = get_row r field in
    let new_row = set_column c value row in
    set_row r new_row field

(* ===== VICTORY ===== *)

(* If there was no victory previously, checks the current option.
   If there already was a winner, it just pushes it forward. *)
let maybe x y z previous =
  match previous with
  | Some w -> Some w
  | None ->
    match x, y, z with
    | Circle, Circle, Circle -> Some Circle
    | Cross, Cross, Cross -> Some Cross
    | _, _, _ -> None

let no_more_moves (r1, r2, r3) =
  let (a1, a2, a3) = r1 in
  let (b1, b2, b3) = r2 in
  let (c1, c2, c3) = r3 in
  not (List.exists ((=) Empty) [a1;a2;a3;b1;b2;b3;c1;c2;c3])

(* Checks all possible victory options.*)
let victory (r1, r2, r3) =
  let (a1, a2, a3) = r1 in
  let (b1, b2, b3) = r2 in
  let (c1, c2, c3) = r3 in
  None
  |> maybe a1 a2 a3 |> maybe b1 b2 b3 |> maybe c1 c2 c3
  |> maybe a1 b1 c1 |> maybe a2 b2 c2 |> maybe a3 b3 c3
  |> maybe a1 b2 c3 |> maybe a3 b2 c1

(* ===== PRINTER ===== *)

let symbol_to_string = function
  | Empty -> "   "
  | Circle -> " O "
  | Cross -> " X "

let print_symbol s = s |> symbol_to_string |> print_string

let print_row (c1, c2, c3) =
  let p = print_string in
  let ps = print_symbol in
  p "|"; ps c1; p "|"; ps c2; p "|"; ps c3; p "|\n"

let print_separator () = print_string "-------------\n"

let print_field (r1, r2, r3) =
  let ps = print_separator in
  let pr = print_row in
  ps (); pr r1; ps (); pr r2; ps (); pr r3; ps ()
