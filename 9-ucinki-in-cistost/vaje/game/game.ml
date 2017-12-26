(* A type denoting the result of a game. *)
type result = Victory of Field.symbol | Draw

(* A function used for switching players in the game. *)
let switch_symbol = function
  | Field.Circle -> Field.Cross
  | Field.Cross -> Field.Circle
  | Field.Empty -> Field.Empty

(* The function that takes the users input, tries to make a move and handles
   all common exceptions by restarting the turn. *)
let rec make_move player field =
  print_endline ("It is the turn of player "^(Field.symbol_to_string player));
  try
    print_string "Place in row : ";
    let x = read_int () in
    print_string "Place in column : ";
    let y = read_int () in
    Field.set_symbol x y player field
  with
  (* Handle all errors of input. *)
  | Field.Coordinate_out_of_bounds ->
    print_endline "Wrong coordinates. Try again."; make_move player field
  | Field.Nonempty_value_at_coordinates ->
    print_endline "Field is already full. Try again."; make_move player field
  | Failure msg when msg = "int_of_string" ->
    print_endline "Input must be integer. Try agan."; make_move player field

(* The main loop of the game, that makes players take turns until the game is
   finished. Also prints the situation after every turn. *)
let rec game_loop player field =
  (* Check if game over. *)
  match Field.victory field with
  | Some s -> Victory s (* Someone won. *)
  | None ->
    if Field.no_more_moves field then
      Draw (* Noone can win. *)
    else
      (* Make a move. *)
      let new_field = make_move player field in
      (* Print field. *)
      print_newline (); Field.print_field new_field; print_newline ();
      (* Switch player and repeat loop. *)
      let new_player = switch_symbol player in
      game_loop new_player new_field


(* Runs the game loop with an empty field. After the game is finished, it
   announces the result and starts a new game. *)
let rec run_game () =
  let field = Field.empty_field () in
  let current_player = Field.Cross in
  match game_loop current_player field with
  | Draw ->
    print_endline "The game was a draw. Starting new game!";
    print_newline ();
    run_game ()
  | Victory s ->
    print_endline ("Player "^(Field.symbol_to_string s)^" won! Starting new game!");
    print_newline ();
    run_game ()

(* ===== RUN GAME ===== *)
(* Only uncomment this part if you want your compiled program to automatically
   run the main game loop. *)

(*

let () = run_game ()

*)
