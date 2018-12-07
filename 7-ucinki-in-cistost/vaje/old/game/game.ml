(* A type denoting the result of a game. *)
type result = Victory of Field.symbol | Draw

(* A function used for switching players in the game. *)
let switch_symbol = ()
  (*/// Write a function, that transforms a Circle to Cross, a Cross
        to a Circle, and leaves Empty as Empty. Keep in mind, that the
        the definitions of types are in module Field. ///*)

(* The function that takes the users input, tries to make a move and handles
   all common exceptions by restarting the turn. *)
let rec make_move player field =
  (*/// Print whose turn it is. The input "player" is of type Field.symbol. ///*)
  try
    print_string "Place in row : ";
    let x = read_int () in
    print_string "Place in column : ";
    let y = read_int () in
    (*/// Set the symbol on the coordinates x y in the field to the players
          symbol. ///*)
  with
  (* Handle all errors of input. *)
  (*/// Handle the errors by printing out a message and restarting the turn.
        Restarting the turn is done by calling the function "make_move" again. ///*)
  | Field.Coordinate_out_of_bounds ->
    failwith "To Do"
  | Field.Nonempty_value_at_coordinates ->
    failwith "To Do"
  | Failure msg when msg = "int_of_string" ->
    failwith "To Do"

(* The main loop of the game, that makes players take turns until the game is
   finished. Also prints the situation after every turn. *)
let rec game_loop player field = ()
  (*/// The input "player" represents the player whose turn it is and "field"
        the current field layout.
        First check if someone has won, and finish by returning the correct
        result (which is defined at the beginning).
        Next, check if there are any possible moves, if not, return the result
        signifying that noone can win the game.
        Otherwise use the below code to make the next move. ///*)
  (*
      (* Make a move. *)
      let new_field = make_move player field in
      (* Print field. *)
      print_newline (); Field.print_field new_field; print_newline ();
      (* Switch player and repeat loop. *)
      let new_player = switch_symbol player in
      game_loop new_player new_field
  *)

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
