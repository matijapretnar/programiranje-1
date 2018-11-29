(* Tip, ki predstavlja izid igre. *)
type result = Victory of Field.symbol | Draw

(* Funkcija za zamenjavo igralcov. *)
let switch_symbol = function
  | Field.Circle -> Field.Cross
  | Field.Cross -> Field.Circle
  | Field.Empty -> Field.Empty

(* Funkcija, ki zahteva vhod igralca in poskusi narediti potezo. Pri tem
   vse neveljavne poteze prestreže. *)
let rec make_move player field =
  print_endline ("It is the turn of player "^(Field.symbol_to_string player));
  try
    print_string "Place in row : ";
    let x = read_int () in
    print_string "Place in column : ";
    let y = read_int () in
    Field.set_symbol x y player field
  with
  (* Prestrezi in obravnavaj napake. *)
  (*/// Obravnavaj napake tako, da izpišeš sporočilo uporabniku in ponoviš
        potezo. Potezo ponoviš tako, da ponovno kličeš "make_move". ///*)
        | Field.Coordinate_out_of_bounds ->
          print_endline "Napačne koordinate. Poskusi ponovno."; make_move player field
        | Field.Nonempty_value_at_coordinates ->
          print_endline "Polje je že polno. Poskusi ponovno."; make_move player field
        | Failure msg when msg = "int_of_string" ->
          print_endline "Vnos mora biti celo število. Poskusi ponovno."; make_move player field

(* Glavna zanka igre, ki izvaja poteze igralcev dokler se igra ne konča.
   Hkrati vsako potezo izpiše trenutno stanje. *)
let rec game_loop player field =
  (* Preveri, če je igre konec. *)
  match Field.victory field with
  | Some s -> Victory s (* Nekdo je zmagal. *)
  | None ->
    if Field.no_more_moves field then
      Draw (* Nihče ne more zmagati. *)
    else
      (* Naredi poteo. *)
      let new_field = make_move player field in
      (* Izpiši polje. *)
      print_newline (); Field.print_field new_field; print_newline ();
      (* Zamenjaj igralca in naredi potezo. *)
      let new_player = switch_symbol player in
      game_loop new_player new_field

(* Zažene igro na praznem polju. Po končani igri izpiše izzi in zažene novo
   igro. *)
let rec run_game () =
  let field = Field.empty_field () in
  let current_player = Field.Cross in
  match game_loop current_player field with
  | Draw ->
    print_endline "Igra je bila neodločena. Začnimo znova!";
    print_newline ();
    run_game ()
  | Victory s ->
    print_endline ("Igralec "^(Field.symbol_to_string s)^" je zmagal! Začnimo znova!");
    print_newline ();
    run_game ()

(* ===== ZAŽENI IGRO ===== *)
(* Odkomentiraj naslednji del v primeru, ko želiš, da se igra zažene takoj,
   ko uporabiš datoteko (uporabno pri prevedenih datotekah). *)

(*

let () = run_game ()

*)
