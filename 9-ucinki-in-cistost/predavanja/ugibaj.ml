exception NegativnoStevilo

;;

try
  (* Nastavimo generator psevdo naključnih števil na psevdo naključno vrednost. *)
  Random.self_init ();
  (* OCamlu povemo, naj ujame klic za prekinitev programa. *)
  Sys.catch_break true;
  print_endline "Izmislil si bom naravno število, ti pa ga boš uganil.";
  print_string "Koliko je največje število, ki si ga sposoben uganiti? ";
  let meja = read_int () in
  let izmisljeno_stevilo = 1 + Random.int meja in

  let rec ugibaj () =
    print_string "Katero število sem si izmislil? ";
    let poskus = read_int () in
    if poskus < 0 then
      raise NegativnoStevilo
    else if izmisljeno_stevilo = poskus then
      print_endline "BRAVO!"
    else if izmisljeno_stevilo < poskus then
      begin
        print_endline "Ne, moje število je manjše";
        ugibaj ()
      end
    else
      begin
        print_endline "Ne, moje število je večje";
        ugibaj ()
      end
  in
  ugibaj ()
with
| Failure "int_of_string" ->
  print_endline "Če ne veš, kako se zapiše števila, tole verjetno nima smisla..."
| Sys.Break ->
  print_endline "Adijo!"
| NegativnoStevilo ->
  print_endline "Rekel sem NARAVNO število!"
| _ ->
  print_endline "Kaj se pa greš?"
