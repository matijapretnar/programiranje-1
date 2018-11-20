(* Definiramo izjemo, ki jo bomo sprožili ob vnosu negativnega števila *)
exception NegativnoStevilo

;;

try
  (* PREDPRIPRAVA *)
  (* Generatorju psevdonaključnih števil psevdonaključno nastavimo seme. *)
  Random.self_init ();
  (* OCamlu povemo, naj ujame klic za prekinitev programa. *)
  Sys.catch_break true;
  print_string "Do koliko znaš šteti? ";
  (* Z vhodne vrstice preberemo niz in ga pretvorimo v število. *)
  let meja = read_int () in
  (* Izračunamo psevdonaključno število v danih mejah *)
  let izmisljeno_stevilo = 1 + Random.int meja in
  print_endline ("Izmislil sem si število med 1 in " ^ string_of_int meja);

  let preberi_stevilo () =
    let poskus = read_int () in
    if poskus < 0 then
      (* Če je vnešeno število negativno, sprožimo izjemo. *)
      raise NegativnoStevilo
    else
      poskus
  in

  (* GLAVNA ZANKA *)
  (* Definiramo glavno zanko programa *)
  let rec ugibaj () =
    print_string "Katero število sem si izmislil? ";
    let poskus = preberi_stevilo () in
    if izmisljeno_stevilo = poskus then
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
  (* Poženemo glavno zanko programa *)
  ugibaj ()

(* LOVLJENJE IZJEM *)
with
| Failure msg when msg = "int_of_string" ->
  print_endline "Če ne veš, kako se zapiše števila, tole verjetno nima smisla..."
| Sys.Break ->
  print_endline "Adijo!"
| NegativnoStevilo ->
  print_endline "Rekel sem NARAVNO število!"
| _ ->
  print_endline "Kaj se pa greš?"
