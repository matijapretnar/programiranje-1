let kvadrati = Hashtbl.create 512

let mem_kvadrat x =
  match Hashtbl.find_opt kvadrati x with
  | Some y -> y
  | None ->
      print_endline ("Računam kvadrat števila " ^ string_of_int x);
      let y = x * x in
      Hashtbl.add kvadrati x y;
      y
