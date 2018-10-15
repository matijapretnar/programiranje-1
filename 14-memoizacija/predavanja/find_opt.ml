let kvadrati = Hashtbl.create 512

let mem_kvadrat x =
  match Hashtbl.find_opt kvadrati x with
  | Some y -> y
  | None ->
      print_endline (string_of_int x);
      let y = x * x in
      Hashtbl.add kvadrati x y;
      y

let mem_kvadrat' x =
  try
    Hashtbl.find kvadrati x
  with
  | Not_found ->
      print_endline (string_of_int x);
      let y = x * x in
      Hashtbl.add kvadrati x y;
      y

let mem_kvadrat'' x =
  if
    Hashtbl.mem kvadrati x
  then
    Hashtbl.find kvadrati x
  else
    begin
      print_endline (string_of_int x);
      let y = x * x in
      Hashtbl.add kvadrati x y;
      y
    end
