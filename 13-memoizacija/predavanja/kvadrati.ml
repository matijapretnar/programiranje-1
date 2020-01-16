let kvadrat x =
  let y = x * x in
  Format.printf "%d^2 = %d" x y;
  y

let find_opt hashtbl x =
    try
        Some (Hashtbl.find hashtbl x)
    with
        Not_found -> None

let memoiziraj f =
  let rezultati = Hashtbl.create 512 in
  let mem_f x =
    match find_opt rezultati x with
    | None ->
        let y = f x in
        Hashtbl.add rezultati x y;
        y
    | Some y ->
        y
  in
  mem_f

let mem_kvadrat = memoiziraj kvadrat
