let rezultati = Hashtbl.create 32

let rec mem_fib n =
  match Hashtbl.find_opt rezultati n with
  | Some y ->
      Printf.printf "fib(%d) že obstaja\n" n;
      y
  | None ->
      Printf.printf "fib(%d) še ne obstaja\n" n;
      let y = fib n in
      Hashtbl.add rezultati n y;
      y

and fib n =
  Printf.printf "%d\n" n;
  match n with 0 -> 0 | 1 -> 1 | n -> mem_fib (n - 1) + mem_fib (n - 2)

;;
print_endline "--------------------";
mem_fib 10

;;
print_endline "--------------------";
mem_fib 10

;;
mem_fib 9

;;
mem_fib 11
