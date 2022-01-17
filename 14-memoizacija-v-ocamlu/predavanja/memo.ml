let cache f =
  let rezultati = Hashtbl.create 32 in
  let mem_f x =
    match Hashtbl.find_opt rezultati x with
    | Some y ->
        print_endline "Odgovor že obstaja";
        y
    | None ->
        print_endline "Odgovor še ne obstaja";
        let y = f x in
        Hashtbl.add rezultati x y;
        y
  in
  mem_f

let kvadrat x =
  Printf.printf "Računam %d^2\n" x;
  let y = x * x in
  Printf.printf "Odgovor je %d\n" y;
  y

let rec fib n =
  Printf.printf "%d\n" n;
  match n with 0 -> 0 | 1 -> 1 | n -> fib (n - 1) + fib (n - 2)

let kvadrat = cache kvadrat

;;
kvadrat 5;
kvadrat 5;
kvadrat 5

;;
print_endline "--------------------";
fib 10

;;
print_endline "--------------------";
fib 10

;;
print_endline "--------------------"

let mem_fib = cache fib

;;
print_endline "--------------------";
mem_fib 10

;;
print_endline "--------------------";
mem_fib 10

;;
print_endline "--------------------";
mem_fib 9
