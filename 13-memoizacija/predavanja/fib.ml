let memoiziraj f =
  let rezultati = Hashtbl.create 512 in
  let mem_f x =
    match Hashtbl.find_opt rezultati x with
    | None ->
        let y = f x in
        Hashtbl.add rezultati x y;
        y
    | Some y ->
        y
  in
  mem_f

let rec fib n =
  print_int n; print_char '-';
  match n with
  | 0 | 1 -> n
  | n -> fib (n - 1) + fib (n - 2)

let mem_fib = memoiziraj fib

let odviti_fib f n =
  print_int n; print_char '-';
  match n with
  | 0 | 1 -> n
  | n -> f (n - 1) + f (n - 2)

let rezultati = Hashtbl.create 512

let rec mem_fib x =
    match Hashtbl.find_opt rezultati x with
    | None ->
        let y = odviti_fib mem_fib x in
        Hashtbl.add rezultati x y;
        y
    | Some y ->
        y

let rec fib n = odviti_fib fib n

(* fib = (odviti_fib fib) *)

(* obicajna rekurzivna funkcija : A -> B *)
(* odvita funkcija : (A -> B) -> (A -> B) *)
(* zavezi vozel : ((A -> B) -> (A -> B)) -> (A -> B) *)

let memoiziraj_rec odviti_f =
  let rezultati = Hashtbl.create 512 in
  let rec mem_f x =
    match Hashtbl.find_opt rezultati x with
    | None ->
        let y = odviti_f mem_f x in
        Hashtbl.add rezultati x y;
        y
    | Some y ->
        y
  in
  mem_f