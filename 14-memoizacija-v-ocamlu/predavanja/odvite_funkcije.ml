let rec fib n =
  Printf.printf "%d\n" n;
  match n with 0 -> 0 | 1 -> 1 | n -> fib (n - 1) + fib (n - 2)

let rec odviti_fib k n =
  Printf.printf "%d\n" n;
  match n with 0 -> 0 | 1 -> 1 | n -> k (n - 1) + k (n - 2)

let rec obicajni_fib n = odviti_fib obicajni_fib n

let leni_fib n =
  odviti_fib
    (fun n ->
      print_endline "Meni se ne da.";
      n)
    n

let rezultati = Hashtbl.create 32

let rec memo_fib n = odviti_fib k n

and k n =
  match Hashtbl.find_opt rezultati n with
  | Some y ->
      Printf.printf "fib(%d) že obstaja\n" n;
      y
  | None ->
      Printf.printf "fib(%d) še ne obstaja\n" n;
      let y = memo_fib n in
      Hashtbl.add rezultati n y;
      y

let cache odviti_f =
  let rezultati = Hashtbl.create 32 in

  let rec mem_f n =
    match Hashtbl.find_opt rezultati n with
    | Some y ->
        Printf.printf "rezultat že obstaja\n";
        y
    | None ->
        Printf.printf "rezultat še ne obstaja\n";
        let y = odviti_f mem_f n in
        Hashtbl.add rezultati n y;
        y
  in
  mem_f

(* (('a -> 'b) -> ('a -> 'b))
    ->
   ('a -> 'b) *)

let odviti_fib fib n =
  Printf.printf "%d\n" n;
  match n with 0 -> 0 | 1 -> 1 | n -> fib (n - 1) + fib (n - 2)

let fib =
  cache (fun fib n ->
      Printf.printf "%d\n" n;
      match n with 0 -> 0 | 1 -> 1 | n -> fib (n - 1) + fib (n - 2))

(*
  let rec f x =
    ...

    ~>

  let f = cache (
    fun f x ->
      ...
  )
      
*)

let cache_funkcije_dveh_arg odviti_f =
  let rezultati = Hashtbl.create 32 in

  let rec mem_f m n =
    match Hashtbl.find_opt rezultati (m, n) with
    | Some y ->
        Printf.printf "rezultat že obstaja\n";
        y
    | None ->
        Printf.printf "rezultat še ne obstaja\n";
        let y = odviti_f mem_f m n in
        Hashtbl.add rezultati (m, n) y;
        y
  in
  mem_f

(* let rec odviti_f k_f k_g x =
  ... k_f ... k_g ...

and g k_f k_g y =
  ... f ... g ... *)

let cache_funkcije_dveh_hkrati_def_funkcij odviti_f odviti_g =
  let rezultati_f = Hashtbl.create 32 in
  let rezultati_g = Hashtbl.create 32 in

  let rec mem_f a =
    match Hashtbl.find_opt rezultati_f a with
    | Some y ->
        Printf.printf "rezultat že obstaja\n";
        y
    | None ->
        Printf.printf "rezultat še ne obstaja\n";
        let y = odviti_f mem_f mem_g a in
        Hashtbl.add rezultati_f a y;
        y
  and mem_g b =
    match Hashtbl.find_opt rezultati_g b with
    | Some y ->
        Printf.printf "rezultat že obstaja\n";
        y
    | None ->
        Printf.printf "rezultat še ne obstaja\n";
        let y = odviti_g mem_f mem_g b in
        Hashtbl.add rezultati_g b y;
        y
  in
  (mem_f, mem_g)
