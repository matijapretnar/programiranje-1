let rec fib n =
  Format.printf "%d-" n;
  match n with
  | 0 | 1 -> n
  | n -> fib (n - 1) + fib (n - 2)

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

(* Ta ne dela v redu, ker kliče fib, ki ne memoizira *)
let mem_fib = memoiziraj fib

let odviti_fib f n =
  print_int n; print_char '-';
  match n with
  | 0 | 1 -> n
  | n -> f (n - 1) + f (n - 2)

let rec rec_fib n = odviti_fib rec_fib n

let zavezi_vozel odviti_f =
  let rec f x =
    odviti_f f x
  in
  f

let rec_fib' = zavezi_vozel odviti_fib

let rec_fib'' = zavezi_vozel (fun fib n ->
  print_int n;
  match n with
  | 0 | 1 -> n
  | n -> fib (n - 1) + fib (n - 2)
)

let memoiziraj_rec odviti_f =
  let rezultati = Hashtbl.create 512 in
  let rec mem_f x =
    match find_opt rezultati x with
    | None ->
        let y = odviti_f mem_f x in
        Hashtbl.add rezultati x y;
        y
    | Some y ->
        y
  in
  mem_f

let memo_fib = memoiziraj_rec odviti_fib

let memo_fib' = memoiziraj_rec (fun fib n ->
  Format.printf "%d-" n;
  match n with
  | 0 | 1 -> n
  | n -> fib (n - 1) + fib (n - 2)
)
