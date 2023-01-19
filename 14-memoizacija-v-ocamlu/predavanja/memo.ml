
let memoiziraj f =
  let ze_izracunani = Hashtbl.create 512 in
  let mem_f x =
    match Hashtbl.find_opt ze_izracunani x with
    | Some y -> y
    | None ->
        let y = f x in
        Hashtbl.add ze_izracunani x y;
        y
    in
    mem_f

let kvadrat x =
  Printf.printf "Računam %d^2" x;
  x * x

let rec fib n =
  Printf.printf "fib(%d)\n" n;
  if n <= 1 then n else fib (n - 1) + fib (n - 2)

let razviti_fib kp n =
  Printf.printf "razviti_fib(%d)\n" n;
  if n <= 1 then n else kp (n - 1) + kp (n - 2)
  
let memoiziraj_razvito razviti_f =
  let ze_izracunani = Hashtbl.create 512 in
  let rec mem_f x =
    match Hashtbl.find_opt ze_izracunani x with
    | Some y -> y
    | None ->
        let y = razviti_f mem_f x in
        Hashtbl.add ze_izracunani x y;
        y
    in
    mem_f

let memoiziraj_dve_razviti razviti_f razviti_g =
  let ze_izracunani_f = Hashtbl.create 512
  and ze_izracunani_g = Hashtbl.create 512 in
  let rec mem_f x =
    match Hashtbl.find_opt ze_izracunani_f x with
    | Some y -> y
    | None ->
        let y = razviti_f mem_f mem_g x in
        Hashtbl.add ze_izracunani_f x y;
        y
  and mem_g x =
    match Hashtbl.find_opt ze_izracunani_g x with
    | Some y -> y
    | None ->
        let y = razviti_g mem_f mem_g x in
        Hashtbl.add ze_izracunani_g x y;
        y
    in
  (mem_f, mem_g)
let mem_f = memoiziraj_razvito razviti_fib



let fib = memoiziraj_razvito (fun fib n ->
  Printf.printf "fib(%d)\n" n;
  if n <= 1 then n else fib (n - 1) + fib (n - 2)
)

let ime_funkcije = memoiziraj_razvito (fun fib n ->
  Printf.printf "fib(%d)\n" n;
  if n <= 1 then n else fib (n - 1) + fib (n - 2)
)
