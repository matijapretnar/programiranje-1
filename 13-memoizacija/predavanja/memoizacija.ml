let kvadrati = Hashtbl.create 512
let mem_kvadrat x =
  match Hashtbl.find_opt kvadrati x with
  | Some y -> y
  | None ->
      print_endline (string_of_int x);
      let y = x * x in
      Hashtbl.add kvadrati x y;
      y

let fibonaccijeva_st = Hashtbl.create 512
let rec mem_fib n =
  match Hashtbl.find_opt fibonaccijeva_st n with
  | Some fn -> fn
  | None ->
      print_endline ("Zacenjam " ^ string_of_int n);
      let fn =
        match n with
        | 0 | 1 -> n
        | n -> mem_fib (n - 1) + mem_fib (n - 2)
      in
      print_endline ("Koncal z " ^ string_of_int n);
      Hashtbl.add fibonaccijeva_st n fn;
      fn

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

let odviti_fib f n =
  print_int n;
  match n with
  | 0 | 1 -> n
  | n -> f (n - 1) + f (n - 2)

let rec fib n = odviti_fib fib n

let memoiziraj_rec odviti_f =
  let fref = ref (fun _ -> assert false) in
  let mem_f =
    memoiziraj (fun x -> odviti_f !fref x)
  in
  fref := mem_f;
  mem_f

let memo_fib = memoiziraj_rec odviti_fib

(* Bolj enostavna rešitev kot tista na predavanjih *)
let memoiziraj_rec' odviti_f =
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
