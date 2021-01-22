let podvoji x =
  print_endline ("Računam " ^ string_of_int x);
  2 * x

let rezultati = Hashtbl.create 32

let mem_podvoji x =
  match Hashtbl.find_opt rezultati x with
  | Some y -> y
  | None ->
      let y = podvoji x in
      Hashtbl.add rezultati x y;
      y

let memoiziraj f =
    let rezultati = Hashtbl.create 32 in

    let mem_f x =
      match Hashtbl.find_opt rezultati x with
      | Some y -> y
      | None ->
          let y = f x in
          Hashtbl.add rezultati x y;
          y
    in

    mem_f

let rec stevilo_stolpov =
  function
  | 0 -> 1
  | n when n < 0 -> 0
  | n ->
      let na_dnu_1 = stevilo_stolpov (n - 1)
      and na_dnu_2 = stevilo_stolpov (n - 2)
      and na_dnu_3 = stevilo_stolpov (n - 3)
      in
      na_dnu_1 + na_dnu_2 + na_dnu_3

let odviti_stolpi f = function
| 0 -> 1
| n when n < 0 -> 0
| n ->
    let na_dnu_1 = f (n - 1)
    and na_dnu_2 = f (n - 2)
    and na_dnu_3 = f (n - 3)
    in
    na_dnu_1 + na_dnu_2 + na_dnu_3

let napacni_stolpi n = odviti_stolpi (fun _ -> 50) n

let pravi_stolpi n = odviti_stolpi stevilo_stolpov n
let rec pravi_stolpi2 n =odviti_stolpi pravi_stolpi2 n

let rezultati = Hashtbl.create 32

let rec mem_stolpi n =
  match Hashtbl.find_opt rezultati n with
  | Some y -> y
  | None ->
      let y = def_stolpi n in
      Hashtbl.add rezultati n y;
      y
and def_stolpi n =
  odviti_stolpi mem_stolpi n



let memoiziraj_rec odviti_f =
  let rezultati = Hashtbl.create 32 in
  let rec mem_f n =
    match Hashtbl.find_opt rezultati n with
    | Some y -> y
    | None ->
        let y = def_f n in
        Hashtbl.add rezultati n y;
        y
  and def_f n =
    odviti_f mem_f n
  in
  mem_f

let stevilo_stolpov = memoiziraj_rec (fun stevilo_stolpov ->
  function
  | 0 -> 1
  | n when n < 0 -> 0
  | n ->
      let na_dnu_1 = stevilo_stolpov (n - 1)
      and na_dnu_2 = stevilo_stolpov (n - 2)
      and na_dnu_3 = stevilo_stolpov (n - 3)
      in
      na_dnu_1 + na_dnu_2 + na_dnu_3
)