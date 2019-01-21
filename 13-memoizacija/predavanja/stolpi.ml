let rec rdeci = function
  | 0 -> 1
  | n when n < 0 -> 0
  | n -> modri (n - 1) + modri (n - 2)

and modri = function
  | 0 -> 1
  | n when n < 0 -> 0
  | n -> rdeci (n - 2) + rdeci (n - 3)

let stolpi = function
  | 0 -> 1
  | n -> rdeci n + modri n

let odviti_rdeci _ m = function
  | 0 -> 1
  | n when n < 0 -> 0
  | n -> m (n - 1) + m (n - 2)

let odviti_modri r _ = function
  | 0 -> 1
  | n when n < 0 -> 0
  | n -> r (n - 2) + r (n - 3)

let memoiziraj_rec2 odviti_f odviti_g =
  let rezultati_f = Hashtbl.create 512 in
  let rezultati_g = Hashtbl.create 512 in
  let rec mem_f x =
    match Hashtbl.find_opt rezultati_f x with
    | None ->
        let y = odviti_f mem_f mem_g x in
        Hashtbl.add rezultati_f x y;
        y
    | Some y ->
        y
  and mem_g x =
    match Hashtbl.find_opt rezultati_g x with
    | None ->
        let y = odviti_g mem_f mem_g x in
        Hashtbl.add rezultati_g x y;
        y
    | Some y ->
        y
  in
  mem_f, mem_g

let mem_rdeci, mem_modri = memoiziraj_rec2 odviti_rdeci odviti_modri
let mem_stolpi = function
  | 0 -> 1
  | n -> mem_rdeci n + mem_modri n
