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

let fib = memoiziraj_rec (fun fib n ->
  match n with
  | 0 | 1 -> n
  | n -> fib (n - 1) + fib (n - 2)
)

let max_skupno = memoiziraj_rec (fun max_skupno xs ->
  memoiziraj (fun ys ->
  match xs, ys with
  | [], _ | _, [] -> (0, [])
  | x :: xs', y :: ys' when x = y ->
    let d, zs = max_skupno xs' ys' in
    (d + 1, x :: zs)
  | _ :: xs', _ :: ys' ->
    max
      (max_skupno xs ys')
      (max_skupno xs' ys)
))

let curry f = fun x -> fun y -> f (x, y)
let uncurry g = fun (x, y) -> g x y

let max_skupno' = curry (memoiziraj_rec (fun max_skupno (xs, ys) ->
  match xs, ys with
  | [], _ | _, [] -> (0, [])
  | x :: xs', y :: ys' when x = y ->
    let d, zs = max_skupno (xs', ys') in
    (d + 1, x :: zs)
  | _ :: xs', _ :: ys' ->
    max
      (max_skupno (xs, ys'))
      (max_skupno (xs', ys))
))


let memoiziraj_rec2 odviti_f odviti_g =
  let rezultati_f = Hashtbl.create 512
  and rezultati_g = Hashtbl.create 512 in
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
  (mem_f, mem_g)

let stolpi n =
  let (modri_stolpi, rdeci_stolpi) = memoiziraj_rec2 (fun modri_stolpi rdeci_stolpi ->
      function
      | 0 -> 1
      | n when n < 0 -> 0
      | n -> rdeci_stolpi (n - 2) + rdeci_stolpi (n - 3)
  ) (fun modri_stolpi rdeci_stolpi ->
      function
      | 0 -> 1
      | n when n < 0 -> 0
      | n -> modri_stolpi (n - 1) + modri_stolpi (n - 2)
  )
  in
  modri_stolpi n + rdeci_stolpi n

