let rec dolzina_max_skupnega xs ys =
  match xs, ys with
  | [], _ | _, [] -> 0
  | x :: xs', y :: ys' when x = y ->
    1 + dolzina_max_skupnega xs' ys'
  | _ :: xs', _ :: ys' ->
    max
      (dolzina_max_skupnega xs ys')
      (dolzina_max_skupnega xs' ys)

let rec max_skupno xs ys =
  match xs, ys with
  | [], _ | _, [] -> (0, [])
  | x :: xs', y :: ys' when x = y ->
    let d, zs = max_skupno xs' ys' in
    (d + 1, x :: zs)
  | _ :: xs', _ :: ys' ->
    max
      (max_skupno xs ys')
      (max_skupno xs' ys)

let max_narascajoce xs =
  let rec aux mini xs =
    match mini, xs with
    | _, [] -> (0, [])
    | None, x :: xs'
    | Some mini, x :: xs' when mini <= x ->
        let d, zs = aux x xs' in
        (d + 1, x :: zs)
    | mini, _ :: xs' -> aux mini xs'
  in
  aux None xs
