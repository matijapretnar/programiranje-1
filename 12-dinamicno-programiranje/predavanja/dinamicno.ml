let rec max_skupno xs ys =
    match xs, ys with
    | [], _ -> []
    | _, [] -> []
    | x :: xs', y :: ys' when x = y -> x :: max_skupno xs' ys'
    | x :: xs', y :: ys' ->
        let zs1 = max_skupno xs' ys
        and zs2 = max_skupno xs ys' in
        if List.length zs1 >= List.length zs2 then zs1 else zs2

