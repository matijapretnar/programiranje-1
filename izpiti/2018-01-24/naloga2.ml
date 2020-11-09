type filter_tree = Node of int * filter_tree * filter_tree
                 | Box of int list

let example_tree = Node(10,
                Node(5, (Box [1]), (Box [])),
                Node(15, (Box []), (Box [19;20])))


let rec insert x ftree =
  match ftree with
  | Node(f, lt, rt) ->
    if f > x
    then Node(f, insert x lt, rt)
    else Node(f, lt, insert x rt)
  | Box(xs) -> Box(x::xs)


let rec insert_many l ftree =
  List.fold_right insert l ftree


let rec boxed_correctly ftree =
  let checker lower upper x =
    match (lower, upper) with
    | (None, None) -> true
    | (Some l, None) -> l <= x
    | (None, Some u) -> x < u
    | (Some l, Some u) -> l <= x && x < u
  in
  let rec values_between lower upper ftree =
    match ftree with
    | Box(xs) -> List.for_all (checker lower upper) xs
    | Node(f, lt, rt) ->
      (values_between lower (Some f) lt) && (values_between (Some f) upper rt)
  in
  values_between None None ftree
