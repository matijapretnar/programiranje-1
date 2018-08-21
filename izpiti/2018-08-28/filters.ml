type 'a filter = Filter of ('a -> bool) * 'a list * 'a filter | All of 'a list


let divisible_by x = (fun y -> y mod x = 0)


let rec to_division_filter = function
  | x :: xs -> Filter(divisible_by x, [], to_division_filter xs)
  | [] -> All []


let rec insert x = function
  | All lst -> All (x :: lst)
  | Filter (f, lst, filters) when f x -> Filter (f, x :: lst, filters)
  | Filter (f, lst, filters) -> Filter (f, lst, insert x filters)


let rec find x = function
  | All lst -> List.mem x lst
  | Filter (f, lst, filters) when f x -> List.mem x lst
  | Filter (_, _, filters) -> find x filters


let rec empty_filters = function
  | All lst -> (All [], lst)
  | Filter (f, lst, filters) ->
      let e_filters, data = empty_filters filters in
      (Filter (f, [], e_filters), lst @ data)


let rec add_filter f filters =
  let e_filters, data = empty_filters filters in
  let new_filters = Filter (f, [], e_filters) in
  List.fold_right insert data new_filters
