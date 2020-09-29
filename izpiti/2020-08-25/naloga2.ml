type xytree = 
  | Xsplit of int * xytree * xytree
  | Ysplit of int * xytree * xytree
  | Elements of (int * int) list

(* a *)
let example =
  Xsplit (2,
    Ysplit (3,
      Elements [(1, 1); (0, 2)],
      Elements []),
    Ysplit (2,
      Elements [(3,1)],
      Xsplit (4,
        Elements [(4, 3)],
        Elements []))
  )

(* b *)
let rec num_of_elements = function
  | Xsplit (_, lt, rt) -> num_of_elements lt + num_of_elements rt
  | Ysplit (_, lt, rt) -> num_of_elements lt + num_of_elements rt
  | Elements els -> List.length els

(* c *)
let rec insert (x, y) = function
  | Xsplit (s, lt, rt) ->
      if x <= s then
        Xsplit (s, (insert (x, y) lt), rt)
      else
        Xsplit (s, lt, (insert (x, y) rt))
  | Ysplit (s, lt, rt) ->
      if y <= s then
        Ysplit (s, (insert (x, y) lt), rt)
      else
        Ysplit (s, lt, (insert (x, y) rt))
  | Elements els -> Elements ((x,y) :: els)

(* d *)
let alternates t =
  let rec alts_x = function
    | Xsplit (_, lt, rt) -> alts_y lt && alts_y rt
    | Ysplit (_, lt, rt) -> false
    | Elements _ -> true
  and alts_y = function
    | Xsplit (_, lt, rt) -> false
    | Ysplit (_, lt, rt) -> alts_x lt && alts_x rt
    | Elements _ -> true
  in
  alts_x t || alts_y t

(* e *)
let boxed_correctly xytree =
  let less_eq x = function
    | None -> true
    | Some b -> x <= b
  and more x = function
    | None -> true
    | Some b -> x > b
  in
  let check_el x_low x_up y_low y_up (x, y) =
    more x x_low && more y y_low && less_eq x x_up && less_eq y y_up
  in
  let rec checker x_low x_up y_low y_up = function
    | Xsplit (s, lt, rt) ->
        checker x_low (Some s) y_low y_up lt 
        && checker (Some s) x_up y_low y_up rt
    | Ysplit (s, lt, rt) ->
        checker x_low x_up y_low (Some s) lt
        && checker x_low x_up (Some s) y_up rt
    | Elements els ->
        List.for_all (check_el x_low x_up y_low y_up) els
  in
  checker None None None None xytree

