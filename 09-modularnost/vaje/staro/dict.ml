(* Another possible direction for exercises on modules *)
type comparison = LT | EQ | GT

module type Dict = sig
    type key

    type 'a t

    val compare : key -> key -> comparison

    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t

    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t

    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list

    val find : key -> 'a t -> 'a
    val find_opt : key -> 'a t -> 'a option
  end

type key = int
let compare_int x y = if x < y then LT else if x > y then GT else EQ

module List_Dict : Dict = struct

  type nonrec key = key
  type 'a t = (key * 'a) list

  let compare = compare_int

  let uncurry f = (fun (k,v) -> f k v)

  let empty = []
  let is_empty = function [] -> true | _::_ -> false
  let mem x = List.exists (fun (k, _v) -> x == k)
  let add k v = List.cons (k, v)
  let remove k = List.filter (fun (k',_v) -> k != k')
  let rec update k f = function
    | [] -> (match f None with
             | None -> []
             | Some x -> [(k, x)])
    | (k', v) :: d ->
       (if k' == k
        then match f (Some v) with
             | None -> remove k d
             | Some x -> (k, x) :: d
        else (k', v) :: (update k f d))
  let singleton k v = [(k,v)]

  let iter f = List.iter (uncurry f)
  let fold f d base =
    let rec aux seen acc = function
      | [] -> acc
      | (k, v) :: d ->
         if List.mem k seen
         then aux seen acc d
         else aux seen (f k v acc) d
    in aux [] base d
  let for_all pred = List.for_all (uncurry pred)
  let exists pred = List.exists (uncurry pred)
  let filter pred = List.filter (uncurry pred)
  let map f = List.map (fun (k, x) -> (k, f x))

  let cardinal d =
    let rec aux seen count = function
      | [] -> count
      | (k,_)::d -> if List.mem k seen then aux seen count d
                    else aux (k::seen) (count + 1) d
    in aux [] 0 d
  let bindings d =
    let (_, l) =
      List.fold_left
        (fun (seen, res) (k,v) ->
         if List.mem k seen
         then (seen, res)
         else (k :: seen, (k,v) :: res))
        ([],[]) d in
    List.sort (fun (k,_) (k',_) -> Pervasives.compare k k') l

  let rec find k = function
    | [] -> raise Not_found
    | (k',v) :: _ when k' == k -> v
    | _ :: d -> find k d
  let find_opt k d =
    try Some (find k d)
    with Not_found -> None

end
