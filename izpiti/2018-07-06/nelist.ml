type 'a nelist = Base of 'a | Cons of 'a * 'a nelist

let head = function Base x | Cons (x, _) -> x
let tail = function Base _ -> None | Cons (_, t) -> Some t

let rec length = function
  | Base _ -> 1
  | Cons (_, t) -> 1 + length t

let rec list_of_nelist = function
  | Base x -> [x]
  | Cons (x, xs) -> x :: (list_of_nelist xs)

let rec fold f s = function
  | Base x -> f s x
  | Cons (x, l) -> fold f (f s x) l
