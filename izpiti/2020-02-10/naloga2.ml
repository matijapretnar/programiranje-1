(* a *)

type ('a, 'b) tree = 
  | Empty 
  | ANode of ('a, 'b) tree * 'a * ('a, 'b) tree 
  | BNode of ('a, 'b) tree * 'b * ('a, 'b) tree 

let aleaf a = ANode (Empty, a, Empty) 
let bleaf b = BNode (Empty, b, Empty)

let test = ANode (bleaf true, 12, ANode (Empty, 1, bleaf false))

(* b *)

let rec adepth = function
    | Empty -> 0
    | ANode _ -> 1
    | BNode (lt, _ ,rt) -> max (adepth lt) (adepth rt)

let rec bdepth = function
    | Empty -> 0
    | ANode (lt, _ ,rt) -> max (bdepth lt) (bdepth rt)
    | BNode _ -> 1

(* c *)

type result = {a:int; b:int}

let rec count abtree =
  let rec counter abtree acc =
    match abtree with
    | Empty -> acc
    | ANode (lt, _, rt) -> {a=acc.a+1; b=acc.b} |> counter lt |> counter rt
    | BNode (lt, _, rt) -> {a=acc.a; b=acc.b+1} |> counter lt |> counter rt
  in
  counter abtree {a=0; b=0}

(* e *)

let rec is_typemirror abtree batree =
  match abtree, batree with
  | Empty, Empty -> true
  | ANode (lt1, a, rt1), BNode (lt2, b, rt2) ->
      a = b && is_typemirror lt1 lt2 && is_typemirror rt1 rt2
  | BNode (lt1, b, rt1), ANode (lt2, a, rt2) ->
      a = b && is_typemirror lt1 lt2 && is_typemirror rt1 rt2
  | _ -> false 
 
(* d *)

let rec foldmap fa fb acc = function
  | Empty -> (acc, Empty)
  | ANode (lt, a, rt) ->
      let (acc', lt') = foldmap fa fb acc lt in
      let (acc'', a') = fa acc' a in
      let (acc''', rt') = foldmap fa fb acc'' rt in
      (acc''', ANode (lt', a', rt'))
  | BNode (lt, b, rt) ->
      let (acc', lt') = foldmap fa fb acc lt in
      let (acc'', b') = fb acc' b in
      let (acc''', rt') = foldmap fa fb acc'' rt in
      (acc''', BNode (lt', b', rt'))
