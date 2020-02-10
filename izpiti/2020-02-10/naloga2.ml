(* a *)

type ('a, 'b) tree = 
  | Empty 
  | ANode of ('a, 'b) tree * 'a * ('a, 'b) tree 
  | BNode of ('a, 'b) tree * 'b * ('a, 'b) tree 

let aleaf a = ANode (Empty, a, Empty) 
let bleaf b = BNode (Empty, b, Empty)

let test = ANode (bleaf true, 12, ANode (aleaf 0, 5, bleaf false))

(* b *)

let rec adepth = function
    | Empty -> 0
    | ANode (lt, _ ,rt) -> 
        1 + max (adepth lt) (adepth rt)
    | BNode (lt, _ ,rt) -> 
        let ad = max (adepth lt) (adepth rt) in
        if ad = 0 then 0 else 1 + ad

let rec bdepth = function
    | Empty -> 0
    | ANode (lt, _ ,rt) -> 
        let bd = max (bdepth lt) (bdepth rt) in
        if bd = 0 then 0 else 1 + bd
    | BNode (lt, _ ,rt) -> 
        1 + max (bdepth lt) (bdepth rt)

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

let rec foldmap tr fa fb acc =
  match tr with
  | Empty -> (acc, Empty)
  | ANode (lt, a, rt) ->
      let (acc', lt') = foldmap lt fa fb acc in
      let (acc'', a') = fa acc' a in
      let (acc''', rt') = foldmap rt fa fb acc'' in
      (acc''', ANode (lt', a', rt'))
  | BNode (lt, b, rt) ->
      let (acc', lt') = foldmap lt fa fb acc in
      let (acc'', b') = fb acc' b in
      let (acc''', rt') = foldmap rt fa fb acc'' in
      (acc''', BNode (lt', b', rt'))
