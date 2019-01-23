type 'a mm_drevo = 
  | Node of 'a mm_drevo * 'a * int * 'a mm_drevo
  | Empty

let rec vstavi x = function
  | Empty -> Node (Empty, x, 1, Empty)
  | Node (lt, y, count, rt) when y = x -> Node(lt, y, count + 1, rt)
  | Node (lt, y, count, rt) when y > x -> Node(vstavi x lt, y, count, rt)
  | Node (lt, y, count, rt) -> Node(lt, y, count, vstavi x rt)

let rec multimnozica_iz_seznama = 
  List.fold_left (fun mmtree x -> insert x mmtree) Empty

let rec velikost_multimnozice = function
  | Empty -> 0
  | Node (lt, _, count, rt) -> 
      velikost_multimnozice lt + count + velikost_multimnozice rt


let seznam_iz_multimnozice mmtree = 
  let rec ponovi x n = if n <= 0 then [] else x :: ponovi x (n-1) in
  let rec v_seznam = function
    | Empty -> []
    | Node (lt, x, c, rt) -> (v_seznam lt) @ ponovi x c @ (v_seznam rt)
  in
  v_seznam mmtree

(* Repno rekurzivna različica. *)
type 'a action = DoTree of 'a mm_drevo | Generate of 'a * int 

let rec tlrec_seznam_iz_multimnozice mmtree =
  let rec add_to_list x n lst =
    if n <= 0 then
      lst
    else
      add_to_list x (n - 1) (x :: lst)
  in
  let rec to_list queue acc =
    match queue with
    | [] -> acc
    | (Generate (x, n)) :: queue -> 
        let new_acc = add_to_list x n acc in
        to_list queue new_acc
    | (DoTree Empty) :: queue -> to_list queue acc
    | (DoTree (Node (lt, x, c, rt))) :: queue ->
        let do_left, do_right = DoTree lt, DoTree rt in
        let gen_x = Generate (x, c) in
        to_list (do_right :: gen_x :: do_left :: queue) acc
  in
  to_list [DoTree mmtree] []