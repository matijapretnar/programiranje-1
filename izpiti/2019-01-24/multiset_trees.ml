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

(* Repno rekurzivna različica, kjer si definiramo pomožen tip za postopke,
   ki jih še moramo izvesti. *)
type 'a action = DoTree of 'a mm_drevo | Generate of 'a * int 

let rec tlrec_seznam_iz_multimnozice mmtree =
  (* Define a tl-rec function that adds [n] repetitions of [x] to [lst]. *)
  let rec add_to_list x n lst =
    if n <= 0 then
      lst
    else
      add_to_list x (n - 1) (x :: lst)
  in
  (* Define the function with a queue of actions that need to be performed. *)
  let rec to_list queue acc =
    match queue with
    | [] -> acc (* Done *)
    | (Generate (x, n)) :: queue -> 
        (* Add [n] repetitions of [x] to list. *)
        let new_acc = add_to_list x n acc in
        to_list queue new_acc
    | (DoTree Empty) :: queue -> to_list queue acc (* Nothing to do. *)
    | (DoTree (Node (lt, x, c, rt))) :: queue ->
        (* Create actions and add them to queue in the right order. *)
        let do_left, do_right = DoTree lt, DoTree rt in
        let gen_x = Generate (x, c) in
        to_list (do_right :: gen_x :: do_left :: queue) acc
  in
  (* Run! *)
  to_list [DoTree mmtree] []