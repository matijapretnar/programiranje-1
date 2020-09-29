(* a *)
let angle_between (x1, y1) (x2, y2) =
  let l1 = sqrt (x1*.x1 +. y1*.y1) in
  let l2 = sqrt (x2*.x2 +. y2*.y2) in
  acos ((x1*.x2 +. y1*.y2) /. (l1 *. l2))

(* b *)
let list_to_triple = function
  | [x; y; z] -> Some (x, y, z)
  | _ -> None

(* c *)
type counter = {lt : int; eq : int; gt : int}

let compare_with lst x =
  let count cnt y =
    if y < x then
      {cnt with lt = cnt.lt + 1}
    else if y = x then
      {cnt with eq = cnt.eq + 1}
    else
      {cnt with gt = cnt.gt + 1}
  in
  List.fold_left count {lt=0; eq=0; gt=0} lst


(* d *)

(* let test = [( * ) 2; ( + ) 3; ( * ) 4] *)
(* let long_test = List.init 1000000 (fun _ -> (+) 1) *)

let rec apply_all = function
  | [] -> (fun x -> x)
  | f :: fs -> (fun x -> f (apply_all fs x))

let rec apply_all_tlrec lst =
  let rec apply_all acc_f = function
  | [] -> acc_f
  | f :: fs -> apply_all (fun x -> acc_f (f x)) fs
  in
  apply_all (fun x -> x) lst

let rec apply_all_tlrec_short lst x =
  List.rev lst |> List.fold_left (fun x f -> f x) x