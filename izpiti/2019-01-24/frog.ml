let test1 = [2; 4; 1; 2; 1; 3; 1; 1; 5]  (* Should be 3 *)
let test2 = [4; 1; 8; 2; 11; 1; 1; 1; 1; 1] (* Should be 2 *)
let test3 = List.init 30 (fun _ -> 12)   (* Should be 3, needs memoisation *)

let frog_jumps swamp =
  let len = List.length swamp in
  let rec frog energy position =
    if position >= len then
      0
    else
      let energy = energy + (List.nth swamp position) in
      let jump k = frog (energy - k) (position + k) in
      let best = ref (jump 1) in
      for i = 2 to energy do
        let s = jump i in
        best := min !best s
      done ;
      1 + !best
  in
  frog 0 0

let memoiziraj_rec odviti_f =
  let rezultati = Hashtbl.create 512 in
  let rec mem_f x =
    match Hashtbl.find_opt rezultati x with
    | None ->
        let y = odviti_f mem_f x in
        Hashtbl.add rezultati x y;
        y
    | Some y ->
        y
  in
  mem_f

let frog_jumps_mem swamp =
  let len = List.length swamp in
  let frog = memoiziraj_rec (fun frog (energy, position) ->
    if position >= len then
      0
    else
      let energy = energy + (List.nth swamp position) in
      let jump k = frog ((energy - k), (position + k)) in
      let best = ref (jump 1) in
      for i = 2 to energy do
        let s = jump i in
        best := min !best s
      done ;
      1 + !best)
  in
  frog (0, 0)
