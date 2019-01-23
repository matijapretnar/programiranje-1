(* Exercise largely taken from Jeff Erickson's lecture notes. *)

(* In a previous life, you worked as a cashier in the lost Antarctican colony of Nadira, spending
   the better part of your day giving change to your customers. Because paper is a very rare
   and valuable resource in Antarctica, cashiers were required by law to use the fewest bills
   possible whenever they gave change. Thanks to the numerological predilections of one of
   its founders, the currency of Nadira, called Dream Dollars, was available in the following
   denominations: $1, $7, $10, $28.
*)

let denominations = [ 1; 7; 10; 28 ]


(* 0.i) Formulate the problem precisely in natural language. *)

(*
   Given an amount n, find the shortest list of bills such that their sum equals k.
*)


(* 0.ii) Describe the problem recursively. *)

(*
   Given an amount n, for each bill, let s_b denote the recursively computed
   shortest list to change n - bill. Take the shortest s_b and add its
   corresponding bill to change the amount n.
*)


(* 1. The greedy change algorithm repeatedly takes the largest bill that does
   not exceed the target amount. For example, to make $122 using the greedy
   algorithm, we first take a $91 bill, then a $28 bill, and finally three $1
   bills.

   Give an example where this greedy algorithm uses more Dream Dollar bills
   than the minimum possible.

   Hint: this is tricky. If you can't find a solution, you can implement the
   greedy algorithm and test it against your dynamic programming solutions
   later.
*)

let rec bills_greedy n =
  if n < 0
  then failwith "unsolvable"
  else if n = 0
  then []
  else let rec max_le den acc = match den with
      | [] -> acc
      | h :: t -> if h > n then acc else
          max_le t h in
    let nxt = max_le denominations (-1) in
    nxt :: (bills_greedy (n - nxt))


(* 2.i) Describe and analyze a recursive algorithm that computes, given an
   integer k, the shortest list of bills needed to make k Dream Dollars. (Don’t
   worry about making your algorithm fast; just make sure it’s correct.)
*)

let rec bills_rec n =
  if n < 0
  then failwith "unsolvable"
  else if n = 0
  then []
  else
    let sols = List.map
        (fun b -> let k = n - b in if k < 0 then None else
            Some (b :: (bills_rec k))) denominations in
    let sol =
      List.fold_left (fun best next -> match (best, next) with
          | (x, None) | (None, x) -> x
          | (Some best, Some next) -> if List.length best < List.length next
            then Some best else Some next) None sols in
    match sol with
    | None -> failwith ("couldn't solve " ^ (string_of_int n))
    | Some sol -> sol

(* Use the generic memozation function below to write a memoized recursive
   algorithm. *)
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

let rec bills_mem n = memoiziraj_rec (fun bills_rec n ->
    if n < 0
    then failwith "unsolvable"
    else if n = 0
    then []
    else
      let sols = List.map
          (fun b -> let k = n - b in if k < 0 then None else
              Some (b :: (bills_rec k))) denominations in
      let sol =
        List.fold_left (fun best next -> match (best, next) with
            | (x, None) | (None, x) -> x
            | (Some best, Some next) -> if List.length best < List.length next
              then Some best else Some next) None sols in
      match sol with
      | None -> failwith ("couldn't solve " ^ (string_of_int n))
      | Some sol -> sol)


type example = { counter : int ; greedy : int list ; dynamic : int list }

let counterexample () =
  let i = ref 0
  and example = ref None in
  while !i < 500 && !example = None do
    i := !i + 1 ;
    let br = bills_rec !i and bg = bills_greedy !i in
    if not (br = bg) then
      example := Some ({ counter = !i ; greedy = bg ; dynamic = br })
  done ;
  !example


(* 2.ii) Draw the call tree of your recursive definition for n = 5 and identify
   which subproblems are repeated. Can you find an evaluation order that will
   allow you to compute your solutions bottom-up? *)

(* It's rather hard to predict this in general, but because we have a bill of
   denomination 1, we will end up enumerating all possibilities. *)


(* 2.iii) Describe a dynamic programming algorithm that computes, given an integer
   k, the shortest list of bills needed to make k Dream Dollars. (This one needs
   to be fast.)
*)

let bills_iter n =
  let sols = Array.init (n+1) (fun _ -> None) in
  sols.(0) <- Some [];
  for i = 0 to n do
    List.iter (fun bill ->
        let k = i - bill in
        if k < 0
        then ()
        else match sols.(k) with
          | None -> failwith ("impossible " ^ (string_of_int k))
          | Some sk ->
            let sb = bill :: sk in
            let s' =
              (match sols.(i) with
               | None -> sb
               | Some si ->
                 if List.length si < List.length sb
                 then si
                 else sb) in
            sols.(i) <- Some s'
      ) denominations
  done;
  sols . (n)
