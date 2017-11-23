(* ===== Exercise 2: Functional Programming  ===== *)

(*Hint: Write a function for reversing lists.*)

let reverse l =
  let rec reverse_aux acc = function
    | [] -> acc
    | hd::tl -> reverse_aux (hd::acc) tl
  in reverse_aux l []

(* The function "repeat x n" returns a list with n repetitions of x.
 For unsuitable n it returns an empty list.
 ----------
 # repeat "A" 5;;
 - : string list = ["A"; "A"; "A"; "A"; "A"]
 # repeat "A" (-2);;
 - : string list = []
 ---------- *)

let rec repeat x n = if n<=0 then [] else x::(repeat x (n-1))

(* The function "range n" returns a list of all non-negative integers up to
 (including) n. For unsuitable n it returns an empty list.
 The function is tail recursive..
 ----------
 # range 10;;
 - : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
 ---------- *)

let range n =
  let rec range_aux n acc =
    if n < 0 then acc else range_aux (n-1) (n::acc)
  in
  range_aux n []

(* The function "map f l" accepts a list l = [l0; l1; l2; ...] and a function f
 and returns the list [f l0; f l1; f l2; ...].
 ----------
 # let plus_two = (+)2 in
   map plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
 ---------- *)

let rec map f = function
  | [] -> []
  | hd::tl -> (f hd) :: (map f tl)

(* The function "map_tlrec" is the tail recursive version of map.
 ----------
 # let plus_two = (fun x -> x+2) in
   map_tlrec plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
 ---------- *)

let map_tlrec f l =
  let rec map_aux f l acc =
    match l with
    | [] -> acc
    | hd::tl -> map_aux f tl (f hd :: acc)
  in
  let mapped_but_reversed = map_aux f l [] in
  reverse mapped_but_reversed

(* The function "mapi f l" accepts a list l = [l0; l1; l2; ...] and a function f
 and returns the list [f 0 l0; f 1 l1; f 2 l2; ...].
 ----------
 # mapi (+) [0; 0; 0; 2; 2; 2];;
 - : int list = [0; 1; 2; 5; 6; 7]
 ---------- *)

let mapi f l =
  let rec mapi_aux f l index =
    match l with
    | [] -> []
    | hd::tl -> (f index hd)::(mapi_aux f tl (index+1))
  in
  mapi_aux f l 0

(* The function "zip l1 l2" accepts lists l1 = [l1_0; l1_1; l1_2; ...] and
 l2 = [l2_0; l2_1; l2_2; ...] and returns the list [(l1_0,l2_0); (l1_1,l2_1); ...].
 If the lenght of lists l1 and l2 doesn't match it fails.
 ----------
 # zip [1; 1; 1; 1] [0; 1; 2; 3];;
 - : (int * int) list = [(1, 0); (1, 1); (1, 2); (1, 3)]
 # zip [1; 1; 1; 1] [1; 2; 3; 4; 5];;
 Exception: Failure "Different lengths.".
 ---------- *)

let rec zip l1 l2 =
  match (l1,l2) with
  | ([],[]) -> []
  | (_,[]) | ([],_) -> failwith("Different lengths.")
  | (hd1::tl1,hd2::tl2) -> (hd1,hd2)::(zip tl1 tl2)

(* The function "zip_enum_tlrec l1 l2" accepts lists l1 = [l1_0; l1_1; l1_2; ...] 
 and l2 = [l2_0; l2_1; l2_2; ...] and returns [(0, l1_0, l2_0); (1, l1_1, l2_1); ...].
 The function is tail recursive..
 If the lenght of lists l1 and l2 doesn't match it fails.
 ----------
 # zip_enum_tlrec ["a"; "b"; "c"; "d"] [7; 3; 4; 2];;
 - : (int * string * int) list = [(0, "a", 7); (1, "b", 3); (2, "c", 4); (3, "d", 2)]
 ---------- *)

let zip_enum_tlrec l1 l2 =
  let rec zip_enum_aux l1 l2 index acc =
    match (l1, l2) with
    | ([],[]) -> acc
    | (_,[]) | ([],_) -> failwith("Lengths not matching.")
    | (hd1::tl1,hd2::tl2) ->
      let new_head = (index,hd1,hd2) in
      zip_enum_aux tl1 tl2 (index+1) (new_head::acc)
   in
   zip_enum_aux l1 l2 0 [] |> reverse

(* The function "unzip l" accepts a list l = [(a0, b0); (a1, b2); ...]
 and returns the pair ([a0; a1; ...], [b0; b1; ...]).
 ----------
 # unzip [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
 ---------- *)

let rec unzip = function
  | [] -> ([], [])
  | (x,y)::tl -> let (l1,l2) = unzip tl in (x::l1, y::l2)

(* The function "unzip_tlrec l" is the tail recursive version of unzip.
 ----------
 # unzip_tlrec [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
 ---------- *)

let unzip_tlrec l =
  let rec unzip_tlrec_aux l acc1 acc2 =
    match l with
    | [] -> (reverse acc1, reverse acc2)
    | (x,y)::tl -> unzip_tlrec_aux tl (x::acc1) (y::acc2)
  in
  unzip_tlrec_aux l [] []

(* The function "fold_left_no_acc f l" accepts a list l = [l0; l1; l2; ...; ln] and function f
 and returns the value of f(... (f (f (f l0 l1) l2) l3) ... ln).
 If the list has less than two elements it fails.
 ----------
 # fold_left_no_acc (^) ["F"; "I"; "C"; "U"; "S"];;
 - : string = "FICUS"
 ---------- *)

let rec fold_left_no_acc f = function
  | [] | _::[] -> failwith "List too short."
  | x::y::[] -> f x y
  | x::y::tl -> fold_left_no_acc f ((f x y) :: tl)

(* The function "apply_sequence f x n" returns the list of repeated applications 
 of the function f on x, [x; f x; f (f x); ...; f applied n times on x].
 The function is tail recursive.
 ----------
 # apply_sequence (fun x -> x*x) 2 5;;
 - : int list = [2; 4; 16; 256; 65536; 4294967296]
 # apply_sequence (fun x -> x*x) 2 (-5);;
 - : int list = []
 ---------- *)

let apply_sequence f x n =
  let rec apply_sequence_aux f x n acc =
    if n < 0
    then reverse acc
    else apply_sequence_aux f (f x) (n-1) (x::acc)
  in
  apply_sequence_aux f x n []


(* The function "filter f l" returns the list of elements for which 
 (f x) equals true.
 ----------
 # filter ((<)3) [0; 1; 2; 3; 4; 5];;
 - : int list = [4; 5]
 ---------- *)

let rec filter f = function
  | [] -> []
  | hd::tl -> if f hd then hd::(filter f tl) else filter f tl


(* The function "exists f l" checks if there exists an element of the list l
 for which the function f returns true, otherwise it returns false.
 The function is tail recursive.
 ----------
 # exists ((<)3) [0; 1; 2; 3; 4; 5];;
 - : bool = true
 # exists ((<)8) [0; 1; 2; 3; 4; 5];;
 - : bool = false
 ---------- *)

let rec exists f = function
  | [] -> false
  | hd::tl -> if f hd then true else exists f tl

(* The function "first f none_value l" returns the first element of the list l
 for which f returns true. If such an element does not exist it returns none_value.
 The function is tail recursive.
 ----------
 # first ((<)3) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 5
 # first ((<)8) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 0
 ---------- *)

let rec first f none_value = function
  | [] -> none_value
  | hd::tl -> if f hd then hd else first f none_value tl
  
(* The northerners are attacking Middlebirch. As the archwizard you know the sequence
 of spells needed to protect Middlebirch. The sequence of spells is written as a list
 [("name1", value1); ("name2", value2); ...].
 At your disposal is a band of wizards, who are represented as a list
 [("name1", ability1); ("name2", ability2); ...].
 A wizard can cast the sequence of spells only if his ability is greater or equal to
 the combined value of all spells in the sequence.

 The function "able_protectors spells wizards" returns a list of names of all the
 wizards able to protect Middlebirch on their own.

 The function "fails_on spells wizards" returns a list of pairs (wizard, failed spell),
 where the failed spell is the first spell in the sequence for which the wizard has 
 insufficient ability. The ability to cast all spells is represented as an empty string 
 for the failed spell.
 
 Hint: A good wizard uses his knowledge and experiences gained during his studies.
 
 ----------
 # let spells = [("Protect",51); ("Renounce", 17); ("Blaze", 420); ("Banish",103)] in
   let wizards = [("Merlin", 1832); ("Frodo", 53); ("Ajitam", 1337);
  ("Mr Duck", 7); ("Kylo Ren", 589); ("Snoop Dogg", 123)] in
   able_protectors spells wizards;;
 - : string list = ["Merlin"; "Ajitam"]
 ----------
 # let spells = [("Protect",51); ("Renounce", 17); ("Blaze", 420); ("Banish",103)] in
   let wizards = [("Merlin", 1832); ("Frodo", 53); ("Ajitam", 1337);
  ("Mr Duck", 7); ("Kylo Ren", 589); ("Snoop Dogg", 123)] in
   fails_on spells wizards;;
 - : (string * string) list = [("Merlin", ""); ("Frodo", "Renounce"); ("Ajitam", "");
  ("Mr Duck", "Protect"); ("Kylo Ren", "Banish"); ("Snoop Dogg", "Blaze")]
 ----------*)

let able_protectors spells wizards =
  (* Unzip to sum all costs. *)
  let (_, spell_values) = unzip spells in
  (* 0::0::spell_values added so that an empty list sums to 0. *)
  let sequence_cost = fold_left_no_acc (+) (0::0::spell_values) in
  (* Make filter function. *)
  let can_cast (_, wizard_ability) = (wizard_ability >= sequence_cost) in
  (* Get only the wizards who can cast. *)
  let mighty_wizards = filter can_cast wizards in
  (* Extract names *)
  let (mighty_wizard_names, _) = unzip_tlrec mighty_wizards in
  mighty_wizard_names

let fails_on spells wizards =
  (* Write auxiliary function that determines the first uncastable spell. *)
  let rec gets_stuck spells wizard_ability =
    match spells with
    | [] -> ""
    | (spell_name, spell_value)::tl ->
      if wizard_ability >= spell_value
      then gets_stuck tl (wizard_ability - spell_value)
      else spell_name
  in
  (* Unzip to get a list of wizard ability values. *)
  let (wizard_names, wizard_abilities) = unzip wizards in
  (* Create a list of first uncastable spells. *)
  let stuck_spells = map (gets_stuck spells) wizard_abilities in
  (* Zip it back together. *)
  zip wizard_names stuck_spells
