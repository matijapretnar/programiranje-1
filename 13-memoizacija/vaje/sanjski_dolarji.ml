(* Vaja povečini povzeta iz zapiskov predavanj Jeffa Ericksona. *)

(* V prejšnjem življenju ste delali kot blagajničar v pozabljeni antarktični
   koliniji Nadira, kjer ste večino dneva porabili za vračanje drobiža kupcem.
   Ker je papir redek na antarktiki so blagajničarji morali po zakonu vračati
   najmanjše možno število bankovcev kot drobiž. Po zaslugi numerološke
   navdušenosti enega od ustanoviteljev Nadire je denar, imenovan sanjski
   dolarji, na voljo v naslednjih bankovcih: $1, $4, $7, $13, $28, $52, $91, $365.
*)

let denominations = [1; 4; 7; 13; 28; 52; 91; 365]


(* 0.i) Matematično formulirajte problem. *)

(*
   Denimo, da moramo vrniti število n, ...
*)


(* 0.ii) Opišite rekurzijo za problem. *)

(*
   Denimo, da moramo vrniti število n, ...
*)


(* 1. Požrešni algoritem zaporedno jemlje največji možni bankovec, ki ga lahko
   vrne brez da preseže vrednost. Naprimer za $122 najprej vrne $91 nato $28
   in končno še tri $1 bankovce.

   Poskusite poiskati primer, kjer to ni optimalno.

   Namig: Iskanje primera ni tako preprosto. Del lahko izpustite dokler ne
   implementirate natančne rešitve, ki jo nato primerjate z požrešno.
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

(* 2.i) Opišite in analizirajte rekurzivni algoritem, ki za dani k izračuna
   najmanjši možni nabor bankovcev, potrebnih za vračanje k sanjskih dolarjev.
   (Na tej točki hitrost algoritma ni pomembna.)
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


(* 2.ii) Narišite drevo rekurzivnih klicov za n=5 in ugotovite kateri
   podproblemi se ponavljajo. Ali lahko poiščete vrstni red, ki zgradi
   rešitev od spodaj navzgor? *)



(* 2.iii) Napišite algoritem s pomočjo dinamičnega programiranja, ki za dano
   število k izračuna najmanjši možni nabor bankovcev, potrebnih za vračanje k
   sanjskih dolarjev.
   (Tokrat je hitrost pomembna.)
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
  sols
