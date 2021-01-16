(*
  Imamo rdeče kocke velikosti 1 in 2 ter modre kocke velikosti 2 in 3.
  Koliko različnih stolpov, v katerih se izmenjujejo modre in rdeče kocke lahko sestavimo?
*)

let rec rdeci = function
  | 0 -> 1
  | n when n < 0 -> 0
  | n -> Format.printf "M%d-" n; modri (n - 1) + modri (n - 2)

and modri = function
  | 0 -> 1
  | n when n < 0 -> 0
  | n -> Format.printf "R%d-" n; rdeci (n - 2) + rdeci (n - 3)

let stolpi = function
  | 0 -> 1
  | n -> modri n + rdeci n

let odviti_rdeci _ modri = function
  | 0 -> 1
  | n when n < 0 -> 0
  | n -> Format.printf "R%d-" n; modri (n - 1) + modri (n - 2)

let odviti_modri rdeci _ = function
  | 0 -> 1
  | n when n < 0 -> 0
  | n -> Format.printf "M%d-" n; rdeci (n - 2) + rdeci (n - 3)

let find_opt hashtbl x =
    try
        Some (Hashtbl.find hashtbl x)
    with
        Not_found -> None

let memoiziraj_rec2 odviti_f odviti_g =
  let rezultati_f = Hashtbl.create 512 in
  let rezultati_g = Hashtbl.create 512 in
  let rec mem_f x =
    match find_opt rezultati_f x with
    | None ->
        let y = odviti_f mem_f mem_g x in
        Hashtbl.add rezultati_f x y;
        y
    | Some y ->
        y
  and mem_g x =
    match find_opt rezultati_g x with
    | None ->
        let y = odviti_g mem_f mem_g x in
        Hashtbl.add rezultati_g x y;
        y
    | Some y ->
        y
  in
  (mem_f, mem_g)

let mem_rdeci, mem_modri = memoiziraj_rec2 odviti_rdeci odviti_modri

let mem_rdeci', mem_modri' = memoiziraj_rec2
(fun _ modri -> function
  | 0 -> 1
  | n when n < 0 -> 0
  | n -> Format.printf "R%d-" n; modri (n - 1) + modri (n - 2)
) (fun rdeci _ -> function
  | 0 -> 1
  | n when n < 0 -> 0
  | n -> Format.printf "M%d-" n; rdeci (n - 2) + rdeci (n - 3)
)
