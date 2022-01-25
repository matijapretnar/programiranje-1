type 'a pot = Korak of 'a * 'a * int | Stik of 'a pot * 'a pot * int

let cena = function Korak (_, _, c) -> c | Stik (_, _, c) -> c

let koraki_poti pot =
  let rec aux pot koraki =
    match pot with
    | Korak (i, j, _) -> (i + 1, j + 1) :: koraki
    | Stik (pot1, pot2, _) -> aux pot1 (aux pot2 koraki)
  in
  aux pot []

let stakni zacetek konec = Stik (zacetek, konec, cena zacetek + cena konec)

let najdrazja poti =
  let drazja pot1 pot2 = if cena pot1 >= cena pot2 then pot1 else pot2 in
  Array.fold_right drazja poti poti.(0)

let dim mat = (Array.length mat, Array.length mat.(0))

let zmnozi mat1 mat2 =
  let (m, n), (n', p) = (dim mat1, dim mat2) in
  assert (n = n');
  Array.init m (fun i ->
      Array.init p (fun j ->
          najdrazja (Array.init n (fun k -> stakni mat1.(i).(k) mat2.(k).(j)))))

let rec potenciraj k mat =
  match k with
  | k when k <= 0 -> invalid_arg "potenciraj"
  | 1 -> mat
  | k ->
      let mat2 = zmnozi mat mat in
      let potenca = potenciraj (k / 2) mat2 in
      if k mod 2 = 0 then potenca else zmnozi mat potenca

let trololo cene m =
  let n = Array.length cene in
  if m = 0 then []
  else
    Array.init n (fun i -> Array.init n (fun j -> Korak (i, j, cene.(i).(j))))
    |> potenciraj m
    |> Array.map najdrazja
    |> najdrazja
    |> koraki_poti

let cene =[|
  [|5; 25; 3; 2|];
  [|1; 4; 25; 1|];
  [|2; 1; 3; 2|];
  [|4; 1; 40; 5|]
|]

let test m = ignore (trololo cene m); print_endline (string_of_int m)

  ;;

test 10;
test 100;
test 1000;
test 10000;
test 100000;
test 1000000;
test 10000000;
test 100000000;
