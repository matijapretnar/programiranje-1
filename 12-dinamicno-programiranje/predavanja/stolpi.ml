let rec stolpi =
  function
  | 0 -> 1
  | n when n < 0 -> 0
  | n -> stolpi (n - 1) + stolpi (n - 2) + stolpi (n - 3)

let stolpi_s_tabelo n = 
  let tabela = Array.make (n + 1) 0 in
  for i = 0 to 2 do
    tabela.(i) <- stolpi i
  done;
  for i = 3 to n do
    tabela.(i) <- tabela.(i - 1) + tabela.(i - 2) + tabela.(i - 3)
  done;
  tabela.(n)

let stolpi_s_3_spremenljivkami n =
  (*
    aux 0 0 1 3 =
    aux 0 1 1 2 =
    aux 1 1 2 1 =
    aux 1 2 4 0 = 4
  *)
  let rec aux a b c =
    function
    | 0 -> c
    | n -> aux b c (a + b + c) (n - 1)
  in
  aux 0 0 1 n


let rec gcd m n =
  if m < n then gcd n m
  else if m mod n = 0 then n
  else gcd n (m mod n)

let rec gcd m n =
  if n = 0 then m else gcd n (m mod n)
