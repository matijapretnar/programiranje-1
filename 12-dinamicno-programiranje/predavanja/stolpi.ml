let rec stevilo_stolpov_rek = function
| 0 -> 1
| n when n < 0 -> 0
| n ->
    let na_dnu_1 = stevilo_stolpov_rek (n - 1)
    and na_dnu_2 = stevilo_stolpov_rek (n - 2)
    and na_dnu_3 = stevilo_stolpov_rek (n - 3)
    in
    na_dnu_1 + na_dnu_2 + na_dnu_3

let stevilo_stolpov_din n =
    let grda_tabela = Array.make (n + 1) 1 in
    grda_tabela.(2) <- 2;
    for i = 3 to n do
      grda_tabela.(i) <-
        grda_tabela.(i - 1)
        + grda_tabela.(i - 2)
        + grda_tabela.(i - 3)
    done;
    grda_tabela.(n)

let stevilo_stolpov n =
   let rec aux a b c = function
   | 0 -> a
   | n -> aux b c (a + b + c) (n - 1)
   in
   aux 1 1 2 n
