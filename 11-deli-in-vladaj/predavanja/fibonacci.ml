let rec fib = function
  | 0 -> 0
  | 1 -> 1
  | n -> fib (n - 1) + fib (n - 2)

let fib' =
  let rec aux a b = function
  | 0 -> a
  | n -> aux b (a + b) (n - 1)
  in
  aux 0 1

type matrika = {
    _11: int; _12: int; _21: int; _22: int
}

let zmnozi a b = {
    _11 = a._11 * b._11 + a._12 * b._21;
    _21 = a._21 * b._11 + a._22 * b._21;
    _12 = a._11 * b._12 + a._12 * b._22;
    _22 = a._21 * b._12 + a._22 * b._22
}

let ostanek a m = {
    _11 = a._11 mod m;
    _12 = a._12 mod m;
    _21 = a._21 mod m;
    _22 = a._22 mod m
}

let rec potenciraj a = function
  | 0 -> {_11 = 1; _12 = 0; _21 = 0; _22 = 1}
  | n ->
      let a2 = zmnozi a a in
      let an = potenciraj a2 (n / 2) in
      let an' = if n mod 2 = 0 then an else zmnozi a an in
      ostanek an' 10000000000

let fib'' n =
  let f0 = {_11 = 1; _12 = 1; _21 = 1; _22 = 0} in
  (potenciraj f0 (n - 1))._11

;;

(* print_int (fib 35);
print_int (fib' 35);
 *)
(* print_int (fib'' 112233445566778899) *)
