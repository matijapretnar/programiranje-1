def fakulteta : Nat → Nat
  | 0 => 1
  | n + 1 => (n + 1) * fakulteta n

#eval fakulteta 5
