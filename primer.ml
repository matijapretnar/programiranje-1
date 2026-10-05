let rec fakulteta =
  function
  | 0 -> 1
  | n -> n * fakulteta (n - 1)