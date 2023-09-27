let f sez_ref =
  sez_ref := 1 :: !sez_ref;
  List.length !sez_ref

let g sez_ref = f sez_ref + f sez_ref
