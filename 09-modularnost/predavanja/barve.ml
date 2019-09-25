module type Barva = sig
  type t
  val po_imenu : string -> t
  val zmesaj : t -> t -> t
  val potemni : t -> t
end

module RGB : Barva = struct
  type t = {r : float; g : float; b : float}

  let po_imenu = function
  | "rdeca" -> {r = 1.; g = 0.; b = 0.}
  | "zelena" -> {r = 0.; g = 1.; b = 0.}
  | "modra" -> {r = 0.; g = 0.; b = 1.}
  | neznana_barva ->
      print_endline ("Barve " ^ neznana_barva ^ " ne poznam!");
      print_endline "Izmislil si bom naključno barvo!";
      { r = Random.float 1.; g = Random.float 1.; b = Random.float 1. }

  let (@+@) x y = (x +. y) /. 2.

  let zmesaj barva1 barva2 = {
      r = barva1.r @+@ barva2.r;
      g = barva1.g @+@ barva2.g;
      b = barva1.b @+@ barva2.b;
  }

  let potemni = zmesaj {r = 0.; g = 0.; b = 0.}
end

(* module BarveZBesedami : Barva = struct
  type t = string

  let po_imenu ime = ime

  let potemni barva = "temno " ^ barva

  let zmesaj barva1 barva2 = barva1 ^ "-" ^ barva2
end *)