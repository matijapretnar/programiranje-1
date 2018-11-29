module type Barva = sig
  type t

  val po_imenu : string -> t
  val posvetli : t -> t
  val potemni : t -> t

  val hex : t -> string
end

module RGB : Barva = struct
  type t = { r : float; g : float; b : float }

  let po_imenu = function
  | "bela" -> {r = 1.; g = 1.; b = 1.}
  | "rdeca" -> {r = 1.; g = 0.; b = 0.}
  | "zelena" -> {r = 0.; g = 1.; b = 0.}
  | "modra" -> {r = 0.; g = 0.; b = 1.}
  | "crna" -> {r = 0.; g = 0.; b = 0.}

  let zmesaj barva1 barva2 = {
    r = (barva1.r +. barva2.r) /. 2.;
    g = (barva1.g +. barva2.g) /. 2.;
    b = (barva1.b +. barva2.b) /. 2.;
  }

  let posvetli = zmesaj {r = 1.; g = 1.; b = 1.}
  let potemni = zmesaj {r = 0.; g = 0.; b = 0.}

  let hex barva =
    let zaokrozi x = truncate (255. *. x) in
    Format.sprintf "#%02x%02x%02x"
      (zaokrozi barva.r)
      (zaokrozi barva.g)
      (zaokrozi barva.b)
end
