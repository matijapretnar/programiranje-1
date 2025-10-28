module NiziSPrimerjavo : Map.OrderedType = struct
  type t = string
  let compare = String.compare
end

module SlovarZNiziZaKljuce = Map.Make(struct
  type t = string
  let compare = String.compare
end)

let _ =
  SlovarZNiziZaKljuce.empty
  |> SlovarZNiziZaKljuce.add "kljuc" 10