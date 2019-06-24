type tenant = string

type plot = Farmed of tenant
          | Sublet of tenant * (plot * plot list)
          | Vacant

let p = Sublet ("Hannah" , (Farmed "Ian", Vacant :: [Farmed "Chris"] ))

let farmed_opt = function Farmed x -> Some x | _ -> None

let rec depth = function
  | Farmed _
  | Vacant -> 0
  | Sublet (_, (subletter, subletters)) ->
    1 + List.fold_left max (depth subletter) (List.map depth subletters)

let rec plot_unused = function
  | Farmed _ -> false
  | Vacant -> true
  | Sublet (tenant, (subletter, subletters)) ->
    plot_unused subletter && List.for_all plot_unused subletters

let rec tenants = function
  | Farmed x -> [x]
  | Vacant -> []
  | Sublet (tenant, (subletter, subletters)) ->
    tenant :: (List.fold_left (fun acc plot -> tenants plot @ acc) [] (subletter :: subletters))

let rec farmers = function
  | Farmed x -> [x]
  | Vacant -> []
  | Sublet (_, (subletter, subletters)) ->
    List.fold_left (fun acc plot -> farmers plot @ acc) [] (subletter :: subletters)
