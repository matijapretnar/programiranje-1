type najemnik = string

type vrt = Obdelovan of najemnik
          | Oddan of najemnik * (vrt * vrt list)
          | Prost

let vrt_primer = Oddan ("Kovalevskaya" ,
                  (Obdelovan "Galois",
                   Prost ::
                   [Obdelovan "Lagrange"] ))

let obdelovalec_vrta = function Obdelovan x -> Some x | _ -> None

let rec globina_oddajanja = function
  | Obdelovan _
  | Prost -> 0
  | Oddan (_, (podvrt, podvrtovi)) ->
    let globine = (List.map globina_oddajanja podvrtovi) in
    1 + List.fold_left max (globina_oddajanja podvrt) globine

let rec v_uporabi = function
  | Obdelovan _ -> true
  | Prost -> false
  | Oddan (oddajalec, (podvrt, podvrtovi)) ->
    v_uporabi podvrt && List.exists v_uporabi podvrtovi

let rec vsi_najemniki = function
  | Obdelovan vrtickar -> [vrtickar]
  | Prost -> []
  | Oddan (oddajalec, (podvrt, podvrtovi)) ->
    let vsi_podnajemniki =
      List.fold_left (fun acc vrt -> vsi_najemniki vrt @ acc) [] podvrtovi
    in
    oddajalec :: vsi_najemniki podvrt @ vsi_podnajemniki

let rec vsi_obdelovalci = function
  | Obdelovan vrtickar -> [vrtickar]
  | Prost -> []
  | Oddan (_, (podvrt, podvrtovi)) ->
    List.fold_left
      (fun acc vrt -> vsi_obdelovalci vrt @ acc) [] (podvrt :: podvrtovi)
