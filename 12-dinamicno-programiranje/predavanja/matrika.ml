let matrika = [|
    [|131; 673; 234; 103; 18|];
    [|201; 96; 342; 965; 150|];
    [|630; 803; 746; 422; 111|];
    [|537; 699; 497; 121; 956|];
    [|805; 732; 524; 37; 331|]
|]

let izpisi_matriko =
  Array.iter (fun vrstica ->
    Array.iter (fun n -> print_int n; print_string " ") vrstica;
    print_endline ""
  )

let najcenejsa_pot matrika =
    let vrstice = Array.length matrika
    and stolpci = Array.length matrika.(0) in
    (* matrika cen od polja (i, j) do spodaj desno *)
    let cene = Array.make_matrix (vrstice + 1) (stolpci + 1) (99999) in
    let poti = Array.make_matrix (vrstice + 1) (stolpci + 1) [] in
    for vrstica = vrstice - 1 downto 0 do
        for stolpec = stolpci - 1 downto 0 do
            izpisi_matriko cene;
            print_endline "-----------------";
            let cena_dol = cene.(vrstica + 1).(stolpec)
            and cena_desno = cene.(vrstica).(stolpec + 1) in
            let pot_dol = poti.(vrstica + 1).(stolpec)
            and pot_desno = poti.(vrstica).(stolpec + 1) in
            let (min_cena, min_pot) =
                min (cena_dol, pot_dol) (cena_desno, pot_desno)
            in
            let cena = matrika.(vrstica).(stolpec) + min_cena
            and pot = matrika.(vrstica).(stolpec) :: min_pot
            in
            if vrstica = vrstice - 1 && stolpec = stolpci - 1 then (
                cene.(vrstica).(stolpec) <- matrika.(vrstica).(stolpec);
                poti.(vrstica).(stolpec) <- [matrika.(vrstica).(stolpec)]
            ) else (
                cene.(vrstica).(stolpec) <- cena;
                poti.(vrstica).(stolpec) <- pot
            )
        done
    done;
    cene.(0).(0), poti.(0).(0)
;;

najcenejsa_pot matrika