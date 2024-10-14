open Vdom
open Model
open Vektor
open Definicije

module Parametri = struct
  let barva_sprejemnega_stanja = "rgb(56, 142, 60)"
  let barva_trenutnega_stanja = "rgb(255, 242, 202)"
  let barva_zacetnega_stanja = "rgb(8, 118, 155)"
  let debelina_crt = 3.
  let dolzina_konice = 10.
  let dolzina_puscice_zacetnega_stanja = 20.
  let naklon_konice = 0.4
  let polmer_oznake = 10.
  let polmer_sprejemnega_stanja = 5.
  let polmer_stanja = 20.
  let polmer_zanke = 25.
  let privzeta_barva_crt = "black"
  let privzeta_barva_polnila = "white"
end

let int_of_float_attr tag value = int_attr tag (int_of_float value)

let svg_krog ?(a = []) sredisce polmer =
  svg_elt "circle"
    ~a:
      ([
         int_of_float_attr "cx" sredisce.x;
         int_of_float_attr "cy" sredisce.y;
         int_of_float_attr "r" polmer;
       ]
      @ a)
    []

let svg_daljica ?(a = []) zacetek konec =
  svg_elt "line"
    ~a:
      ([
         int_of_float_attr "x1" zacetek.x;
         int_of_float_attr "y1" zacetek.y;
         int_of_float_attr "x2" konec.x;
         int_of_float_attr "y2" konec.y;
       ]
      @ a)
    []

let svg_puscica ?(a = []) zacetek konec =
  let vektor = konec --. zacetek in
  let normala = { x = -.vektor.y; y = vektor.x } in
  let vektor_leve_konice =
    sredina ~lambda:Parametri.naklon_konice (-1. **. vektor) normala
  in
  let vektor_desne_konice =
    sredina ~lambda:Parametri.naklon_konice (-1. **. vektor) (-1. **. normala)
  in
  let leva_konica =
    konec ++. (Parametri.dolzina_konice **. normiraj vektor_leve_konice)
  in
  let desna_konica =
    konec ++. (Parametri.dolzina_konice **. normiraj vektor_desne_konice)
  in
  svg_elt "g"
    [
      svg_daljica ~a zacetek konec;
      svg_daljica ~a konec leva_konica;
      svg_daljica ~a konec desna_konica;
    ]

let svg_oznaka ?(a = []) polozaj besedilo =
  svg_elt "text"
    ~a:
      ([
         int_of_float_attr "x" polozaj.x;
         int_of_float_attr "y" polozaj.y;
         attr "text-anchor" "middle";
         attr "dominant-baseline" "central";
       ]
      @ a)
    [ text besedilo ]

let prikaz_stanja model q =
  let avtomat = ZagnaniAvtomat.avtomat model.avtomat in
  let polozaj = polozaj_stanja model q in
  let barva_robu =
    if q = Avtomat.zacetno_stanje avtomat then Parametri.barva_zacetnega_stanja
    else if Avtomat.je_sprejemno_stanje avtomat q then
      Parametri.barva_sprejemnega_stanja
    else Parametri.privzeta_barva_crt
  in
  let barva_polnila =
    if q = ZagnaniAvtomat.stanje model.avtomat then
      Parametri.barva_trenutnega_stanja
    else Parametri.privzeta_barva_polnila
  in
  let svg_elementi =
    [
      svg_krog
        ~a:[ attr "stroke" barva_robu; attr "fill" barva_polnila ]
        polozaj Parametri.polmer_stanja;
      svg_oznaka polozaj (Stanje.v_niz q);
    ]
  in
  let svg_elementi =
    if q = Avtomat.zacetno_stanje avtomat then
      svg_puscica
        ~a:[ attr "stroke" Parametri.barva_zacetnega_stanja ]
        (polozaj
        --. {
              x =
                Parametri.polmer_stanja
                +. Parametri.dolzina_puscice_zacetnega_stanja;
              y = 0.;
            })
        (polozaj --. { x = Parametri.polmer_stanja; y = 0. })
      :: svg_elementi
    else svg_elementi
  in
  let svg_elementi =
    if Avtomat.je_sprejemno_stanje avtomat q then
      svg_elementi
      @ [
          svg_krog
            ~a:
              [
                attr "stroke" Parametri.barva_sprejemnega_stanja;
                attr "fill" "none";
              ]
            polozaj
            (Parametri.polmer_stanja -. Parametri.polmer_sprejemnega_stanja);
        ]
    else svg_elementi
  in

  svg_elt "g"
    ~a:[ onmousedown ~prevent_default:() (fun _ -> ZacniPremikVozlisca q) ]
    svg_elementi

let prikaz_zanke zacetek oznaka =
  let polozaj_oznake =
    zacetek --. { x = 0.; y = 2. *. Parametri.polmer_zanke }
  in
  svg_elt "g"
    [
      svg_puscica
        ~a:[ attr "stroke" Parametri.privzeta_barva_crt ]
        (zacetek ++. { x = Parametri.polmer_stanja +. 2.; y = -3. })
        (zacetek ++. { x = Parametri.polmer_stanja; y = -2. });
      svg_krog
        ~a:[ attr "stroke" Parametri.privzeta_barva_crt; attr "fill" "none" ]
        (zacetek --. { x = 0.; y = Parametri.polmer_zanke })
        Parametri.polmer_zanke;
      svg_krog ~a:[ attr "fill" "white" ] polozaj_oznake Parametri.polmer_oznake;
      svg_oznaka polozaj_oznake oznaka;
    ]

let prikaz_prehoda zacetek konec oznaka =
  let lambda = Parametri.polmer_stanja /. razdalja zacetek konec in
  let zacetek_puscice = sredina ~lambda zacetek konec in
  let konec_puscice = sredina ~lambda:(1. -. lambda) zacetek konec in
  let polozaj_oznake = sredina ~lambda:0.3 zacetek konec in
  svg_elt "g"
    [
      svg_puscica
        ~a:
          [
            int_of_float_attr "stroke-width" Parametri.debelina_crt;
            attr "stroke" Parametri.privzeta_barva_crt;
          ]
        zacetek_puscice konec_puscice;
      svg_krog ~a:[ attr "fill" "white" ] polozaj_oznake Parametri.polmer_oznake;
      svg_oznaka polozaj_oznake oznaka;
    ]

let prikaz_traku model =
  let trak = ZagnaniAvtomat.trak model.avtomat in
  match model.nacin with
  | VnasanjeNiza ->
      elt "h2"
        [
          input
            ~a:[ onchange (fun niz -> VnesiNiz niz); value (Trak.v_niz trak) ]
            [];
        ]
  | _ ->
      let prebrani = Trak.prebrani trak and neprebrani = Trak.neprebrani trak in
      elt "h2"
        ~a:[ ondblclick (fun _ -> ZacniVnosNiza) ]
        [ text prebrani; elt "mark" [ text neprebrani ] ]

let prikaz_gumba_za_naslednji_znak model =
  elt "a"
    ~a:
      [
        attr "role" "button";
        attr "href" "#";
        onclick (fun _ -> PreberiNaslednjiZnak);
        disabled
          (model.nacin = VnasanjeNiza
          || model.avtomat |> ZagnaniAvtomat.trak |> Trak.je_na_koncu);
      ]
    [ text "preberi naslednji znak" ]

let prikaz_avtomata model =
  let avtomat = ZagnaniAvtomat.avtomat model.avtomat in
  let stanja =
    List.map (prikaz_stanja model) (avtomat |> Avtomat.seznam_stanj)
  in
  let prehodi =
    List.map
      (fun (src, chr, dst) ->
        let svg_oznaka = String.make 1 chr in
        if src = dst then prikaz_zanke (polozaj_stanja model src) svg_oznaka
        else
          prikaz_prehoda (polozaj_stanja model src) (polozaj_stanja model dst)
            svg_oznaka)
      (Avtomat.seznam_prehodov avtomat)
  in
  let a =
    match model.nacin with
    | PremikanjeVozlisca _ ->
        [
          onmousemove (fun ev ->
              PremakniVozlisce
                { x = Lazy.force ev.element_x; y = Lazy.force ev.element_y });
          onmouseup (fun _ -> KoncajPremikVozlisca);
        ]
    | _ -> []
  in
  svg_elt "svg"
    ~a:
      (a
      @ [
          int_of_float_attr "width" model.sirina;
          int_of_float_attr "height" model.visina;
          int_of_float_attr "stroke-width" Parametri.debelina_crt;
        ])
    (prehodi @ stanja)

let view model =
  elt "article"
    [
      elt "header" [ prikaz_traku model; prikaz_gumba_za_naslednji_znak model ];
      prikaz_avtomata model;
    ]
