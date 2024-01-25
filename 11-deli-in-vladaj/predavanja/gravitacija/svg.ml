open Vdom
open Vektor

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

let svg_pravokotnik ?(a = []) min max =
  svg_elt "g"
    [
      svg_daljica ~a min { x = max.x; y = min.y };
      svg_daljica ~a { x = max.x; y = min.y } max;
      svg_daljica ~a max { x = min.x; y = max.y };
      svg_daljica ~a { x = min.x; y = max.y } min;
    ]

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
