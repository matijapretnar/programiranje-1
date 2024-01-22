open Vdom
open Vektor

type telo = { polozaj : Vektor.t; hitrost : Vektor.t; masa : float }

let gravitacijska_sila telo1 telo2 =
  let g = 6.67430 in
  let smer = telo2.polozaj --. telo1.polozaj in
  let r = dolzina smer in
  let velikost = g *. telo1.masa *. telo2.masa /. (r *. r) in
  velikost **. normiraj smer

let premakni_telo t sila telo =
  let pospesek = (1. /. telo.masa) **. sila in
  let hitrost = telo.hitrost ++. (t **. pospesek) in
  let polozaj = telo.polozaj ++. (t **. hitrost) in
  { telo with polozaj; hitrost }

module type VESOLJE = sig
  type t

  val prazno : Vektor.t -> Vektor.t -> t
  val posodobi : float -> t -> t
  val vstavi : telo -> t -> t
  val fold_left : ('a -> telo -> 'a) -> 'a -> t -> 'a
end

module EnostavnoVesolje : VESOLJE = struct
  type t = telo list

  let prazno _ _ = []

  let posodobi_telo t telesa telo =
    let sila =
      List.fold_left
        (fun sila telo' ->
          if telo = telo' then sila else sila ++. gravitacijska_sila telo telo')
        izhodisce telesa
    in
    premakni_telo t sila telo

  let posodobi t telesa = List.map (posodobi_telo t telesa) telesa
  let vstavi telo telesa = telo :: telesa
  let fold_left = List.fold_left
end

module BarnesHut : VESOLJE = struct
  type t = {
    min_xy : Vektor.t;
    max_xy : Vektor.t;
    tezisce : Vektor.t;
    skupna_masa : float;
    telesa : telesa;
  }

  and telesa = Prazno | Telo of telo | Kvadranti of t * t * t * t

  let prazno min_xy max_xy =
    {
      min_xy;
      max_xy;
      tezisce = Vektor.sredina min_xy max_xy;
      skupna_masa = 0.;
      telesa = Prazno;
    }

  let rec vstavi telo vesolje =
    match vesolje.telesa with
    | Prazno ->
        let tezisce = telo.polozaj
        and skupna_masa = telo.masa
        and telesa = Telo telo in
        { vesolje with tezisce; skupna_masa; telesa }
    | Telo telo' ->
        let sredina = Vektor.sredina vesolje.min_xy vesolje.max_xy in
        let min_x, sredina_x, max_x =
          (vesolje.min_xy.x, sredina.x, vesolje.max_xy.x)
        and min_y, sredina_y, max_y =
          (vesolje.min_xy.y, sredina.y, vesolje.max_xy.y)
        in
        let zl =
          prazno { x = min_x; y = sredina_y } { x = sredina_x; y = max_y }
        and zd = prazno sredina vesolje.max_xy
        and sl = prazno vesolje.min_xy sredina
        and sd =
          prazno { x = sredina_x; y = min_y } { x = max_x; y = sredina_y }
        in
        {
          (prazno vesolje.min_xy vesolje.max_xy) with
          telesa = Kvadranti (zl, zd, sl, sd);
        }
        |> vstavi telo' |> vstavi telo
    | Kvadranti (zl, zd, sl, sd) ->
        let sredina = Vektor.sredina vesolje.min_xy vesolje.max_xy in
        let skupna_masa = vesolje.skupna_masa +. telo.masa in
        let tezisce =
          (1. /. skupna_masa)
          **. ((vesolje.skupna_masa **. vesolje.tezisce)
              ++. (telo.masa **. telo.polozaj))
        in
        let telesa =
          match (telo.polozaj.x <= sredina.x, telo.polozaj.y <= sredina.y) with
          | true, true -> Kvadranti (vstavi telo zl, zd, sl, sd)
          | false, true -> Kvadranti (zl, vstavi telo zd, sl, sd)
          | true, false -> Kvadranti (zl, zd, vstavi telo sl, sd)
          | false, false -> Kvadranti (zl, zd, sl, vstavi telo sd)
        in
        { vesolje with tezisce; skupna_masa; telesa }

  let rec fold_left f acc vesolje =
    match vesolje.telesa with
    | Prazno -> acc
    | Telo t -> f acc t
    | Kvadranti (zl, zd, sl, sd) ->
        let acc = fold_left f acc zl in
        let acc = fold_left f acc zd in
        let acc = fold_left f acc sl in
        let acc = fold_left f acc sd in
        acc

  let rec sila_vesolja vesolje telo =
    match vesolje.telesa with
    | Prazno -> izhodisce
    | Telo telo' -> gravitacijska_sila telo telo'
    | Kvadranti (zl, zd, sl, sd) ->
        sila_vesolja zl telo ++. sila_vesolja zd telo ++. sila_vesolja sl telo
        ++. sila_vesolja sd telo

  let posodobi_telo t vesolje telo =
    let sila = sila_vesolja vesolje telo in
    premakni_telo t sila telo

  let posodobi t vesolje =
    let vesolje' = prazno vesolje.min_xy vesolje.max_xy in
    fold_left
      (fun vesolje' telo ->
        let telo' = posodobi_telo t vesolje telo in
        vstavi telo' vesolje')
      vesolje' vesolje
end

module Vesolje : VESOLJE = BarnesHut

type model = { vesolje : Vesolje.t }
type msg = Posodobi

let init sirina visina =
  let sredina = { x = sirina /. 2.; y = visina /. 2. } in
  let vesolje = Vesolje.prazno izhodisce (2. **. sredina) in
  let sonce = { polozaj = sredina; hitrost = izhodisce; masa = 100. } in
  let zvezde =
    List.map
      (fun polozaj ->
        {
          polozaj = sredina ++. (20. **. polozaj);
          hitrost = izhodisce;
          masa = 1.;
        })
      (koreni_enote 100)
  in
  { vesolje = List.fold_right Vesolje.vstavi (sonce :: zvezde) vesolje }

let update stari_model msg =
  match msg with
  | Posodobi -> { vesolje = Vesolje.posodobi 0.1 stari_model.vesolje }

let narisi_telo telo =
  svg_elt "g"
    [
      Svg.svg_krog telo.polozaj (sqrt telo.masa);
      Svg.svg_puscica
        ~a:
          [
            Svg.int_of_float_attr "stroke-width"
              (Svg.Parametri.debelina_crt /. 2.);
            attr "stroke" "green";
          ]
        telo.polozaj
        (telo.polozaj ++. telo.hitrost);
    ]

let view model =
  div
    [
      svg_elt "svg"
        ~a:
          [
            Svg.int_of_float_attr "width" 600.;
            Svg.int_of_float_attr "height" 600.;
            Svg.int_of_float_attr "stroke-width" Svg.Parametri.debelina_crt;
            attr "stroke" "black";
            onmousemove (fun _ -> Posodobi);
          ]
        (Vesolje.fold_left
           (fun elts telo -> narisi_telo telo :: elts)
           [] model.vesolje);
    ]

let app = simple_app ~init:(init 500. 500.) ~view ~update ()

let () =
  let open Js_browser in
  let run () =
    Vdom_blit.run app |> Vdom_blit.dom
    |> Js_browser.Element.append_child
         (match
            Js_browser.Document.get_element_by_id Js_browser.document
              "container"
          with
         | Some element -> element
         | None -> Js_browser.Document.document_element Js_browser.document)
  in
  Window.set_onload window run
