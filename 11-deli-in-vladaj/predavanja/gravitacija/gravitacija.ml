open Vdom
open Vektor

module Parametri = struct
  let barnes_hut_faktor = 0.5
  let gravitacijska_konstanta = 6.67430
  let delta_t = 0.05
  let stevilo_planetov = 1000
  let max_masa = 50.
end

module Telo = struct
  type t = { polozaj : Vektor.t; hitrost : Vektor.t; masa : float }

  let gravitacijska_sila telo1 telo2 =
    let smer = telo2.polozaj --. telo1.polozaj in
    let r = dolzina smer in
    if r = 0. then izhodisce
    else
      let velikost =
        Parametri.gravitacijska_konstanta *. telo1.masa *. telo2.masa /. (r *. r)
      in
      velikost **. normiraj smer

  let premakni t sila telo =
    let pospesek = (1. /. telo.masa) **. sila in
    let hitrost = telo.hitrost ++. (t **. pospesek) in
    let polozaj = telo.polozaj ++. (t **. hitrost) in
    { telo with polozaj; hitrost }

  let narisi telo = svg_elt "g" [ Svg.svg_krog telo.polozaj (sqrt telo.masa) ]
  let stacionarno polozaj masa = { polozaj; hitrost = izhodisce; masa }
end

module type VESOLJE = sig
  type t

  val prazno : Vektor.t -> Vektor.t -> t
  val posodobi : float -> t -> t
  val vstavi : Telo.t -> t -> t
  val fold_left : ('acc -> Telo.t -> 'acc) -> 'acc -> t -> 'acc
  val narisi : t -> 'a Vdom.vdom list
  val iz_teles : Telo.t list -> t
end

module Naivno : VESOLJE = struct
  type t = Telo.t list

  let prazno _ _ = []

  let posodobi_telo t telesa telo =
    let sila =
      List.fold_left
        (fun sila telo' ->
          if telo = telo' then sila
          else sila ++. Telo.gravitacijska_sila telo telo')
        izhodisce telesa
    in
    Telo.premakni t sila telo

  let posodobi t telesa = List.map (posodobi_telo t telesa) telesa
  let vstavi telo telesa = telo :: telesa
  let fold_left = List.fold_left
  let narisi telesa = List.map Telo.narisi telesa
  let iz_teles telesa = telesa
end

module BarnesHut : VESOLJE = struct
  type t = {
    min_xy : Vektor.t;
    max_xy : Vektor.t;
    tezisce : Telo.t;
    telesa : telesa;
  }

  and telesa = Prazno | Telo of Telo.t | Kvadranti of t * t * t * t

  let prazno min_xy max_xy =
    {
      min_xy;
      max_xy;
      tezisce =
        {
          polozaj = Vektor.sredina min_xy max_xy;
          masa = 0.;
          hitrost = izhodisce;
        };
      telesa = Prazno;
    }

  let rec vstavi telo vesolje =
    match vesolje.telesa with
    | Prazno ->
        let tezisce = Telo.(stacionarno telo.polozaj telo.masa)
        and telesa = Telo telo in
        { vesolje with tezisce; telesa }
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
        let skupna_masa = vesolje.tezisce.masa +. telo.masa in
        let polozaj_tezisca =
          (1. /. skupna_masa)
          **. ((vesolje.tezisce.masa **. vesolje.tezisce.polozaj)
              ++. (telo.masa **. telo.polozaj))
        in
        let tezisce = Telo.stacionarno polozaj_tezisca skupna_masa in
        let telesa =
          match (telo.polozaj.x <= sredina.x, telo.polozaj.y <= sredina.y) with
          | true, true -> Kvadranti (zl, zd, vstavi telo sl, sd)
          | false, true -> Kvadranti (zl, zd, sl, vstavi telo sd)
          | true, false -> Kvadranti (vstavi telo zl, zd, sl, sd)
          | false, false -> Kvadranti (zl, vstavi telo zd, sl, sd)
        in
        { vesolje with tezisce; telesa }

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
    | Telo telo' -> Telo.gravitacijska_sila telo telo'
    | Kvadranti (zl, zd, sl, sd) ->
        if
          razdalja vesolje.min_xy vesolje.max_xy
          < Parametri.barnes_hut_faktor
            *. razdalja telo.polozaj vesolje.tezisce.polozaj
        then Telo.gravitacijska_sila telo vesolje.tezisce
        else
          sila_vesolja zl telo ++. sila_vesolja zd telo ++. sila_vesolja sl telo
          ++. sila_vesolja sd telo

  let posodobi_telo t vesolje telo =
    let sila = sila_vesolja vesolje telo in
    Telo.premakni t sila telo

  let iz_teles telesa =
    let min_xy =
      List.fold_left
        (fun m telo ->
          { x = min m.x telo.Telo.polozaj.x; y = min m.y telo.polozaj.y })
        izhodisce telesa
    and max_xy =
      List.fold_left
        (fun m telo ->
          { x = max m.x telo.Telo.polozaj.x; y = max m.y telo.polozaj.y })
        izhodisce telesa
    in
    let vesolje = prazno min_xy max_xy in
    List.fold_left (fun vesolje telo -> vstavi telo vesolje) vesolje telesa

  let posodobi t vesolje =
    let telesa =
      fold_left
        (fun telesa telo -> posodobi_telo t vesolje telo :: telesa)
        [] vesolje
    in
    iz_teles telesa

  let rec narisi vesolje =
    Svg.svg_pravokotnik vesolje.min_xy vesolje.max_xy
    ::
    (match vesolje.telesa with
    | Prazno -> []
    | Telo telo -> [ Telo.narisi telo ]
    | Kvadranti (zl, zd, sl, sd) ->
        narisi zl @ narisi zd @ narisi sl @ narisi sd)
end

module Vesolje : VESOLJE = BarnesHut

type 'msg Vdom.Cmd.t += After of int * 'msg
type model = { vesolje : Vesolje.t; sirina : float; visina : float }
type msg = Posodobi

let init sirina visina =
  let sonce =
    Telo.stacionarno { x = Random.float sirina; y = Random.float visina } 1000.
  in
  let planeti =
    List.init Parametri.stevilo_planetov (fun _ ->
        Telo.stacionarno
          { x = Random.float sirina; y = Random.float visina }
          (Random.float Parametri.max_masa))
  in
  return
    ~c:[ After (30, Posodobi) ]
    { vesolje = Vesolje.iz_teles (sonce :: planeti); sirina; visina }

let update model msg =
  match msg with
  | Posodobi ->
      return
        ~c:[ After (30, Posodobi) ]
        {
          model with
          vesolje = Vesolje.posodobi Parametri.delta_t model.vesolje;
        }

let view model =
  div
    [
      svg_elt "svg"
        ~a:
          [
            Svg.int_of_float_attr "width" model.sirina;
            Svg.int_of_float_attr "height" model.visina;
            Svg.int_of_float_attr "stroke-width" Svg.Parametri.debelina_crt;
            onmousemove (fun _ -> Posodobi);
          ]
        (Vesolje.narisi model.vesolje);
    ]

let cmd_handler ctx = function
  | After (n, msg) ->
      ignore
        (Js_browser.Window.set_timeout Js_browser.window
           (fun () -> Vdom_blit.Cmd.send_msg ctx msg)
           n);
      true
  | _ -> false

let () = Vdom_blit.(register (cmd { f = cmd_handler }))

let () =
  let open Js_browser in
  let width = Js_browser.Window.inner_width Js_browser.window in
  let height = Js_browser.Window.inner_height Js_browser.window in
  let app = app ~init:(init width height) ~view ~update () in

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
