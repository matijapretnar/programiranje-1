open Model
open Vdom

let _random_fsm n m =
  let states =
    Array.init n (fun i ->
        let name = string_of_int i in
        { name })
  in
  let initial = states.(0)
  and final =
    states |> Array.to_list |> List.filter (fun _ -> Random.float 1.0 < 0.3)
    and transitions =
      List.init m (fun _ ->
          let src = Random.int n in
          let dst = Random.int n in
          let chr = char_of_int (Random.int 26 + int_of_char 'A') in
          (states.(src), chr, states.(dst)))
    in
  { states; initial; final; transitions }

let even_a =
  let even = { name = "E"}
  and odd = { name = "O"}
in
  {
    states = [| even; odd |];
    initial = even;
    final = [ even ];
    transitions = [ (even, 'A', odd); (even, 'B', even); (odd, 'A', even); (odd, 'B', odd) ];
  }

let init width height fsm string =
  let n = Array.length fsm.states in
  let pi = 4. *. atan 1. in
  let origin = { x = width /. 2.; y = height /. 2. } in
  let characters = string |> String.to_seq |> List.of_seq in
  let positions =
    Array.mapi (fun i state ->
        let angle = 2. *. pi *. float_of_int i /. float_of_int n
        and r = (min width height /. 2.) -. (2. *. View.config.loop_radius) in
        (state, origin ++. { x = r *. cos angle; y = r *. sin angle })) fsm.states
        |> Array.to_list
  in
  {
    fsm;
    positions;
    mode = Normal;
    width;
    height;
    characters;
    current_state = fsm.initial;
  }

let app =
  simple_app ~init:(init 500. 500. even_a "ABABA") ~view:View.view ~update ()

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
