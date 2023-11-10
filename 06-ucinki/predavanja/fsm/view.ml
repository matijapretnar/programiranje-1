open Vdom
open Model

let int_of_float_attr tag value = int_attr tag (int_of_float value)

type config = {
  final_offset : float;
  initial_state_arrow_length : float;
  label_radius : float;
  loop_radius : float;
  state_radius : float;
  transition_color : string;
  transition_width : float;
}

let config =
  {
    final_offset = 2.;
    initial_state_arrow_length = 30.;
    label_radius = 10.;
    loop_radius = 25.;
    state_radius = 10.;
    transition_color = "black";
    transition_width = 2.;
  }

let svg_circle ?(a = []) center radius =
  svg_elt "circle"
    ~a:
      ([
         int_of_float_attr "cx" center.x;
         int_of_float_attr "cy" center.y;
         int_of_float_attr "r" radius;
       ]
      @ a)
    []

let svg_line ?(a = []) source target =
  svg_elt "line"
    ~a:
      ([
         int_of_float_attr "x1" source.x;
         int_of_float_attr "y1" source.y;
         int_of_float_attr "x2" target.x;
         int_of_float_attr "y2" target.y;
       ]
      @ a)
    []

let svg_arrow ?(a = []) ?(slope = 0.4) ?(size = 10.) source target =
  let vec = target --. source in
  let norm = { x = -.vec.y; y = vec.x } in
  let ltipvec = midpoint ~lambda:slope (-1. **. vec) norm in
  let rtipvec = midpoint ~lambda:slope (-1. **. vec) (-1. **. norm) in
  let ltip = target ++. (size **. normalize ltipvec) in
  let rtip = target ++. (size **. normalize rtipvec) in
  svg_elt "g"
    [
      svg_line ~a source target;
      svg_line ~a target ltip;
      svg_line ~a target rtip;
    ]

let svg_text ?(a = []) position label =
  svg_elt "text"
    ~a:
      ([
         int_of_float_attr "x" position.x;
         int_of_float_attr "y" position.y;
         attr "text-anchor" "middle";
         attr "dominant-baseline" "central";
       ]
      @ a)
    [ text label ]

let view_state model state =
  let position = state_position model state in
  let state_color =
    if state = model.fsm.initial then "green"
    else if List.mem state model.fsm.final then "blue"
    else "red"
  in
  let fill_color = if state = model.current_state then "yellow" else "white" in
  let elements =
    [
      svg_circle
        ~a:[ attr "stroke" state_color; attr "fill" fill_color ]
        position config.state_radius;
      svg_text position state.name;
    ]
  in
  let elements =
    if state = model.fsm.initial then
      svg_arrow
        ~a:[ float_attr "stroke-width" 2.; attr "stroke" "black" ]
        (position
        --. {
              x = config.state_radius +. config.initial_state_arrow_length;
              y = 0.1;
            })
        (position --. { x = config.state_radius; y = 0.1 })
      :: elements
    else elements
  in
  let elements =
    if List.mem state model.fsm.final then
      elements
      @ [
          svg_circle
            ~a:[ attr "stroke" state_color; attr "fill" "none" ]
            position
            (config.state_radius -. config.final_offset);
        ]
    else elements
  in

  svg_elt "g"
    ~a:[ onmousedown ~prevent_default:() (fun _ -> DragStart state) ]
    elements

let view_loop source label =
  let label_pos = source --. { x = 0.; y = 2. *. config.loop_radius } in
  svg_elt "g"
    [
      svg_arrow
        ~a:
          [
            float_attr "stroke-width" config.transition_width;
            attr "stroke" config.transition_color;
          ]
        ~slope:0.5
        (source ++. { x = config.state_radius +. 2.; y = -3. })
        (source ++. { x = config.state_radius; y = -2. });
      svg_circle
        ~a:[ attr "stroke" "black"; attr "fill" "none" ]
        (source --. { x = 0.; y = config.loop_radius })
        config.loop_radius;
      svg_circle ~a:[ attr "fill" "white" ] label_pos config.label_radius;
      svg_text label_pos label;
    ]

let view_transition source destination label =
  let lambda = config.state_radius /. distance source destination in
  let sour = midpoint ~lambda source destination in
  let targ = midpoint ~lambda:(1. -. lambda) source destination in
  let label_pos = midpoint ~lambda:0.3 source destination in
  svg_elt "g"
    [
      svg_arrow
        ~a:
          [
            float_attr "stroke-width" config.transition_width;
            attr "stroke" config.transition_color;
          ]
        sour targ;
      svg_circle ~a:[ attr "fill" "white" ] label_pos config.label_radius;
      svg_text label_pos label;
    ]

let view_fsm model =
  let state_els = List.map (view_state model) model.fsm.states in
  let transition_els =
    List.map
      (fun (src, chr, dst) ->
        let label = String.make 1 chr in
        if src = dst then view_loop (state_position model src) label
        else
          view_transition (state_position model src) (state_position model dst)
            label)
      model.fsm.transitions
  in
  transition_els @ state_els

let view model =
  let drag =
    match model.mode with
    | Dragging _ ->
        [
          onmousemove (fun ev -> DragMove { x = ev.x; y = ev.y });
          onmouseup (fun _ -> DragEnd);
        ]
    | _ -> []
  in
  elt "article"
    [
      elt "h1" [ text (model.characters |> List.to_seq |> String.of_seq) ];
      svg_elt "svg"
        ~a:
          (drag
          @ [
              int_of_float_attr "width" model.width;
              int_of_float_attr "height" model.height;
            ])
        (view_fsm model);
      elt "button"
        ~a:[ onclick (fun _ -> Next); disabled (model.characters = []) ]
        [ text "next" ];
    ]
