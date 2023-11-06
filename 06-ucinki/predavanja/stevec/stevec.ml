open Vdom

type model = { stevec : int }
type msg = Povecaj | Pomanjsaj

let update stari_model msg =
  match msg with
  | Povecaj -> { stevec = stari_model.stevec + 1 }
  | Pomanjsaj -> { stevec = stari_model.stevec - 1 }

let view model =
  div
    [
      elt "h1" [ text (string_of_int model.stevec) ];
      elt "button" ~a:[ onclick (fun _ -> Povecaj) ] [ text "+" ];
      elt "button"
        ~a:[ onclick (fun _ -> Pomanjsaj); disabled (model.stevec <= 0) ]
        [ text "-" ];
    ]

let init = { stevec = 0 }
let app = simple_app ~init ~view ~update ()

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
