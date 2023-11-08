open Model
open Vdom

let ravno_prav_nicel =
  let q0 = { name = "ostanek 0" }
  and q1 = { name = "ostanek 1" }
  and q2 = { name = "ostanek 2" } in
  let transitions =
    [
      (q0, '0', q1);
      (q1, '0', q2);
      (q2, '0', q0);
      (q0, '1', q0);
      (q1, '1', q1);
      (q2, '1', q2);
    ]
  in
  { states = [ q0; q1; q2 ]; initial = q0; final = [ q1 ]; transitions }

(* let vsebuje_samo_nicle =
   let ima_enke = { name = "ima 1" } and nima_enk = { name = "nima 1" } in
   {
     states = [ ima_enke; nima_enk ];
     initial = nima_enk;
     final = [ nima_enk ];
     transitions =
       [
         (nima_enk, '0', nima_enk);
         (nima_enk, '1', ima_enke);
         (ima_enke, '0', ima_enke);
         (ima_enke, '1', ima_enke);
       ];
   } *)

let app =
  simple_app
    ~init:(init 500. 500. ravno_prav_nicel "001010101011")
    ~view:View.view ~update ()

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
