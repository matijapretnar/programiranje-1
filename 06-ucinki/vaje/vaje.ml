(*
   Če uporabljate spletno verzijo OCamla, potem uporabite <http://ocaml.besson.link/>,
   saj na njej pravilno deluje branje iz standardnega vhoda.
*)

type stanje = { oznaka : string }

type avtomat = {
  stanja : stanje list;
  zacetno_stanje : stanje;
  sprejemna_stanja : stanje list;
  prehodi : (stanje * char * stanje) list;
}

let preberi_znak avt q chr =
  let _, _, q' =
    List.find (fun (q1, chr', q2) -> q1 = q && chr = chr') avt.prehodi
  in
  q'

(* String.fold_left je podprt samo od 4.13 naprej *)

let s_fold_left f acc s = s |> String.to_seq |> Seq.fold_left f acc
let preberi_niz avt q str = s_fold_left (preberi_znak avt) q str

let ali_sprejema_niz avt str =
  let koncno_stanje = preberi_niz avt avt.zacetno_stanje str in
  List.mem koncno_stanje avt.sprejemna_stanja

type stanje_vmesnika =
  | SeznamMoznosti
  | IzpisAvtomata
  | BranjeNiza
  | RezultatPrebranegaNiza

type model = {
  avtomat : avtomat;
  stanje_avtomata : stanje;
  stanje_vmesnika : stanje_vmesnika;
}

type msg = PreberiNiz of string | ZamenjajVmesnik of stanje_vmesnika

let update model = function
  | PreberiNiz str ->
      {
        model with
        stanje_avtomata = preberi_niz model.avtomat model.stanje_avtomata str;
        stanje_vmesnika = RezultatPrebranegaNiza;
      }
  | ZamenjajVmesnik stanje_vmesnika -> { model with stanje_vmesnika }

let rec izpisi_moznosti () =
  print_endline "1) izpiši avtomat";
  print_endline "2) preberi niz";
  print_string "> ";
  match read_line () with
  | "1" -> ZamenjajVmesnik IzpisAvtomata
  | "2" -> ZamenjajVmesnik BranjeNiza
  | _ ->
      print_endline "** VNESI 1 ALI 2 **";
      izpisi_moznosti ()

let izpisi_avtomat avtomat =
  print_endline "STANJA:";
  print_endline "->(A)";
  (* začetno stanje *)
  print_endline " ((B))";
  (* stanje, ki je sprejemno *)
  print_endline "  (C)";
  (* še eno stanje, ki ni sprejemno *)
  print_endline " ((D))";
  (* in še eno stanje, ki je sprejemno *)
  print_endline "PREHODI:";
  print_endline "  (A)--[0]->(B)";
  print_endline "  (A)--[1]->(C)";
  print_endline "  (A)--[0]->(D)"

let beri_niz model =
  print_string "Vnesi niz > ";
  let str = read_line () in
  PreberiNiz str

let izpisi_rezultat model =
  print_endline "Ne vem, ali je bil niz sprejet ali ne"

let view (model : model) : msg = failwith "TODO"
let init avtomat = failwith "TODO"

let rec main_loop model =
  let msg = view model in
  let model' = update model msg in
  main_loop model'

let vsebuje_samo_nicle =
  let ima_enke = { oznaka = "ima 1" } and nima_enk = { oznaka = "nima 1" } in
  {
    stanja = [ ima_enke; nima_enk ];
    zacetno_stanje = nima_enk;
    sprejemna_stanja = [ nima_enk ];
    prehodi =
      [
        (nima_enk, '0', nima_enk);
        (nima_enk, '1', ima_enke);
        (ima_enke, '0', ima_enke);
        (ima_enke, '1', ima_enke);
      ];
  }

let _ = main_loop (init vsebuje_samo_nicle)
