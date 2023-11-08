type stanje = {
  oznaka : string
}

type avtomat = {
  stanja : stanje list;
  zacetno_stanje : stanje;
  sprejemna_stanja : stanje list;
  prehodi : (stanje * char * stanje) list
}

let preberi_znak avt q chr =
  let (_, _, q') = List.find (fun (q1, chr', q2) -> q1 = q && chr = chr') avt.prehodi in
  q'


let s_fold_left f acc s =
  List.fold_left f acc (List.of_seq (String.to_seq s))

let preberi_niz avt q str =
  s_fold_left (preberi_znak avt) q str  

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
  stanje_vmesnika : stanje_vmesnika
}

type msg = PreberiNiz of string | ZamenjajVmesnik of stanje_vmesnika

let update model =
  function
  | PreberiNiz str -> { model with
                        stanje_avtomata = preberi_niz model.avtomat model.stanje_avtomata str;
                        stanje_vmesnika = RezultatPrebranegaNiza}
  | ZamenjajVmesnik stanje_vmesnika -> { model with stanje_vmesnika }

let rec izpisi_moznosti () =
  print_endline "1) izpiši avtomat";
  print_endline "2) preberi niz";
  print_string "> ";
  match read_line () with
  | "1" -> ZamenjajVmesnik IzpisAvtomata
  | "2" -> ZamenjajVmesnik BranjeNiza
  | _ -> print_endline "** VNESI 1 ALI 2 **"; izpisi_moznosti ()

let izpisi_avtomat avtomat =
  print_endline "STANJA:";
  let izpisi_stanje stanje =
    let prikaz = stanje.oznaka in
    let prikaz = if stanje = avtomat.zacetno_stanje then "-> " ^ prikaz else prikaz in
    let prikaz = if List.mem stanje avtomat.sprejemna_stanja then "(" ^ prikaz ^ ")" else prikaz in
    print_endline prikaz
  in
  List.iter izpisi_stanje avtomat.stanja;
    
  print_endline "PREHODI:";
  let izpisi_prehodi prehod = 
    let prikaz = 
      match prehod with
      |(iz, skozi, v) -> iz.oznaka ^ "--" ^ Char.escaped skozi ^ "->" ^ v.oznaka in
    print_endline prikaz
  in
  List.iter izpisi_prehodi avtomat.prehodi

let beri_niz model =
  print_string "Vnesi niz > ";
  let str = read_line () in
  PreberiNiz str

let izpisi_rezultat model =
  if List.mem model.stanje_avtomata model.avtomat.sprejemna_stanja then
    print_endline "ok"
  else
    print_endline "nope"

let view (model: model) : msg =
  match model.stanje_vmesnika with
  | IzpisAvtomata ->
      izpisi_avtomat model.avtomat;
      ZamenjajVmesnik SeznamMoznosti
  | SeznamMoznosti ->
      izpisi_moznosti ()
  | BranjeNiza ->
      beri_niz model
  | RezultatPrebranegaNiza ->
      izpisi_rezultat model;
      ZamenjajVmesnik SeznamMoznosti

let init avtomat = {
  avtomat = avtomat;
  stanje_avtomata = avtomat.zacetno_stanje;
  stanje_vmesnika = SeznamMoznosti
}

let rec main_loop model =
  let msg = view model in
  let model' = update model msg in
  main_loop model'

let vsebuje_samo_nicle =
  let ima_enke = { oznaka = "ima 1" }
  and nima_enk = { oznaka = "nima 1" }
  in
  {
    stanja = [ima_enke; nima_enk];
    zacetno_stanje = nima_enk;
    sprejemna_stanja = [nima_enk];
    prehodi = [
      (nima_enk, '0', nima_enk);
      (nima_enk, '1', ima_enke);
      (ima_enke, '0', ima_enke);
      (ima_enke, '1', ima_enke);
    ]
  }

let _ = main_loop (init vsebuje_samo_nicle)