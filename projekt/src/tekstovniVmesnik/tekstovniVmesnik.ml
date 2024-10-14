open Definicije
open Avtomat

type stanje_vmesnika =
  | SeznamMoznosti
  | IzpisAvtomata
  | BranjeNiza
  | RezultatPrebranegaNiza
  | OpozoriloONapacnemNizu

type model = {
  avtomat : t;
  stanje_avtomata : Stanje.t;
  stanje_vmesnika : stanje_vmesnika;
}

type msg =
  | PreberiNiz of string
  | ZamenjajVmesnik of stanje_vmesnika
  | VrniVPrvotnoStanje

let preberi_niz avtomat q niz =
  let aux acc znak =
    match acc with
    | None -> None
    | Some q -> Avtomat.prehodna_funkcija avtomat q znak
  in
  niz |> String.to_seq |> Seq.fold_left aux (Some q)

let update model = function
  | PreberiNiz str -> (
      match preberi_niz model.avtomat model.stanje_avtomata str with
      | None -> { model with stanje_vmesnika = OpozoriloONapacnemNizu }
      | Some stanje_avtomata ->
          {
            model with
            stanje_avtomata;
            stanje_vmesnika = RezultatPrebranegaNiza;
          })
  | ZamenjajVmesnik stanje_vmesnika -> { model with stanje_vmesnika }
  | VrniVPrvotnoStanje ->
      {
        model with
        stanje_avtomata = zacetno_stanje model.avtomat;
        stanje_vmesnika = SeznamMoznosti;
      }

let rec izpisi_moznosti () =
  print_endline "1) izpiši avtomat";
  print_endline "2) beri znake";
  print_endline "3) nastavi na začetno stanje";
  print_string "> ";
  match read_line () with
  | "1" -> ZamenjajVmesnik IzpisAvtomata
  | "2" -> ZamenjajVmesnik BranjeNiza
  | "3" -> VrniVPrvotnoStanje
  | _ ->
      print_endline "** VNESI 1, 2 ALI 3 **";
      izpisi_moznosti ()

let izpisi_avtomat avtomat =
  let izpisi_stanje stanje =
    let prikaz = Stanje.v_niz stanje in
    let prikaz =
      if stanje = zacetno_stanje avtomat then "-> " ^ prikaz else prikaz
    in
    let prikaz =
      if je_sprejemno_stanje avtomat stanje then prikaz ^ " +" else prikaz
    in
    print_endline prikaz
  in
  List.iter izpisi_stanje (seznam_stanj avtomat)

let beri_niz _model =
  print_string "Vnesi niz > ";
  let str = read_line () in
  PreberiNiz str

let izpisi_rezultat model =
  if je_sprejemno_stanje model.avtomat model.stanje_avtomata then
    print_endline "Niz je bil sprejet"
  else print_endline "Niz ni bil sprejet"

let view model =
  match model.stanje_vmesnika with
  | SeznamMoznosti -> izpisi_moznosti ()
  | IzpisAvtomata ->
      izpisi_avtomat model.avtomat;
      ZamenjajVmesnik SeznamMoznosti
  | BranjeNiza -> beri_niz model
  | RezultatPrebranegaNiza ->
      izpisi_rezultat model;
      ZamenjajVmesnik SeznamMoznosti
  | OpozoriloONapacnemNizu ->
      print_endline "Niz ni veljaven";
      ZamenjajVmesnik SeznamMoznosti

let init avtomat =
  {
    avtomat;
    stanje_avtomata = zacetno_stanje avtomat;
    stanje_vmesnika = SeznamMoznosti;
  }

let rec loop model =
  let msg = view model in
  let model' = update model msg in
  loop model'

let _ = loop (init enke_1mod3)
