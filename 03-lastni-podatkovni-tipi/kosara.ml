(* slab primer *)

type narocilo = (string * int) list * string * string * string

let narocila : narocilo list =
  [
    ([ ("Death Star 75419", 1) ], "Sheev Palpatine", "po pošti", "Alderaan");
    ( [ ("AT-ST Walker 75417", 12); ("TIE Interceptor 75382", 24) ],
      "Anakin Skywalker",
      "po pošti",
      "Hoth" );
    ( [ ("Lightsaber Gel Pens - 10 Pack 5008815", 10) ],
      "General Grievous",
      "osebno",
      "???" );
  ]

let stroski_posiljanja ((izdelki, _, dostava, _) : narocilo) =
  match dostava with
  | "po pošti" ->
      2.5
      *. (izdelki |> List.map snd |> List.fold_left ( + ) 0 |> float_of_int
        |> sqrt)
  | _ -> 0.

let komu_je_treba_poslati =
  narocila
  |> List.filter (fun (_, _, dostava, _) -> dostava = "po pošti")
  |> List.map (fun ((_, ime, _, _) as narocilo) ->
         (ime, stroski_posiljanja narocilo))

(* Z zapisi *)

type izdelek_v_kosari = { izdelek : string; kolicina : int }
type oseba = { ime : string }
type naslov = { planet : string }

type narocilo = {
  kosara : izdelek_v_kosari list;
  narocnik : oseba;
  dostava : string;
  naslov : naslov;
}

let narocila : narocilo list =
  [
    {
      kosara = [ { izdelek = "Death Star 75419"; kolicina = 1 } ];
      narocnik = { ime = "Sheev Palpatine" };
      dostava = "po pošti";
      naslov = { planet = "Alderaan" };
    };
    {
      kosara =
        [
          { izdelek = "AT-ST Walker 75417"; kolicina = 12 };
          { izdelek = "TIE Interceptor 75382"; kolicina = 24 };
        ];
      narocnik = { ime = "Anakin Skywalker" };
      dostava = "pošta";
      naslov = { planet = "Hoth" };
    };
    {
      kosara =
        [ { izdelek = "Lightsaber Gel Pens - 10 Pack 5008815"; kolicina = 10 } ];
      narocnik = { ime = "General Grievous" };
      dostava = "osebno";
      naslov = { planet = "???" };
    };
  ]

let stroski_posiljanja narocilo =
  match narocilo.dostava with
  | "po pošti" ->
      2.5
      *. (narocilo.kosara
         |> List.map (fun i -> i.kolicina)
         |> List.fold_left ( + ) 0 |> float_of_int |> sqrt)
  | _ -> 0.

let komu_je_treba_poslati =
  narocila
  |> List.filter (fun narocilo -> narocilo.dostava = "po pošti")
  |> List.map (fun narocilo -> (narocilo.narocnik, stroski_posiljanja narocilo))

(* Dodajmo še naštevne tipe *)

type izdelek_v_kosari = { izdelek : string; kolicina : int }
type oseba = { ime : string }
type naslov = { planet : string }
type dostava = OsebniPrevzem | PoPosti

type narocilo = {
  kosara : izdelek_v_kosari list;
  narocnik : oseba;
  dostava : dostava;
  naslov : naslov;
}

let narocila : narocilo list =
  [
    {
      kosara = [ { izdelek = "Death Star 75419"; kolicina = 1 } ];
      narocnik = { ime = "Sheev Palpatine" };
      dostava = PoPosti;
      naslov = { planet = "Alderaan" };
    };
    {
      kosara =
        [
          { izdelek = "AT-ST Walker 75417"; kolicina = 12 };
          { izdelek = "TIE Interceptor 75382"; kolicina = 24 };
        ];
      narocnik = { ime = "Anakin Skywalker" };
      dostava = PoPosti;
      naslov = { planet = "Hoth" };
    };
    {
      kosara =
        [ { izdelek = "Lightsaber Gel Pens - 10 Pack 5008815"; kolicina = 10 } ];
      narocnik = { ime = "General Grievous" };
      dostava = OsebniPrevzem;
      naslov = { planet = "???" };
    };
  ]

let stroski_posiljanja narocilo =
  match narocilo.dostava with
  | PoPosti ->
      2.5
      *. (narocilo.kosara
         |> List.map (fun i -> i.kolicina)
         |> List.fold_left ( + ) 0 |> float_of_int |> sqrt)
  | OsebniPrevzem -> 0.

let komu_je_treba_poslati =
  narocila
  |> List.filter (fun narocilo -> narocilo.dostava = PoPosti)
  |> List.map (fun narocilo -> (narocilo.narocnik, stroski_posiljanja narocilo))

(* Kaj, če naslova ni? *)

type izdelek_v_kosari = { izdelek : string; kolicina : int }
type oseba = { ime : string }
type naslov = { planet : string }
type trgovina = BTC | Rudnik
type dostava = OsebniPrevzem | PoPosti of naslov | PoKurirju of naslov

type narocilo = {
  kosara : izdelek_v_kosari list;
  narocnik : oseba;
  dostava : dostava;
}

let narocila : narocilo list =
  [
    {
      kosara = [ { izdelek = "Death Star 75419"; kolicina = 1 } ];
      narocnik = { ime = "Sheev Palpatine" };
      dostava = PoPosti { planet = "Alderaan" };
    };
    {
      kosara =
        [
          { izdelek = "AT-ST Walker 75417"; kolicina = 12 };
          { izdelek = "TIE Interceptor 75382"; kolicina = 24 };
        ];
      narocnik = { ime = "Anakin Skywalker" };
      dostava = PoPosti { planet = "Hoth" };
    };
    {
      kosara =
        [ { izdelek = "Lightsaber Gel Pens - 10 Pack 5008815"; kolicina = 10 } ];
      narocnik = { ime = "General Grievous" };
      dostava = OsebniPrevzem;
    };
  ]

let stevilo_izdelkov narocilo =
  narocilo.kosara |> List.map (fun i -> i.kolicina) |> List.fold_left ( + ) 0

let stroski_posiljanja narocilo =
  match narocilo.dostava with
  | PoPosti _ -> 2.5 *. (stevilo_izdelkov narocilo |> float_of_int |> sqrt)
  | PoKurirju _ -> 10. *. (stevilo_izdelkov narocilo |> float_of_int)
  | OsebniPrevzem -> 0.

let komu_je_treba_poslati =
  narocila
  |> List.filter (fun narocilo ->
         match narocilo.dostava with PoPosti _ -> true | _ -> false)
  |> List.map (fun narocilo -> (narocilo.narocnik, stroski_posiljanja narocilo))

let komu_je_treba_poslati =
  narocila
  |> List.filter (function { dostava = PoPosti _; _ } -> true | _ -> false)
  |> List.map (fun narocilo -> (narocilo.narocnik, stroski_posiljanja narocilo))
