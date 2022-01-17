let konjugiraj ((x, y) : float * float) : float * float = (x, -. y)

type kompleksno = float * float

let konjugiraj' ((x, y) : kompleksno) : kompleksno = (x, -. y)

type slovarji_kjer_so_kljuci_stringi_in_vrednosti_inti = (string * int) list
type slovarji_kjer_so_kljuci_inti_in_vrednosti_inti = (int * int) list
type 'k slovar_kjer_so_vrednosti_inti = ('k * int) list
type ('k, 'v) slovar = ('k * 'v) list

type 'kol kompleksno = 'kol * 'kol

type predmet = string
type ocena =
  | Ocena6
  | Ocena7
  | Ocena8
  | Ocena9
  | Ocena10
type vpisna = int

type student = {
  ime : string;
  priimek : string;
  vpisna : vpisna;
  ocene : (predmet, ocena) slovar;
}

let matija_vcasih = {
  ime = "Matija";
  priimek = "Pretnar";
  vpisna = 27004498;
  ocene = []
}

let vsota sez = List.fold_right (fun el acc -> el + acc) sez 0
let vsota sez = List.fold_left (fun acc el -> el + acc) 0 sez
let vsota = List.fold_left (+) 0

(* f1 (f2 (f3 (f4 x))) *)
let (|>) x f = f x
(* x |> f4 |> f3 |> f2 |> f1 *)

let vrednost_ocene ocena =
  match ocena with
  | Ocena6 -> 6
  | Ocena7 -> 7
  | Ocena8 -> 8
  | Ocena9 -> 9
  | Ocena10 -> 10

let povprecje student =
    let ocene = student.ocene in
    let stevilo_ocen = List.length ocene in
    let vsota_ocen =
      ocene
      |> List.map snd
      |> List.map vrednost_ocene
      |> vsota
      |> float_of_int
    in
    vsota_ocen /. (float_of_int stevilo_ocen)

let opravi_izpit predmet ocena student =
  {student with ocene = (predmet, ocena) :: student.ocene}

let matija_danes =
  matija_vcasih
  |> opravi_izpit "Uvod v programiranje" Ocena10
  |> opravi_izpit "Analiza 1" Ocena9
  |> opravi_izpit "Algebra 1" Ocena9

type datum = int

type test =
  | HitriTest
  | PCR

type cepljenje = {
  dan : datum;
  doza : int
}

type pct_pogoj =
  | PCR of datum
  | Prebolel
  | Cepljen of cepljenje



let vcerajsnji_test = PCR 20211116

let jutrisnje_cepivo = Cepljen { dan = 20211118; doza = 3}

let veljaven_pct dan_preverjanja =
  function
  | PCR dan_testa -> dan_testa + 2 > dan_preverjanja
  | Prebolel -> true
  | Cepljen cepljenje ->
      cepljenje.doza >= 2 && cepljenje.dan + 14 < dan_preverjanja

let ali_dobi_stipendijo student = povprecje student > 9.5

type 'a mogoce =
| Nimam
| Imam of 'a

let varnejse_povprecje student =
  match student.ocene with
  | [] -> Nimam
  | ocene ->
      let stevilo_ocen = List.length ocene in
      let vsota_ocen =
        ocene
        |> List.map snd
        |> List.map vrednost_ocene
        |> vsota
        |> float_of_int
      in
      Imam (vsota_ocen /. (float_of_int stevilo_ocen))

let ali_dobi_stipendijo student =
  match varnejse_povprecje student with
  | Imam povprecje -> Imam (povprecje > 9.5)
  | Nimam -> Nimam

type 'a option = None | Some of 'a
