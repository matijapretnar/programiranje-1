module type STANJE = sig
  type t

  val iz_niza : string -> t
  val v_niz : t -> string
end

module Stanje : STANJE = struct
  type t = { oznaka : string }

  let iz_niza oznaka = { oznaka }
  let v_niz { oznaka } = oznaka
end

module type AVTOMAT = sig
  type t

  val prazen_avtomat : Stanje.t -> t
  val dodaj_nesprejemno_stanje : Stanje.t -> t -> t
  val dodaj_sprejemno_stanje : Stanje.t -> t -> t
  val dodaj_prehod : Stanje.t -> char -> Stanje.t -> t -> t
  val prehodna_funkcija : t -> Stanje.t -> char -> Stanje.t option
  val zacetno_stanje : t -> Stanje.t
  val seznam_stanj : t -> Stanje.t list
  val seznam_prehodov : t -> (Stanje.t * char * Stanje.t) list
  val je_sprejemno_stanje : t -> Stanje.t -> bool
end

module Avtomat : AVTOMAT = struct
  type stanje = Stanje.t

  type t = {
    stanja : stanje list;
    zacetno_stanje : stanje;
    sprejemna_stanja : stanje list;
    prehodi : (stanje * char * stanje) list;
  }

  let prazen_avtomat zacetno_stanje =
    {
      stanja = [ zacetno_stanje ];
      zacetno_stanje;
      sprejemna_stanja = [];
      prehodi = [];
    }

  let dodaj_nesprejemno_stanje stanje avtomat =
    { avtomat with stanja = stanje :: avtomat.stanja }

  let dodaj_sprejemno_stanje stanje avtomat =
    {
      avtomat with
      stanja = stanje :: avtomat.stanja;
      sprejemna_stanja = stanje :: avtomat.sprejemna_stanja;
    }

  let dodaj_prehod stanje1 znak stanje2 avtomat =
    { avtomat with prehodi = (stanje1, znak, stanje2) :: avtomat.prehodi }

  let prehodna_funkcija avtomat stanje znak =
    match
      List.find_opt
        (fun (stanje1, znak', _stanje2) -> stanje1 = stanje && znak = znak')
        avtomat.prehodi
    with
    | None -> None
    | Some (_, _, stanje2) -> Some stanje2

  let zacetno_stanje avtomat = avtomat.zacetno_stanje
  let seznam_stanj avtomat = avtomat.stanja
  let seznam_prehodov avtomat = avtomat.prehodi

  let je_sprejemno_stanje avtomat stanje =
    List.mem stanje avtomat.sprejemna_stanja
end

let enke_1mod3 =
  let open Avtomat in
  let q0 = Stanje.iz_niza "q0"
  and q1 = Stanje.iz_niza "q1"
  and q2 = Stanje.iz_niza "q2" in
  prazen_avtomat q0 |> dodaj_sprejemno_stanje q1
  |> dodaj_nesprejemno_stanje q2
  |> dodaj_prehod q0 '0' q0 |> dodaj_prehod q1 '0' q1 |> dodaj_prehod q2 '0' q2
  |> dodaj_prehod q0 '1' q1 |> dodaj_prehod q1 '1' q2 |> dodaj_prehod q2 '1' q0

module type TRAK = sig
  type t

  val prazen : t
  val trenutni_znak : t -> char
  val je_na_koncu : t -> bool
  val premakni_naprej : t -> t
  val iz_niza : string -> t
  val v_niz : t -> string
  val prebrani : t -> string
  val neprebrani : t -> string
end

module Trak : TRAK = struct
  type t = { niz : string; indeks_trenutnega_znaka : int }

  let trenutni_znak trak = String.get trak.niz trak.indeks_trenutnega_znaka
  let je_na_koncu trak = String.length trak.niz = trak.indeks_trenutnega_znaka

  let premakni_naprej trak =
    { trak with indeks_trenutnega_znaka = succ trak.indeks_trenutnega_znaka }

  let iz_niza niz = { niz; indeks_trenutnega_znaka = 0 }
  let prazen = iz_niza ""
  let v_niz trak = trak.niz

  let prebrani trak = String.sub trak.niz 0 trak.indeks_trenutnega_znaka

  and neprebrani trak =
    String.sub trak.niz trak.indeks_trenutnega_znaka
      (String.length trak.niz - trak.indeks_trenutnega_znaka)
end

module type ZAGNANIAVTOMAT = sig
  type t

  val pozeni : Avtomat.t -> Trak.t -> t
  val avtomat : t -> Avtomat.t
  val trak : t -> Trak.t
  val stanje : t -> Stanje.t
  val korak_naprej : t -> t option
  val je_v_sprejemnem_stanju : t -> bool
end

module ZagnaniAvtomat : ZAGNANIAVTOMAT = struct
  type t = { avtomat : Avtomat.t; trak : Trak.t; stanje : Stanje.t }

  let pozeni avtomat trak =
    { avtomat; trak; stanje = Avtomat.zacetno_stanje avtomat }

  let avtomat { avtomat; _ } = avtomat
  let trak { trak; _ } = trak
  let stanje { stanje; _ } = stanje

  let korak_naprej { avtomat; trak; stanje } =
    if Trak.je_na_koncu trak then None
    else
      let stanje' =
        Avtomat.prehodna_funkcija avtomat stanje (Trak.trenutni_znak trak)
      in
      match stanje' with
      | None -> None
      | Some stanje' ->
          Some { avtomat; trak = Trak.premakni_naprej trak; stanje = stanje' }

  let je_v_sprejemnem_stanju { avtomat; stanje; _ } =
    Avtomat.je_sprejemno_stanje avtomat stanje
end

let preberi_niz avtomat q niz =
  let aux acc znak =
    match acc with
    | None -> None
    | Some q -> Avtomat.prehodna_funkcija avtomat q znak
  in
  niz |> String.to_seq |> Seq.fold_left aux (Some q)

let ali_sprejema_niz avtomat niz =
  match preberi_niz avtomat (Avtomat.zacetno_stanje avtomat) niz with
  | None -> false
  | Some koncno_stanje -> Avtomat.je_sprejemno_stanje avtomat koncno_stanje
