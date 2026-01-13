(*----------------------------------------------------------------------------*
  # 4. domača naloga
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
  ## Slovarji
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
  Na predavanjih in vajah smo si ogledali iskalna drevesa in implementacijo 
  AVL-dreves za predstavitev množic. V tej nalogi morate s pomočjo AVL-dreves 
  implementirati `slovar`, ki preslika ključe tipa `'k` v vrednosti tipa `'v`.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
  ### Stroga ureditev

  Za predstavitev slovarja potrebujemo strogo ureditev na tipu ključev.
  Najprej definiramo tip `ureditev`, ki predstavlja možne izide
  primerjave dveh elementov, nato pa še modul `UREJEN_TIP`, s katerim
  lahko primerjamo abstraktne elemente.


[*----------------------------------------------------------------------------*)

type ureditev = Less | Equal | Greater

module type UREJEN_TIP = sig
  type t

  val primerjaj : t -> t -> ureditev
end

module INT_UREJEN_TIP : UREJEN_TIP with type t = int = struct
  type t = int

  let primerjaj x y = if x < y then Less else if x > y then Greater else Equal
end

(*----------------------------------------------------------------------------*
  Sestavite modul `STRING_UREJEN_TIP`, ki implementira `UREJEN_TIP` za tip
  `string`.
[*----------------------------------------------------------------------------*)

module STRING_UREJEN_TIP : UREJEN_TIP with type t = string = struct
  type t = string

  let primerjaj x y = assert false
end;;

STRING_UREJEN_TIP.primerjaj "abc" "abd"
(* - : ureditev = Less *)

(*----------------------------------------------------------------------------*
  Za poljuben tip lahko definiramo `razširitev` z najmanjšim in največjim
  elementom. Sestavite parametriziran modul `RAZSIRJEN_UREJEN_TIP`, ki
  sprejme modul, ki implementira signaturo `UREJEN_TIP`, in vrne modul, ki
  implementira signaturo `UREJEN_TIP` za razširjeni tip.
[*----------------------------------------------------------------------------*)

type 'a razsiritev = MinInf | PlusInf | Value of 'a

module RAZSIRJEN_UREJEN_TIP (U : UREJEN_TIP) :
  UREJEN_TIP with type t = U.t razsiritev = struct
  type t = U.t razsiritev

  let primerjaj x y = assert false
end

module LIFTED_INT_UREJEN_TIP = RAZSIRJEN_UREJEN_TIP (INT_UREJEN_TIP);;

LIFTED_INT_UREJEN_TIP.primerjaj MinInf (Value 3)
(* - : ureditev = Less *)

(*----------------------------------------------------------------------------*
  ### AVLSlovar

  Sestavite parametriziran modul `MAKE_SLOVAR`, ki sprejme modul, ki
  implementira `UREJEN_TIP`, in vrne modul s signaturo `SLOVAR`. Vaš slovar
  naj bo implementiran z AVL-drevesi, tako da je vstavljanje in iskanje v
  slovarju v času `O(log n)`.
[*----------------------------------------------------------------------------*)

module type SLOVAR = sig
  type kljuc
  type 'a t

  val prazen : 'a t
  (** Vrne prazen slovar. *)
  val dodaj : kljuc -> 'a -> 'a t -> 'a t
  (** Doda nov par `kljuc`, `vrednost` v slovar. Če ključ v slovarju že obstaja, 
      se njegova vrednost posodobi. *)
  val popravi : kljuc -> ('a option -> 'a option) -> 'a t -> 'a t
  (** Popravi vrednost pod ključem `kljuc` s funkcijo `f`. Če ključ v slovarju
      ne obstaja, se pokliče `f None`, sicer `f (Some vrednost)`. Če je rezultat
      klica `f` enak `None`, se par odstrani iz slovarja, če je rezultat klica 
      `Some v`, se pod ključ `kljuc` zapiše vrednost `v`.*)
  val odstrani : kljuc -> 'a t -> 'a t
  (** Odstrani par s ključem `kljuc` iz slovarja. Če ključa v slovarju ni, naj 
      funkcija vrne prvotni slovar in ne sproži napake. *)
  val velikost : 'a t -> int
  (** Vrne število elementov v slovarju. *)
  val kljuci : 'a t -> kljuc list
  (** Našteje ključe v slovarju v enakem vrstnem redu kot to določa urejenost. *)
  val vrednosti : 'a t -> 'a list
  (** Našteje vrednosti v slovarju v enakem vrstnem redu kot to določa urejenost
      pripadajočih ključev. *)
  val najmanjsi_opt : 'a t -> (kljuc * 'a) option
  (** Vrne najmanjši ključ v slovarju ali `None`, če je slovar prazen. *)
  val najvecji_opt : 'a t -> (kljuc * 'a) option
  (** Vrne največji ključ v slovarju ali `None`, če je slovar prazen. *)
  val poisci_opt : kljuc -> 'a t -> 'a option
  (** Poišče vrednost pod ključem `kljuc`. Če ključ v slovarju ne obstaja,
      vrne `None`. *)
  val iter : (kljuc -> 'a -> unit) -> 'a t -> unit
  (** Izvede funkcijo za vsak par ključ, vrednost v slovarju v enakem vrstnem 
      redu kot ga določa urejenost. *)
  val zlozi : (kljuc -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** Zloži slovar z dano funkcijo in začetno vrednostjo. Elementi se obdelajo
      v enakem vrstnem redu kot ga določa urejenost.
  
      Specifično za
      `zlozi f slovar acc = f k_n v_n (... (f k_2 v_2 (f k_1 v_1 acc))...)`
      , kjer so `(k_1, v_1), ..., (k_n, v_n)` pari ključ, vrednost v slovarju 
      urejeni po ključih.
  *)
  val preslikaj : ('a -> 'b) -> 'a t -> 'b t
  (** Preslika vrednosti v slovarju z dano funkcijo. *)
  val preslikaji : (kljuc -> 'a -> 'b) -> 'a t -> 'b t
  (** Preslika vrednosti v slovarju z dano funkcijo, ki poleg vrednosti dobi še
      pripadajoči ključ. *)
  val vsebuje : kljuc -> 'a t -> bool
  (** Preveri, ali slovar vsebuje podan ključ. *)
  val za_vse : (kljuc -> 'a -> bool) -> 'a t -> bool
  (** Preveri, ali za vse pare ključ, vrednost v slovarju velja podan pogoj. *)
  val obstaja : (kljuc -> 'a -> bool) -> 'a t -> bool
  (** Preveri, ali obstaja vsaj en par ključ, vrednost v slovarju, ki izpolnjuje
      podan pogoj. *)
  val v_seznam : 'a t -> (kljuc * 'a) list
  (** Pretvori slovar v seznam parov ključ, vrednost v enakem vrstnem redu kot
      to določa urejenost. *)
  val iz_seznama : (kljuc * 'a) list -> 'a t
  (** Ustvari slovar iz seznama parov ključ, vrednost. Če se ključi v seznamu
      ponavljajo, naj enak ključ obdrži zadnjo vrednost. *)
end


module MAKE_SLOVAR (U : UREJEN_TIP) : SLOVAR with type kljuc = U.t = struct
  type kljuc = U.t
  type 'a t = unit

  let prazen : 'a t = ()
  let dodaj _ _ _ = assert false
  let popravi _ _ _ = assert false
  let odstrani _ _ = assert false
  let velikost _ = assert false
  let kljuci _ = assert false
  let vrednosti _ = assert false
  let najmanjsi_opt _ = assert false
  let najvecji_opt _ = assert false
  let poisci_opt _ = assert false
  let iter _ _ = assert false
  let zlozi _ _ _ = assert false
  let preslikaj _ _ = assert false
  let preslikaji _ _ = assert false
  let vsebuje _ _ = assert false
  let za_vse _ _= assert false
  let obstaja _ _= assert false
  let v_seznam _ = assert false
  let iz_seznama _ = assert false
end

module SLOVAR_NIZ = MAKE_SLOVAR (STRING_UREJEN_TIP)

let slovar =
  SLOVAR_NIZ.iz_seznama
    [ ("jabolko", "apple"); ("banana", "banana"); ("cesnja", " cherry") ]
  |> SLOVAR_NIZ.dodaj "datelj" "date"
  |> SLOVAR_NIZ.odstrani "banana"
  |> SLOVAR_NIZ.popravi "cesnja" (function
       | None -> Some "cherry"
       | Some v -> Some ("sour " ^ v))
  |> SLOVAR_NIZ.preslikaj String.length
  |> SLOVAR_NIZ.v_seznam

(*----------------------------------------------------------------------------*
  ## Turingovi stroji
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
  Na predavanjih in vajah smo si ogledali Turingove stroje. Pred vami je
  neučinkovito implementiran Turingov stroj. Vaša naloga je, da implementacijo
  s pomočjo slovarjev izboljšate tako, da bo deloval učinkoviteje.
[*----------------------------------------------------------------------------*)

type direction = Left | Right
type state = string

module type TAPE = sig
  type t

  val make : string -> t
  val print : t -> unit
  val read : t -> char
  val move : direction -> t -> t
  val write : char -> t -> t
end

module Tape : TAPE = struct
  type t = { left : char list; right : char list }

  let make str = { left = []; right = str |> String.to_seq |> List.of_seq }

  let print { left; right } =
    List.rev_append left right |> List.to_seq |> String.of_seq |> print_endline;
    print_endline (String.make (List.length left) ' ' ^ "^")

  let read { right } = match right with [] -> ' ' | chr :: _ -> chr

  let move dir { left; right } =
    match (dir, left, right) with
    | Left, ' ' :: left, [] -> { left; right }
    | Left, chr :: left, right -> { left; right = chr :: right }
    | Left, [], right -> { left = []; right = ' ' :: right }
    | Right, [], ' ' :: right -> { left; right }
    | Right, left, chr :: right -> { left = chr :: left; right }
    | Right, left, [] -> { left = ' ' :: left; right = [] }

  let write chr { left; right } =
    match right with
    | [] when chr = ' ' -> { left; right }
    | [] -> { left; right = [ chr ] }
    | _ :: right -> { left; right = chr :: right }
end

let primer_trak =
  Tape.(
    make "ABCDE" |> move Left |> move Left |> move Right |> move Right
    |> move Right |> move Right |> write '!' |> print)

module type MACHINE = sig
  type t

  val make : state -> state list -> t
  val initial : t -> state
  val add_transition : state -> char -> state -> char -> direction -> t -> t
  val step : t -> state -> Tape.t -> (state * Tape.t) option
  val run : t -> state -> unit
  val speed_run : t -> state -> unit
end

module MachineNeucinkovito : MACHINE = struct
  type t = {
    initial : state;
    transitions : (state * char * state * char * direction) list;
  }

  let make initial _states = { initial; transitions = [] }
  let initial { initial } = initial

  let add_transition st chr st' chr' dir tm =
    { tm with transitions = (st, chr, st', chr', dir) :: tm.transitions }

  let step tm st tape =
    let chr = Tape.read tape in
    match
      List.find_opt
        (fun (st', chr', _, _, _) -> st = st' && chr = chr')
        tm.transitions
    with
    | None -> None
    | Some (_, _, st', chr', dir) ->
        Some (st', tape |> Tape.write chr' |> Tape.move dir)

  let run tm str =
    let rec step' (st, tape) =
      (Tape.print tape;
      print_endline st;
      match step tm st tape with
      | None -> ()
      | Some config' -> step' config')
    in
    step' (initial tm, Tape.make str)

  let speed_run tm str =
  let rec step' (st, tape) =
    match step tm st tape with
    | None -> Tape.print tape
    | Some config' -> step' config'
  in
  step' (initial tm, Tape.make str)
end

(*----------------------------------------------------------------------------*
  Sestavite modul `MachineUcinkovito`, ki učinkovito implementira signaturo
  `MACHINE` z uporabo slovarja, ki ste ga implementirali pred tem. Na kratko
  analizirajte časovno zahtevnost operacij `add_transition` in `step` v
  primerjavi z neučinkovito implementacijo.

  Namig:  
  Za dodatne točke je časovna zahtevnost iskanja prehoda v funkciji
  `speed_run` z nekaj preprocesiranja konstantna.
[*----------------------------------------------------------------------------*)

module MachineUcinkovito : MACHINE = struct
  type t = unit

  let make _ _ = assert false
  let initial _ = assert false
  let add_transition _ _ _ _ _ = assert false
  let step _ _ _ = assert false
  let run _ _ = assert false
  let speed_run _ _ = assert false
end


(*----------------------------------------------------------------------------*
  Sestavite Turingov stroj, ki na vhodnem nizu prepozna palindrom (iz `0` in
  `1`). Če je na vhodnem nizu palindrom, naj na koncu na traku zapiše `1`,
  sicer `0`.
[*----------------------------------------------------------------------------*)

let palindrom_stroj : MachineUcinkovito.t = assert false

(*----------------------------------------------------------------------------*
  Sestavite Turingov stroj, ki na vhod sprejme niz `n` enic in na koncu na
  traku zapiše `n^2` enic.
[*----------------------------------------------------------------------------*)

let kvadrat_stroj : MachineUcinkovito.t = assert false

(*----------------------------------------------------------------------------*
  Sestavite Turingov stroj, ki na začetku na traku sprejme število `n`,
  zapisano v dvojiškem zapisu, na koncu pa naj bo na traku zapisanih
  natanko `n` enic.
[*----------------------------------------------------------------------------*)

let enice_stroj : MachineUcinkovito.t = assert false

(*----------------------------------------------------------------------------*
  Sestavite ravno obratni Turingov stroj, torej tak, ki na začetku na traku
  sprejme število `n` enic, na koncu pa naj bo na traku zapisano število `n`
  v dvojiškem zapisu.
[*----------------------------------------------------------------------------*)

let dvojski_stroj : MachineUcinkovito.t = assert false

(*----------------------------------------------------------------------------*
  Sestavite Turingov stroj, ki na začetku na traku sprejme oklepaje `(` in
  `)`, `[` in `]` ter `{` in `}`. Stroj naj na traku izpiše `1`, če so
  oklepaji pravilno uravnoteženi in gnezdeni, ter `0` sicer.
[*----------------------------------------------------------------------------*)

let uravnotezeni_oklepaji_stroj : MachineUcinkovito.t = assert false
