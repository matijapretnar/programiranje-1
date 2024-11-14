(*----------------------------------------------------------------------------*
 # 2. domača naloga
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Pri tej nalogi boste napisali svoj simulator računalnika, ki se bo malenkostno
 razlikoval od [tistega, ki smo ga spoznali na
 predavanjih](https://schweigi.github.io/assembler-simulator/):
 - Simulator bo uporabljal Harvardsko arhitekturo, kar pomeni, da bo ločil med
 pomnilnikoma za program in podatke.
 - Namesto pomnilnika z omejeno velikostjo bomo imeli samo sklad, ki ga bomo
 predstavili s poljubno velikim seznamom.
 - Prav tako ne bomo vsega predstavili z 8-bitnimi števili. Za ukaze bomo
 definirali svoj naštevni tip, števila v pomnilniku pa bodo taka, kot jih
 podpira OCaml.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Podatkovni tipi
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Pri vsakem večjem funkcijskem programu je prvi korak definicija ustreznih
 tipov. V simulatorju bomo imeli dva glavna tipa: `instruction`, s katerim bomo
 predstavili posamezne ukaze v programu, in `state`, s katerim bomo predstavili
 trenutno stanje računalnika. Seveda pa si bomo morali pred njima definirati še
 nekaj pomožnih tipov.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ### Registri
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Nekateri ukazi za argument sprejmejo register, ki ga spreminjajo, na primer:
 `INC A` ali `POP B`.

 Definirajte naštevni tip `register`, ki bo predstavljal štiri možne registre
 procesorja **A**, **B**, **C** in **D**.
[*----------------------------------------------------------------------------*)

type register 

(* let primer_tipi_1 = [[A; B; B; A]; [A; C; D; C]] *)
(* val primer_tipi_1 : register list list = [[A; B; B; A]; [A; C; D; C]] *)

(*----------------------------------------------------------------------------*
 ### Izrazi
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Nekateri ukazi poleg registra sprejmejo še dodaten argument, ki je lahko bodisi
 register, bodisi celoštevilska konstanta, na primer `MOV A, B` ali `MOV A, 42`.
 Definirajte naštevni tip `expression`, ki predstavlja izraze, ki so lahko
 registri ali števila.
[*----------------------------------------------------------------------------*)

type expression 

(* let primer_tipi_2 = [Register B; Const 42] *)
(* val primer_tipi_2 : expression list = [Register B; Const 42] *)

(*----------------------------------------------------------------------------*
 ### Naslovi
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Ukazi za skoke za argument sprejmejo naslov ukaza v pomnilniku. Naslove bomo
 predstavili s celimi števili, da pa jih ne bi ponesreči zamešali s
 celoštevilskimi konstantami, definirajte še tip `address`, ki naj bo naštevni
 tip z eno samo varianto `Address` s celoštevilskim argumentom.
[*----------------------------------------------------------------------------*)

type address 

(* let primer_tipi_3 = (42, Address 42) *)
(* val primer_tipi_3 : int * address = (42, Address 42) *)

(*----------------------------------------------------------------------------*
 ### Ukazi
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Naš simulator bo podpiral naslednje ukaze, pri čemer je _R_ vedno poljuben
 register, _A_ naslov v ukaznem pomnilniku, _E_ pa izraz, torej bodisi register
 bodisi celoštevilska konstanta.

 ukaz                      | opis
 ------------------------: | ---------------------------------------------------
 --------------------------------------------------
 `MOV` _R_, _E_            | premakni vrednost izraza _E_ v register _R_
 `ADD`/`SUB` _R_, _E_      | register _R_ povečaj/zmanjšaj za _E_
 `INC`/`DEC` _R_           | register _R_ povečaj/zmanjšaj za 1
 `MUL`/`DIV` _E_           | register **A** pomnoži/deli z _E_
 `AND`/`OR`/`XOR` _R_, _E_ | v register _R_ shrani rezultat logične operacije _R
 op E_
 `NOT` _R_                 | negiraj register _R_
 `CMP` _R_, _E_            | primerjaj register _R_ z vrednostjo _E_ ter
 rezultat primerjave shrani v zastavici **Zero** in **Carry**
 `JMP` _A_                 | skoči na naslov _A_
 `JA`/`JAE` _A_            | skoči na naslov _A_, če je v zadnji primerjavi
 veljalo _x > y_ / _x ≥ y_
 `JB`/`JBE` _A_            | skoči na naslov _A_, če je v zadnji primerjavi
 veljalo _x < y_ / _x ≤ y_
 `JE`/`JNE` _A_            | skoči na naslov _A_, če je v zadnji primerjavi
 veljalo _x = y_ / _x ≠ y_
 `CALL` _A_                | skoči na naslov _A_ in shrani naslov naslednjega
 ukaza na vrh sklada
 `RET`                     | iz funkcije se vrni na naslov na vrhu sklada
 `PUSH` _E_                | vrednost izraza _E_ shrani na vrh sklada
 `POP` _R_                 | snemi vrednost s sklada in jo shrani v register _R_
 `HLT`                     | ustavi izvajanje programa
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Dopolnite naslednjo definicijo tipa `instruction`, da bo imel eno varianto za
 vsakega od zgoraj navedenih ukazov:
[*----------------------------------------------------------------------------*)

type instruction =
  | MOV of register * expression
  (* | ADD of TODO *)
  (* | SUB of TODO *)
  | INC of register
  (* | DEC of TODO *)
  (* | MUL of TODO *)
  | DIV of expression
  (* | AND of TODO *)
  (* | OR of TODO *)
  (* | XOR of TODO *)
  (* | NOT of TODO *)
  (* | CMP of TODO *)
  | JMP of address
  (* | JA of TODO *)
  (* | JAE of TODO *)
  (* | JB of TODO *)
  (* | JBE of TODO *)
  (* | JE of TODO *)
  (* | JNE of TODO *)
  | CALL of address
  | RET
  | PUSH of expression
  | POP of register
  | HLT

(* let primer_tipi_4 = [ MOV (A, Register B); MOV (C, Const 42); JA (Address 10); HLT ] *)
(* val primer_tipi_4 : instruction list =
  [MOV (A, Register B); MOV (C, Const 42); JA (Address 10); HLT] *)

(*----------------------------------------------------------------------------*
 Za primer večjega programa se spomnimo programa za izračun Fibonaccijevih
 števil. S seznamom ukazov, bi ga napisali kot spodaj. Pri tem opazite, da so
 naslovi ukazov v programu zapisani kot celoštevilski indeksi. Pretvorbo iz
 berljivih oznak kot so `main`, `fib` in `.fib_end` bomo obravnavali kasneje.
[*----------------------------------------------------------------------------*)

(* let fibonacci n = [
  JMP (Address 20);       (* JMP main *)

(* fib: *)
  (* ; Shranimo vrednosti registrov *)
  PUSH (Register C);      (* PUSH C *)
  PUSH (Register B);      (* PUSH B *)

  (* ; V C shranimo začetno vrednost A *)
  MOV (C, Register A);    (* MOV C, A *)

  (* ; Če je A = 0, je to tudi rezultat *)
  CMP (A, Const 0);       (* CMP A, 0 *)
  JE (Address 17);        (* JE .fib_end *)

  (* ; Če je A = 1, je to tudi rezultat *)
  CMP (A, Const 1);       (* CMP A, 1 *)
  JE (Address 17);        (* JE .fib_end *)

  (* ; V nasprotnem primeru najprej izračunamo fib(A - 1) in ga shranimo v B *)
  DEC C;                  (* DEC C *)
  MOV (A, Register C);    (* MOV A, C *)
  CALL (Address 1);       (* CALL fib *)
  MOV (B, Register A);    (* MOV B, A *)

  (* ; Nato izračunamo še fib(A - 2) in ga shranimo v A *)
  DEC C;                  (* DEC C *)
  MOV (A, Register C);    (* MOV A, C *)
  CALL (Address 1);       (* CALL fib *)
  
  (* ; Nazadnje k A prištejemo še B, s čimer dobimo končni rezultat *)
  ADD (A, Register B);    (* ADD A, B *)
  JMP (Address 17);       (* JMP .fib_end *)

(* .fib_end: *)
  (* ; Povrnemo vrednosti registrov in vrnemo rezultat *)
  POP B;                  (* POP B *)
  POP C;                  (* POP C *)
  RET;                    (* RET *)

(* main: *)
  MOV (A, Const n);       (* MOV A, n *)
  CALL (Address 1);       (* CALL fib *)
  HLT;                    (* HLT *)
] *)
(* val fibonacci : int -> instruction list = <fun> *)

(* let primer_tipi_5 = fibonacci 10 *)
(* val primer_tipi_5 : instruction list =
  [JMP (Address 20); PUSH (Register C); PUSH (Register B);
   MOV (C, Register A); CMP (A, Const 0); JE (Address 17); CMP (A, Const 1);
   JE (Address 17); DEC C; MOV (A, Register C); CALL (Address 1);
   MOV (B, Register A); DEC C; MOV (A, Register C); CALL (Address 1);
   ADD (A, Register B); JMP (Address 17); POP B; POP C; RET;
   MOV (A, Const 10); CALL (Address 1); HLT] *)

(*----------------------------------------------------------------------------*
 ### Pomnilnik
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Morda v nasprotju s pričakovanji ukazov ne bomo shranjevali v sezname tipa
 `instruction list`, ampak v tabele tipa `instruction array`. O tabelah se bomo
 še pogovarjali, njihova bistvena prednost pa je ta, da do elementa na danem
 mestu lahko dostopamo takoj, ne da bi se morali sprehoditi po predhodnih
 elementih. Tabele pišemo tako kot sezname, le da oklepaje pišemo kot `[| ...
 |]` namesto kot `[ ... ]`, do posameznega elementa tabele pa dostopamo prek
 `tabela.(indeks)`, na primer `[| 314; 42; 2718 |].(1)` vrne `42`.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Nazadnje bomo celotno stanje računalnika predstavili z zapisnim. Definirajte
 tip `state` s sledečimi polji:
 - `instructions`: tabela ukazov v ukaznem pomnilniku,
 - `a`, `b`, `c`, `d`: štiri celoštevilske vrednosti v registrih,
 - `ip`: naslov trenutnega ukaza, tipa `address`,
 - `zero`, `carry`: vrednosti zastavic **Zero** in **Carry**,
 - `stack`: seznam celoštevilskih vrednosti na skladu.
[*----------------------------------------------------------------------------*)

type state 

(* let primer_tipi_6 = {
  instructions = [| MOV (A, Register B); MOV (C, Const 42); JA (Address 10); HLT |];
  a = 1; b = 2; c = 3; d = 4;
  ip = Address 0;
  zero = true; carry = false;
  stack = [5; 6; 7];
} *)
(* val primer_tipi_6 : state =
  {instructions =
    [|MOV (A, Register B); MOV (C, Const 42); JA (Address 10); HLT|];
   a = 1; b = 2; c = 3; d = 4; ip = Address 0; zero = true; carry = false;
   stack = [5; 6; 7]} *)

(*----------------------------------------------------------------------------*
 ### Začetno stanje
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Prazno stanje pomnilnika lahko predstavimo z zapisom:
[*----------------------------------------------------------------------------*)

(* let empty = {
  instructions = [||];
  a = 0;
  b = 0;
  c = 0;
  d = 0;
  ip = Address 0;
  zero = false;
  carry = false;
  stack = [];
} *)
(* val empty : state =
  {instructions = [||]; a = 0; b = 0; c = 0; d = 0; ip = Address 0;
   zero = false; carry = false; stack = []} *)

(*----------------------------------------------------------------------------*
 Kljub temu, da so tabele učinkovitejše, so seznami za delo bolj praktični. Zato
 definirajte funkcijo `init : instruction list -> state`, ki sprejme seznam
 ukazov in vrne začetno stanje računalnika, v katerem so vsi registri in
 zastavice nastavljeni na nič, sklad pa je prazen. Pri tem si lahko za pretvorbo
 seznama v tabelo pomagate z uporabo funkcije `Array.of_list`.
[*----------------------------------------------------------------------------*)

let init _ = ()

(* let primer_tipi_7 = init [ MOV (A, Register B); MOV (C, Const 42); JA (Address 10); HLT ] *)
(* val primer_tipi_7 : state =
  {instructions =
    [|MOV (A, Register B); MOV (C, Const 42); JA (Address 10); HLT|];
   a = 0; b = 0; c = 0; d = 0; ip = Address 0; zero = false; carry = false;
   stack = []} *)

(*----------------------------------------------------------------------------*
 ## Izvajanje ukazov
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 S pripravljenima tipoma ukazov in stanja se lahko lotimo pisanja funkcij za
 izvrševanje ukazov.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ### Branje stanja
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `read_instruction : state -> instruction option`, ki v danem
 stanju vrne trenuten ukaz. Če ukaz sega izven območja ukaznega pomnilnika, naj
 funkcija vrne `None`.
[*----------------------------------------------------------------------------*)

let read_instruction _ = ()

(* let primer_izvajanje_1 =
  [
    read_instruction { empty with instructions = [| MOV (A, Register B); MOV (C, Const 42); JA (Address 10); HLT |]; ip = (Address 1) };
    read_instruction { empty with instructions = [| MOV (A, Register B); MOV (C, Const 42); JA (Address 10); HLT |]; ip = (Address 3) };
    read_instruction { empty with instructions = [| MOV (A, Register B); MOV (C, Const 42); JA (Address 10); HLT |]; ip = (Address 5) };
  ] *)
(* val primer_izvajanje_1 : instruction option list =
  [Some (MOV (C, Const 42)); Some HLT; None] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `read_register : state -> register -> int`, ki vrne vrednost
 registra v danem stanju.
[*----------------------------------------------------------------------------*)

let read_register _ _ = ()

(* let primer_izvajanje_2 =
  read_register { empty with a = 10; b = 42 } B *)
(* val primer_izvajanje_2 : int = 42 *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `read_expression : state -> expression -> int`, ki vrne
 celoštevilsko vrednost izraza v danem stanju.
[*----------------------------------------------------------------------------*)

let read_expression _ _ = ()

(* let primer_izvajanje_3 =
  read_expression { empty with a = 10; b = 20 } (Register B) *)
(* val primer_izvajanje_3 : int = 20 *)

(* let primer_izvajanje_4 =
  read_expression { empty with a = 10; b = 20 } (Const 42) *)
(* val primer_izvajanje_4 : int = 42 *)

(*----------------------------------------------------------------------------*
 ### Spreminjanje registrov
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `write_register : state -> register -> int -> state`, ki
 vrednost registra v danem stanju nastavi na dano število. Funkcija naj vrne
 novo stanje.
[*----------------------------------------------------------------------------*)

let write_register _ _ _ = ()

(* let primer_izvajanje_5 =
  write_register { empty with c = 42 } D 24 *)
(* val primer_izvajanje_5 : state =
  {instructions = [||]; a = 0; b = 0; c = 42; d = 24; ip = Address 0;
   zero = false; carry = false; stack = []} *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `perform_unop : (int -> int) -> state -> register -> state`,
 ki izvede eniško operacijo na vrednosti registra. Funkcija naj vrne novo stanje
 s spremenjenim registrom.
[*----------------------------------------------------------------------------*)

let perform_unop _ _ _ = ()

(* let primer_izvajanje_6 =
  perform_unop (fun x -> 101 * x) { empty with c = 5 } C *)
(* val primer_izvajanje_6 : state =
  {instructions = [||]; a = 0; b = 0; c = 505; d = 0; ip = Address 0;
   zero = false; carry = false; stack = []} *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `perform_binop : (int -> int -> int) -> state -> register ->
 expression -> state`, ki izvede dvojiško operacijo na danem registru in izrazu.
 Funkcija naj vrne novo stanje s spremenjenim registrom.
[*----------------------------------------------------------------------------*)

let perform_binop _ _ _ _ = ()

(* let primer_izvajanje_7 =
  perform_binop ( * ) { empty with c = 5 } C (Const 101) *)
(* val primer_izvajanje_7 : state =
  {instructions = [||]; a = 0; b = 0; c = 505; d = 0; ip = Address 0;
   zero = false; carry = false; stack = []} *)

(*----------------------------------------------------------------------------*
 ### Skoki
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `next : address -> address`, ki vrne naslednji naslov (torej
 povečan za 1, saj v našem primeru vsi ukazi zasedejo enako prostora).
[*----------------------------------------------------------------------------*)

let next _ = ()

(* let primer_izvajanje_8 =
  next (Address 41) *)
(* val primer_izvajanje_8 : address = Address 42 *)

(*----------------------------------------------------------------------------*
 Napišite funkciji `jump : state -> address -> state` in `proceed : state ->
 state`. Prva naj v danem stanju skoči na dani naslov, druga pa naj skoči na
 naslednji ukaz.
[*----------------------------------------------------------------------------*)

let jump _ _ = ()
let proceed _ = ()

(* let primer_izvajanje_9 =
  jump { empty with ip = Address 42} (Address 10) *)
(* val primer_izvajanje_9 : state =
  {instructions = [||]; a = 0; b = 0; c = 0; d = 0; ip = Address 10;
   zero = false; carry = false; stack = []} *)

(* let primer_izvajanje_10 =
  proceed { empty with ip = Address 42} *)
(* val primer_izvajanje_10 : state =
  {instructions = [||]; a = 0; b = 0; c = 0; d = 0; ip = Address 43;
   zero = false; carry = false; stack = []} *)

(*----------------------------------------------------------------------------*
 Napišite funkciji `push_stack : state -> int -> state` in `pop_stack : state ->
 int * state`, ki dodata vrednost na sklad oziroma jo odstranita z njega.
 Funkcija `pop_stack` poleg spremenjenega stanja vrne tudi odstranjeno vrednost.
 Če je sklad prazen, naj funkcija `pop_stack` sproži izjemo.
[*----------------------------------------------------------------------------*)

let push_stack _ _ = ()
let pop_stack _ = ()

(* let primer_izvajanje_10 =
  push_stack { empty with stack = [1; 2; 3] } 42 *)
(* val primer_izvajanje_10 : state =
  {instructions = [||]; a = 0; b = 0; c = 0; d = 0; ip = Address 0;
   zero = false; carry = false; stack = [42; 1; 2; 3]} *)

(* let primer_izvajanje_11 =
  pop_stack { empty with stack = [1; 2; 3] } *)
(* val primer_izvajanje_11 : int * state =
  (1,
   {instructions = [||]; a = 0; b = 0; c = 0; d = 0; ip = Address 0;
    zero = false; carry = false; stack = [2; 3]}) *)

(*----------------------------------------------------------------------------*
 ### Pogojni skoki
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `compare : state -> int -> int -> state`, ki primerja
 vrednosti dveh števil in ustrezno nastavi zastavici **Zero** in **Carry**. Prvo
 naj nastavi na `true` natanko tedaj, kadar sta števili enaki, drugo pa takrat,
 kadar je prvo število manjše.Funkcija naj vrne novo stanje.
[*----------------------------------------------------------------------------*)

let compare _ _ _ = ()

(* let primer_izvajanje_12 =
  compare empty 24 42 *)
(* val primer_izvajanje_12 : state =
  {instructions = [||]; a = 0; b = 0; c = 0; d = 0; ip = Address 0;
   zero = false; carry = true; stack = []} *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `conditional_jump : state -> bool -> address -> state`, ki
 skoči na dani naslov, če je podan pogoj izpolnjen. V nasprotnem primeru naj
 funkcija skoči na naslednji ukaz.
[*----------------------------------------------------------------------------*)

let conditional_jump _ _ _ = ()

(* let primer_izvajanje_13 =
  conditional_jump { empty with ip = Address 42 } (Address 10) true *)
(* val primer_izvajanje_13 : state =
  {instructions = [||]; a = 0; b = 0; c = 0; d = 0; ip = Address 10;
   zero = false; carry = false; stack = []} *)

(* let primer_izvajanje_14 =
  conditional_jump { empty with ip = Address 42 } (Address 10) false *)
(* val primer_izvajanje_14 : state =
  {instructions = [||]; a = 0; b = 0; c = 0; d = 0; ip = Address 43;
   zero = false; carry = false; stack = []} *)

(*----------------------------------------------------------------------------*
 ### Klici funkcij
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `call : state -> address -> state`, ki v danem stanju skoči
 na dani naslov in na sklad doda naslednji naslov.
[*----------------------------------------------------------------------------*)

let call _ _ = ()

(* let primer_izvajanje_15 =
  call { empty with ip = Address 42 } (Address 10) *)
(* val primer_izvajanje_15 : state =
  {instructions = [||]; a = 0; b = 0; c = 0; d = 0; ip = Address 10;
   zero = false; carry = false; stack = [43]} *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `return : state -> state`, ki v danem stanju skoči na naslov,
 ki je na vrhu sklada, in odstrani ta naslov s sklada.
[*----------------------------------------------------------------------------*)

let return _ = ()

(* let primer_izvajanje_16 =
  return { empty with ip = (Address 100); stack = [42; 43; 44] } *)
(* val primer_izvajanje_16 : state =
  {instructions = [||]; a = 0; b = 0; c = 0; d = 0; ip = Address 42;
   zero = false; carry = false; stack = [43; 44]} *)

(*----------------------------------------------------------------------------*
 ### Izvajanje programov
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 S pomočjo zgoraj definiranih funkcij dopolnite funkcijo `run_instruction :
 state -> instruction -> state`, ki izvede podani ukaz v danem stanju in vrne
 novo stanje. Za interpretacije pogojnih skokov si lahko pomagate z [navodili
 simulatorja](https://schweigi.github.io/assembler-simulator/instruction-
 set.html), ki smo ga pogledali na predavanjih.
[*----------------------------------------------------------------------------*)

(* let run_instruction st = function
  (* | MOV (reg, exp) -> TODO *)
  | ADD (reg, exp) -> perform_binop ( + ) st reg exp |> proceed
  (* | SUB (reg, exp) -> TODO *)
  | INC reg -> perform_unop succ st reg |> proceed
  (* | DEC reg -> TODO *)
  (* | MUL exp -> TODO *)
  (* | DIV exp -> TODO *)
  (* Pozor, OCaml land/lor/lxor interpretira kot simbole, zato jih pišemo infiksno! *)
  | AND (reg, exp) -> perform_binop ( land ) st reg exp |> proceed
  | OR (reg, exp) -> perform_binop ( lor ) st reg exp |> proceed
  | XOR (reg, exp) -> perform_binop ( lxor ) st reg exp |> proceed
  | NOT reg -> perform_unop lnot st reg |> proceed
  (* | CMP (reg, exp) -> TODO *)
  (* | JMP add -> TODO *)
  | JA add -> conditional_jump st add (not st.carry && not st.zero)
  (* | JAE add -> TODO *)
  (* | JB add -> TODO *)
  (* | JBE add -> TODO *)
  (* | JE add -> TODO *)
  (* | JNE add -> TODO *)
  (* | CALL add -> TODO *)
  (* | RET -> TODO *)
  | PUSH exp -> push_stack st (read_expression st exp) |> proceed
  | POP reg ->
      let n, st' = pop_stack st in
      write_register st' reg n |> proceed
  | HLT -> failwith "Cannot execute instruction" *)
(* val run_instruction : state -> instruction -> state = <fun> *)

let run_instruction _ _ = ()

(*----------------------------------------------------------------------------*
 Napišite funkcijo `run_program : state -> state`, ki izvaja ukaze v danem
 stanju, dokler ne naleti na ukaz `HLT` ali pa ukazni kazalec skoči ven iz
 ukaznega pomnilnika. Funkcija naj vrne končno stanje.
[*----------------------------------------------------------------------------*)

let rec run_program _ = ()

(* let primer_izvajanje_16 =
  fibonacci 10
  |> init
  |> run_program *)
(* val primer_izvajanje_16 : state =
  {instructions =
    [|JMP (Address 20); PUSH (Register C); PUSH (Register B);
      MOV (C, Register A); CMP (A, Const 0); JE (Address 17);
      CMP (A, Const 1); JE (Address 17); DEC C; MOV (A, Register C);
      CALL (Address 1); MOV (B, Register A); DEC C; MOV (A, Register C);
      CALL (Address 1); ADD (A, Register B); JMP (Address 17); POP B; 
      POP C; RET; MOV (A, Const 10); CALL (Address 1); HLT|];
   a = 55; b = 0; c = 0; d = 0; ip = Address 22; zero = true; carry = false;
   stack = []} *)

(*----------------------------------------------------------------------------*
 ## Branje zbirnika
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Da bomo programe lahko pisali v zbirniku, napišimo še funkcije za branje nizov.
 Predpostavljate lahko, da bodo vsi nizi pravilno oblikovani, zato v primeru
 napake s `failwith ...` javite ustrezno sporočilo o napaki.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ### Registri in izrazi
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `parse_register : string -> register`, ki iz niza prebere
 register.
[*----------------------------------------------------------------------------*)

let parse_register _ = ()

let primer_branje_1 = parse_register "A"
(* val primer_branje_1 : register = A *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `parse_expression : string -> expression`, ki iz niza prebere
 izraz.
[*----------------------------------------------------------------------------*)

let parse_expression _ = ()

let primer_branje_2 = parse_expression "A"
(* val primer_branje_2 : expression = Register A *)

let primer_branje_3 = parse_expression "42"
(* val primer_branje_3 : expression = Const 42 *)

(*----------------------------------------------------------------------------*
 ### Čiščenje vrstic
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `clean_line : string -> string`, ki iz niza odstrani vse
 presledke in komentarje (ki se začnejo z znakom `;`). Pri iskanju in
 odstranjevanju komentarjev si pomagajte z uporabo funkcij `String.index_opt` in
 `String.sub`.
[*----------------------------------------------------------------------------*)

let clean_line _ = ()

let primer_branje_4 = clean_line "   MOV A, 42    ; To je komentar   "
(* val primer_branje_4 : string = "MOV A, 42" *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `clean_lines : string list -> string list`, ki iz seznama
 nizov najprej odstrani vse komentarje in presledke, nato pa odstrani vse prazne
 vrstice.
[*----------------------------------------------------------------------------*)

let clean_lines _ = ()

(*----------------------------------------------------------------------------*
 ### Oznake
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Kot smo navajeni iz zbirnika, skokov ne podajamo z indeksi, ampak raje v dele
 kode napišemo oznake kot so `main:` ali `.loop:`, nato pa se nanje sklicujemo
 kot `JA .loop`, `JMP main`, `CALL fib` in tako naprej. Oznake bomo hranili v
 seznamu, ki bo vsaki oznaki priredil ustrezen naslov v ukaznem pomnilniku.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `parse_address : (string * address) list -> string ->
 address`, ki pri danem seznamu oznak iz niza prebere naslov. Naslov je lahko
 podan direktno s številom ali pa z eno izmed oznak v seznamu.
[*----------------------------------------------------------------------------*)

let parse_address _ _ = ()

(* let primer_branje_5 = parse_address [("main", Address 42)] "main" *)
(* val primer_branje_5 : address = Address 42 *)

(* let primer_branje_6 = parse_address [("main", Address 42)] "123" *)
(* val primer_branje_6 : address = Address 123 *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `parse_label : string -> string option`, ki vrne oznako, če
 se niz konča z dvopičjem, sicer pa vrne `None`.
[*----------------------------------------------------------------------------*)

let parse_label _ = ()

let primer_branje_7 = parse_label "main:"
(* val primer_branje_7 : string option = Some "main" *)

let primer_branje_8 = parse_label "MOV A, 42"
(* val primer_branje_8 : string option = None *)

(*----------------------------------------------------------------------------*
 Da bomo iz kode določili oznake, napišite funkcijo `parse_labels : string list
 -> (string * address) list * string list`, ki iz seznama nizov, ki so bodisi
 oznake bodisi ukazi, izloči oznake in jim priredi naslove, ostale vrstice pa
 pusti nespremenjene.
[*----------------------------------------------------------------------------*)

let parse_labels _ = ()

let primer_branje_9 =
  parse_labels ["JMP main"; "main:"; "MOV A, 0"; "loop:"; "INC A"; "JMP loop"]
(* val primer_branje_9 : (string * address) list * string list =
  ([("loop", Address 2); ("main", Address 1)],
   ["JMP main"; "MOV A, 0"; "INC A"; "JMP loop"]) *)

(*----------------------------------------------------------------------------*
 Dopolnite spodnjo funkcijo `parse_instruction : (string * address) list ->
 string -> instruction`, ki iz niza prebere ukaz.
[*----------------------------------------------------------------------------*)

(* let parse_instruction labels line =
  let tokens =
    line
    |> String.split_on_char ' '
    |> List.concat_map (String.split_on_char ',')
    |> List.map String.trim
    |> List.filter (fun token -> token <> "")
  in
  match tokens with
  | ["MOV"; reg; exp] -> MOV (parse_register reg, parse_expression exp)
  (* | ["ADD"; reg; exp] -> TODO *)
  (* | ["SUB"; reg; exp] -> TODO *)
  | ["INC"; reg] -> INC (parse_register reg)
  (* | ["DEC"; reg] -> TODO *)
  (* | ["MUL"; exp] -> TODO *)
  (* | ["DIV"; exp] -> TODO *)
  (* | ["AND"; reg; exp] -> TODO *)
  (* | ["OR"; reg; exp] -> TODO *)
  (* | ["XOR"; reg; exp] -> TODO *)
  (* | ["NOT"; reg] -> TODO *)
  (* | ["CMP"; reg; exp] -> TODO *)
  | ["JMP"; add] -> JMP (parse_address labels add)
  (* | ["JA"; add] -> TODO *)
  (* | ["JAE"; add] -> TODO *)
  (* | ["JB"; add] -> TODO *)
  (* | ["JBE"; add] -> TODO *)
  (* | ["JE"; add] -> TODO *)
  (* | ["JNE"; add] -> TODO *)
  (* | ["CALL"; add] -> TODO *)
  | ["RET"] -> RET
  | ["PUSH"; exp] -> PUSH (parse_expression exp)
  | ["POP"; reg] -> POP (parse_register reg)
  | ["HLT"] -> HLT
  | _ -> failwith ("Invalid instruction: " ^ line) *)

(* let primer_branje_10 =
  List.map (parse_instruction [("main", Address 42)]) ["MOV A, 42"; "CALL main"; "HLT"] *)
(* val primer_branje_10 : instruction list =
  [MOV (A, Const 42); CALL (Address 42); HLT] *)

(*----------------------------------------------------------------------------*
 S pomočjo zgoraj napisanih funkcij sestavite funkcijo `run : string -> state`,
 ki niz razbije na vrstice, prebere ukaze in oznake ter pripravi začetno stanje,
 nato pa program izvaja vse dokler ne naleti na ukaz `HLT`. Po klicu naj
 funkcija vrne končno stanje.
[*----------------------------------------------------------------------------*)

let run _ = ()

let fibonacci = {|
  JMP main
  ; Funkcija, ki izračuna fib(A) in vrednost shrani v register A
  fib:
      ; Shranimo vrednosti registrov
      PUSH C
      PUSH B
  
      ; V C shranimo začetno vrednost A
      MOV C, A
  
      ; Če je A = 0, je to tudi rezultat
      CMP A, 0
      JE .fib_end
  
      ; Če je A = 1, je to tudi rezultat
      CMP A, 1
      JE .fib_end
  
      ; V nasprotnem primeru najprej izračunamo fib(A - 1) in ga shranimo v B
      DEC C
      MOV A, C
      CALL fib
      MOV B, A
  
      ; Nato izračunamo še fib(A - 2) in ga shranimo v A
      DEC C
      MOV A, C
      CALL fib
      
      ; Nazadnje k A prištejemo še B, s čimer dobimo končni rezultat
      ADD A, B
      JMP .fib_end
  
  .fib_end:
      ; Povrnemo vrednosti registrov in vrnemo rezultat
      POP B
      POP C
      RET
  
  main:
      MOV A, 7
      CALL fib
  HLT
|}
(* val fibonacci : string =
  "\n  JMP main\n  ; Funkcija, ki izračuna fib(A) in vrednost shrani v register A\n  fib:\n      ; Shranimo vrednosti registrov\n      PUSH C\n      PUSH B\n  \n      ; V C shranimo začetno vrednost A\n      MOV C, A\n  \n      ; Če je A = 0, je to tudi rezultat\n      CMP A, 0\n      JE .fib_end\n  \n      ; Če"... (* string length 872; truncated *) *)

let primer_branje_11 =
  run fibonacci
(* val primer_branje_11 : state =
  {instructions =
    [|JMP (Address 20); PUSH (Register C); PUSH (Register B);
      MOV (C, Register A); CMP (A, Const 0); JE (Address 17);
      CMP (A, Const 1); JE (Address 17); DEC C; MOV (A, Register C);
      CALL (Address 1); MOV (B, Register A); DEC C; MOV (A, Register C);
      CALL (Address 1); ADD (A, Register B); JMP (Address 17); POP B; 
      POP C; RET; MOV (A, Const 7); CALL (Address 1); HLT|];
   a = 13; b = 0; c = 0; d = 0; ip = Address 22; zero = true; carry = false;
   stack = []} *)
