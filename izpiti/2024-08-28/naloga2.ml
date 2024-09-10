(*----------------------------------------------------------------------------*]
    Imejmo preprost procesor, sestavljen iz treh komponent:
    - tabele ukazov `instructions`
    - kazalca na trenutni ukaz `instruction_pointer`
    - pomnilnika, ki je predstavljen s tabelo celih števil `memory`

    Mesta v pomnilniku so oštevilčena z naslovi #0, #1, …, ukazi procesorja pa so:
    - `INC a`, ki za 1 poveča vrednost na mestu `a` v pomnilniku,
    - `DEC a`, ki za 1 zmanjša vrednost na mestu `a` v pomnilniku,
    - `COPY a1 a2`, ki na mesto `a2` shrani vrednost, shranjeno na mestu `a1`,
    - `WRITE x a`, ki na mesto `a` shrani število `x`.
    - `JMP ip`, ki kazalec ukaza nastavi na `ip`,
    - `JMPZ a ip`, ki kazalec ukaza nastavi na `ip`,
        če je na mestu `a` v pomnilniku shranjeno število 0,
    Program se konča, ko kazalec ukaza skoči iz tabele ukazov.

    Na primer, program
      0 COPY #0 #2
      1 COPY #1 #3
      2 JMPZ #3 6
      3 INC #2
      4 DEC #3
      5 JMP 2
    v #2 zapiše vsoto števil na mestih #0 in #1 tako, da najprej #0 prekopira v #2,
    #1 pa v #3, nato pa toliko časa povečuje #2 in zmanjšuje #3,
    dokler #2 ne doseže 0.
  [*----------------------------------------------------------------------------*)

type address = Address of int

type instruction =
  | INC of address
  | DEC of address
  | COPY of address * address
  | WRITE of int * address
  | JMP of int
  | JMPZ of address * int

type state = {
  instructions : instruction array;
  instruction_pointer : int;
  memory : int array;
}

let example =
  {
    instructions =
      [|
        (* 0 *) COPY (Address 0, Address 2);
        (* 1 *) COPY (Address 1, Address 3);
        (* 2 *) JMPZ (Address 3, 6);
        (* 3 *) INC (Address 2);
        (* 4 *) DEC (Address 3);
        (* 5 *) JMP 2;
      |];
    instruction_pointer = 0;
    memory = [| 20; 22; 0; 0 |];
  }

(* Spodnje pomožne funkcije lahko uporabite za testiranje delovanja procesorja. *)
let string_of_address (Address a) = "#" ^ string_of_int a

let string_of_instruction = function
  | INC a -> "INC " ^ string_of_address a
  | DEC a -> "DEC " ^ string_of_address a
  | COPY (addr1, addr2) ->
      "COPY " ^ string_of_address addr1 ^ " " ^ string_of_address addr2
  | WRITE (i, a) -> "WRITE " ^ string_of_int i ^ " " ^ string_of_address a
  | JMP ip -> "JMP " ^ string_of_int ip
  | JMPZ (a, ip) -> "JMPZ " ^ string_of_address a ^ " " ^ string_of_int ip

let print_memory mem =
  Array.iteri
    (fun a i ->
      print_endline (string_of_address (Address a) ^ ": " ^ string_of_int i))
    mem

let print_state state =
  print_endline ("IP: " ^ string_of_int state.instruction_pointer);
  print_memory state.memory

(*----------------------------------------------------------------------------*]
    Sestavite funkcijo `increase_instruction_pointer : state -> state`,
    ki za 1 poveča kazalec ukaza.
  [*----------------------------------------------------------------------------*)
let increase_instruction_pointer state =
  { state with instruction_pointer = state.instruction_pointer + 1 }

(*----------------------------------------------------------------------------*]
    Sestavite funkciji `read_memory : state -> address -> int`, ki prebere
    vrednost, shranjeno na danem mestu pomnilnika, ter
    `write_memory : int -> address -> state -> state`, ki na dano mesto v
    pomnilniku zapiše dano vrednost. Tabelo prvotnega stanja naj funkcija pusti
    nespremenjeno, vrne pa naj stanje s posodobljeno kopijo prvotne tabele.
  [*----------------------------------------------------------------------------*)

let read_memory state (Address a) = state.memory.(a)

let write_memory i (Address a) state =
  let memory = Array.copy state.memory in
  memory.(a) <- i;
  { state with memory }

(*----------------------------------------------------------------------------*]
    Sestavite funkcijo `step : state -> instruction -> state`, ki na danem stanju
    izvede dani ukaz ter vrne novo stanje. Pri tem ne pozabite, da z izjemo skokov
    po vsakem izvedenem ukazu povečamo kazalec ukaza. Tudi tu tabelo prvotnega
    stanja pustite nespremenjeno in v končnem stanju vrnite njeno ustrezno
    posodobljeno kopijo.
  [*----------------------------------------------------------------------------*)

let step state = function
  | INC a ->
      let i = read_memory state a in
      state |> write_memory (i + 1) a |> increase_instruction_pointer
  | DEC a ->
      let i = read_memory state a in
      state |> write_memory (i - 1) a |> increase_instruction_pointer
  | COPY (addr1, addr2) ->
      let i = read_memory state addr1 in
      state |> write_memory i addr2 |> increase_instruction_pointer
  | WRITE (i, a) -> state |> write_memory i a |> increase_instruction_pointer
  | JMP ip -> { state with instruction_pointer = ip }
  | JMPZ (a, ip) ->
      let i = read_memory state a in
      if i = 0 then { state with instruction_pointer = ip }
      else increase_instruction_pointer state

(*----------------------------------------------------------------------------*]
    Sestavite funkcijo `run : state -> int array`, ki na procesorju v danem stanju
    toliko časa izvaja ukaze, dokler kazalec ukazov ne skoči iz tabele. Ko se to
    zgodi, vrnite končno stanje pomnilnika.
  [*----------------------------------------------------------------------------*)
let rec run state =
  print_state state;
  print_endline "------------";
  let ip = state.instruction_pointer in
  if ip < Array.length state.instructions then
    let state' = step state state.instructions.(ip) in
    run state'
  else state.memory

let _ = example |> run |> print_memory
