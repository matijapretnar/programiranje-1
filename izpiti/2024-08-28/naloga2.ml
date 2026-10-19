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

(* 2. a *)

(* 2. b *)

(* 2. c *)

(* 2. d *)
