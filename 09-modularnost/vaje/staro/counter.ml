(* As simple a module as it gets. A bit boring without ref's. *)
module type COUNTER = sig
    type t

    val initialise : int -> t
    val set : int -> t -> t
    val get : t -> int
    val inc : t -> t
  end

module Counter : COUNTER = struct
  type t = int
  let initialise n = n
  let set n cnt = n
  let get cnt = cnt
  let inc n = n+1
end

module Verbose_Counter : COUNTER = struct
  type t = int

  let initialise n =
    print_endline (Printf.sprintf "initialised counter to %d" n);
    n
  let set n cnt =
    print_endline (Printf.sprintf "setting counter from %d to %d" n cnt);
    n
  let get cnt =
    print_endline (Printf.sprintf "getting counter at %d" cnt);
    cnt
  let inc n =
    print_endline (Printf.sprintf "incrementing counter from %d to %d" n (n+1));
    n+1
end
