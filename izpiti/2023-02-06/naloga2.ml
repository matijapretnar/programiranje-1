type combinator_operator = And | Or
type atom = Variable of string | Constant of bool

type combinator = {
  operator : combinator_operator;
  children : formula list;
  negated : bool;
}

and formula =
  | Combinator of combinator
  | Leaf of { value : atom; negated : bool }

let example =
  Combinator
    {
      operator = And;
      negated = false;
      children =
        [
          Leaf { value = Variable "x"; negated = true };
          Combinator
            {
              operator = Or;
              negated = true;
              children = [ Leaf { value = Constant true; negated = false } ];
            };
        ];
    }

(* 1. a) *)

(* 1. b) *)

(* 1. c) *)

(* 1. d) *)

(* 1. e) *)
