type student = {
  ime : string;
  mutable priimek : string;
  vpisna : int;
  ocene : int list;
}

type 'a sklic = { mutable vsebina : 'a }

let sklic x = { vsebina = x }

let ( ! ) s = s.vsebina

let ( := ) s x = s.vsebina <- x
