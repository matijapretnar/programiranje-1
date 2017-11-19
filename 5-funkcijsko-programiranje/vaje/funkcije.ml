(* ===== Vaja 2: Funkcijsko Programiranje  ===== *)

(*Namig: Napiši si pomožno funkcijo za obračanje seznamov. *)
  
(* Funkcija "repeat x n" vrne seznam n ponovitev x. 
 Za neprimerne n funkcija vrne prazen seznam.
 ----------
 # repeat "A" 5;;
 - : string list = ["A"; "A"; "A"; "A"; "A"]
 # repeat "A" (-2);;
 - : string list = []
 ---------- *)

let repeat x n = ()

(* Funkcija "range n" sprejme številio n in vrne seznam vseh celih števil od 0
 do vključno n. Za neprimerne n funkcija vrne prazen seznam.
 Funkcija je repno rekurzivna.
 ----------
 # range 10;;
 - : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
 ---------- *)

let range n = ()

(* Funkcija "map f l" sprejme seznam l = [l0; l1; l2; ...] in funkcijo f
 in vrne seznam [f(l0); f(l1); f(l2); ...].
 ----------
 # let plus_two = (+)2 in
   map plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
 ---------- *)

let map f l = ()

(* Funkcija "map_tlrec" je tail-recursive verzija funkcije map.
 ----------
 # let plus_two = (fun x -> x+2) in
   map_tlrec plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
 ---------- *)

let map_tlrec f l = ()

(* Funkcija "mapi f l" sprejme seznam l = [l0; l1; l2; ...] in funkcijo f
 ter vrne seznam [f 0 l0; f 1 l1; f 2 l2; ...].
 ----------
 # mapi (+) [0; 0; 0; 2; 2; 2];;
 - : int list = [0; 1; 2; 5; 6; 7]
 ---------- *)

let mapi f l = ()

(* Funkcija "zip l1 l2" sprejme seznama l1 = [l1_0; l1_1; l1_2; ...] in
 l2 = [l2_0; l2_1; l2_2; ...] in vrne seznam [(l1_0,l2_0); (l1_1,l2_1); ...].
 Če seznama nista enake dolžine vrne napako.
 ----------
 # zip [1; 1; 1; 1] [0; 1; 2; 3];;
 - : (int * int) list = [(1, 0); (1, 1); (1, 2); (1, 3)]
 # zip [1; 1; 1; 1] [1; 2; 3; 4; 5];;
 Exception: Failure "Seznama razlicnih dolzin.".
 ---------- *)

let zip l1 l2 = ()

(* Funkcija "zip_enum_tlrec l1 l2" sprejme seznama l1 = [l1_0; l1_1; l1_2; ...] 
 in l2 = [l2_0; l2_1; l2_2; ...] in vrne [(0, l1_0, l2_0); (1, l1_1, l2_1); ...].
 Funkcija je repno rekurzivna.
 Če seznama nista enake dolžine vrne napako.
 ----------
 # zip_enum_tlrec ["a"; "b"; "c"; "d"] [7; 3; 4; 2];;
 - : (int * string * int) list = [(0, "a", 7); (1, "b", 3); (2, "c", 4); (3, "d", 2)]
 ---------- *)

let zip_enum_tlrec l1 l2 = ()

(* Funkcija "unzip l" sprejme seznam l = [(a0, b0); (a1, b2); ...]
 in vrne dvojico seznamov ([a0; a1; ...], [b0; b1; ...]).
 ----------
 # unzip [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
 ---------- *)

let unzip l = ()

(* Funkcija "unzip_tlrec l" je tail-recursive verzija funkcije unzip.
 ----------
 # unzip_tlrec [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
 ---------- *)

let unzip_tlrec l = ()

(* Funkcija "fold_left_no_acc f l" sprejme seznam l = [l0; l1; l2; ...; ln] in funkcijo f,
 vrne pa f(... (f (f (f l0 l1) l2) l3) ... ln).
 V primeru seznama z manj kot dvema elementoma vrne napako.
 ----------
 # fold_left_no_acc (^) ["F"; "I"; "K"; "U"; "S"];;
 - : string = "FIKUS"
 ---------- *)

let fold_left_no_acc f l = ()

(* Funkcija "apply_sequence f x n" vrne seznam zaporednih uporab funkcije f na x,
 [x; f x; f (f x); ...; f uporabljena n-krat na x].
 Funkcija je repno rekurzivna.
 ----------
 # apply_sequence (fun x -> x*x) 2 5;;
 - : int list = [2; 4; 16; 256; 65536; 4294967296]
 # apply_sequence (fun x -> x*x) 2 (-5);;
 - : int list = []
 ---------- *)

let apply_sequence f x n = ()

(* Funkcija "filter f l" sprejme seznam l in vrne seznam elementov l,
 za katere funkcija f vrne true.
 ----------
 # filter ((<)3) [0; 1; 2; 3; 4; 5];;
 - : int list = [4; 5]
 ---------- *)

let filter f l = ()

(* Funkcija "exists f l" sprejme seznam l in vrne true če obstaja 
element
 seznama l, za katerega fukcija f vrne true in false sicer.
 Funkcija je repno rekurzivna.
 ----------
 # exists ((<)3) [0; 1; 2; 3; 4; 5];;
 - : bool = true
 # exists ((<)8) [0; 1; 2; 3; 4; 5];;
 - : bool = false
 ---------- *)

let exists f l = ()

(* Funkcija "first f none_value l" sprejme seznam l in vrne prvi element seznama,
 za katerega funkcija f vrne true, če takšnega elementa ni, pa vrne none_value.
 Funkcija je repno rekurzivna.
 ----------
 # first ((<)3) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 5
 # first ((<)8) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 0
 ---------- *)

let first f none_value l = ()
  
(* Severnjaki napadajo Medbrezje. Kot vrhovni čarodej poznaš zaporedje urokov s katerimi
 lahko Medbrezje zaščitiš pred napadom, zaporedje urokov pa je predstavljeno v seznamu oblike
 [("ime1", vrednost1); ("ime2", vrednost2); ...].
 Na razpolago imaš skupino čarodejov, ki so prav tako predstavljeni v seznamu oblike
 [("ime1", spretnost1); ("ime2", spretnost2); ...].
 Čarodej lahko izvede zaporedje urokov, če je njegova spretnost večja ali enaka skupni
 vrednosti vseh urokov v zaporedju.

 Funkcija "able_protectors spells wizards" vrne seznam imen vseh čarodejov, ki lahko
 samostojno zaščitijo Medbrezje.


 Funkcija "fails_on spells wizards" vrne seznam parov (čarodej, neuspešni urok), kjer
 je neuspešni urok prvi urok v zaporedju, za katerega čarodej nima več dovolj spretnosti.
 Če lahko čarodej zaporedje izvede v celoti, to predstavlja prazen niz.

 Namig: Dober čarodej uporablja svoje znanje in izkušnje, ki jih pridobi tekom učenja.

 ----------
 # let spells = [("Protect",51); ("Renounce", 17); ("Blaze", 420); ("Banish",103)] in
   let wizards = [("Merlin", 1832); ("Frodo", 53); ("Ajitam", 1337);
  ("Mr Duck", 7); ("Kylo Ren", 589); ("Snoop Dogg", 123)] in
   able_protectors spells wizards;;
 - : string list = ["Merlin"; "Ajitam"]
 ----------
 # let spells = [("Protect",51); ("Renounce", 17); ("Blaze", 420); ("Banish",103)] in
   let wizards = [("Merlin", 1832); ("Frodo", 53); ("Ajitam", 1337);
  ("Mr Duck", 7); ("Kylo Ren", 589); ("Snoop Dogg", 123)] in
   fails_on spells wizards;;
 - : (string * string) list = [("Merlin", ""); ("Frodo", "Renounce"); ("Ajitam", "");
  ("Mr Duck", "Protect"); ("Kylo Ren", "Banish"); ("Snoop Dogg", "Blaze")]
 ----------*)

let able_protectors spells wizards = ()

let fails_on spells wizards = ()
