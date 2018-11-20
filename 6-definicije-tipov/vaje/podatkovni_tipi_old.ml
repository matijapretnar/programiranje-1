(* Vojne čarodejov se nadaljujejo. *)

(* Čarodeji, ki se borijo v vojnah so pripadniki teh treh ras.  *)
type race = Orc | Hobbit | Human

(* Uroki [spells] prihajajo iz treh šol [school] magije: firewall in blaze sta ognjena uroka [Fire],
   resurrect in cripple sta nekromantska [Necrotic], in renounce ter
   banish sta angelska [Angelic].

   Definiraj tipa, ki predstavljata različne uroke in šole magije.
*)

type school = Fire | Necrotic | Angelic

type spell = Blaze | Firewall
           | Renounce | Banish
           | Resurrect | Cripple

(* Veščine [skills], ki jih je čarodej osvojil, so seznam vseh urokov,
   ki jih lahko hitro izvede. Definiraj tip `skills'. *)

type skills = spell list

type mana = int
type health = int

(* Čarodeja opišemo z imenom, številom življenskih točk [hp], sposobnost [ability]
   ki jo predstavimo s številom točk mane, raso [race] in veščino [skills].
   To shranimo kot zapisni tip (record). *)

type wizard = { name : string; hp : health; ability : mana; race : race; skills : spell list}

let string_of_spell = function
  | Blaze -> "Blaze"
  | Firewall -> "Firewall"
  | Renounce -> "Renounce"
  | Banish -> "Banish"
  | Resurrect -> "Resurrect"
  | Cripple -> "Cripple"

(* Napiši funkcijo ki vsakemu uroku priredi primerno šolo magije. *)

let school_of_spell = function
  | Blaze | Firewall -> Fire
  | Renounce | Banish -> Angelic
  | Resurrect | Cripple -> Necrotic

(* Glede na tabelo napiši funkcijo, ki uroku priredi količino mane,
   ki jo čarodej potrebuje za izvršitev uroka:
  blaze : 420
  firewall : 35
  renounce : 17
  banish : 103
  resurrect : 178
  cripple : 250

   Namig: Lahko si pomagaš z regex-replace v Notepad++
 *)
 
let mana_of_spell = function
  | Blaze -> 420
  | Firewall -> 35
  | Renounce -> 17
  | Banish -> 103
  | Resurrect -> 178
  | Cripple -> 250

(* Ustvari nekaj primerov čarodejov, tako kot je prikazano na primeru Merlina.
   Ponovno si lahko pomagaš s regex-replace.*)
(*
name : "Frodo",      ability : 53,   hp : 1000,  skills : [Renounce],                      race : Hobbit
name : "Ajitam",     ability : 1337, hp : 7331,  skills : [Firewall; Resurrect; Firewall], race : Hobbit
name : "Mr Duck",    ability : 7,    hp : 90000, skills : [Cripple],                       race : Orc
name : "Kylo Ren",   ability : 589,  hp : 90,    skills : [Resurrect],                     race : Human
name : "Snoop Dogg", ability : 420,  hp : 4000,  skills : [Blaze],                         race : Orc
*)

(* let merlin = {name = "Merlin";   ability = 1832; hp = 9001; skills = [Renounce; Banish];  race = Human} *)

let merlin = {name = "Merlin";   ability = 1832; hp = 9001; skills = [Renounce; Banish];  race = Human}
let frodo =  {name = "Frodo";    ability = 53;   hp = 1000;  skills = [Renounce];  race = Hobbit}
let ajitam = {name = "Ajitam";   ability = 1337; hp = 7331; skills = [Firewall; Resurrect; Firewall]; race = Hobbit}
let mrDuck = {name = "Mr Duck";  ability = 7;    hp = 90000; skills = [Cripple]; race = Orc}
let kYloReN = {name = "Kylo Ren"; ability = 589;  hp = 90;    skills = [Resurrect]; race = Human}
let snoop_dogg = {name = "Snoop Dogg"; ability = 420; hp = 4000; skills = [Blaze]; race = Orc}

(* Napiši funkcijo, ki iz seznama čarodejev vrne čarodeja z največ mane. *)

let rec strongest_wizard (wizards : wizard list) : wizard option =
  match wizards with
  | [] -> None
  | w :: ws ->
    begin match strongest_wizard ws with
      | None -> Some w
      | Some w' ->
        if w.ability >= w'.ability
        then Some w
        else Some w'
    end
	
(* Posploši funkcijo strongest_wizard na funkcijo max_list, ki sprejme seznam
   in dodatno funkcijo dveh elementov max : 'a -> 'a -> 'a in vrne maksimalni element seznama
   glede na funkcijo max.
*)

let rec max_list (xs : 'a list) (max : 'a -> 'a -> 'a) : 'a option =
  match xs with
  | [] -> None
  | x :: xs ->
    begin match max_list xs max with
      | None -> Some x
      | Some y ->
        Some (max x y)
    end

(* Rase imajo različno občutljivost [vulnerability] na določene šole magije.
   Napiši tip s katerim lahko izraziš kdaj ima rasa visoko [High], navadno [Normal]
   ali pa nizko [Low] občutljivost na urok. *)
   
type vulnerability = Normal | High | Low

(* Napiši funkcijo, ki glede na šolo uroka in raso izračuna občutljivost.

   Low za:     orcs:necrotic, hobbits:fire, humans:angelic,
   High za:    hobbit:necrotic, human:fire, orc:angelic
   Sicer vrne Normal
*)

let effectiveness (school : school) (race : race) : vulnerability =
  match (school, race) with
  | (Necrotic, Orc)
  | (Fire, Hobbit)
  | (Angelic, Human) -> Low
  | (Necrotic, Hobbit)
  | (Fire, Human)
  | (Angelic, Orc) -> High
  | _ -> Normal
  
(* Zapiši funkcijo, ki za čarodeja izračuna njegovo občutljivost na podani urok. *)

let vulnerable spell wizard = effectiveness (school_of_spell spell) wizard.race

(* Občutljivost se v boju izrazi kot koeficient škode, ki jo utrpi čarodej, če ga urok zadane.
   Zapiši funkcijo, ki glede na občutljivost vrne primeren koeficient, tako da čarodej z nizko
   občutljivostjo utrpi le pol škode, čarodej z visoko občutljivostjo pa dvakratnik.*)
   
let coef = function
  | Low -> 0.5
  | Normal -> 1.0
  | High -> 2.0

(* Vsak urok naredi toliko škode, kot je potrebnih točk mane za izvršitev uroka.
   Napiši funkcijo, ki glede na urok in čarodeja izračuna koliko škode utrpi,
   če ga urok zadane.

   Namig: za pretvarjanje med int in float se uporabljata funkciji float_of_int in
   int_of_float.
*)

let damage_caused spell target =
  int_of_float (float_of_int (mana_of_spell spell) *. (coef (vulnerable spell target)))

(* Zapiši funkcijo, ki vrne novo stanje čarodeja (z znižanimi življenskimi točkami [hp]),
   po tem, ko ga je zadel izbrani urok.
   (Novo stanje čarodeja je prav tako tipa wizard)
*)

let attack wizard spell = {wizard with hp = wizard.hp - damage_caused spell wizard}

(* Napiši funkcijo, ki za danega čarovnika izvršuje uroke, dokler ne izvede vseh urokov
   na seznamu, ali pa mu zmanjka točk mane. *)
   
let cast_spells (caster : wizard) : wizard * spell list =
  let m = caster.ability in
  let (available, spells_cast) =
    List.fold_left
      (fun (available, spells_cast) spell ->
         let cost = mana_of_spell spell in
         if cost < available
         then (available - cost, spell::spells_cast)
         else (available, spells_cast))
      (m, [])
      caster.skills
  in
  ({caster with ability = available}, spells_cast)

(* Napiši funkcijo, ki simulira spopad dveh čarodejev. V primeru, ko napadalec ne more izvršiti
   nobenega uroka, napadalec izgubi. V nasprotnem primeru uporabi vse uroke, ki jih lahko.
   Če branilcu zmanjka življenskih točk, izgubi, sicer pa se vlogi napadalca in branilca zamenjata.
*)

let rec duel (attacker : wizard) (defender : wizard) : wizard =
  let defender =
    if attacker.hp > 0
    then
      let (attacker, spells) = cast_spells attacker in
      match spells with
      | [] -> defender
      | _ ->
        let defender = List.fold_left attack defender spells in
        duel defender attacker
    else
      defender
  in defender

let _ = duel frodo snoop_dogg
