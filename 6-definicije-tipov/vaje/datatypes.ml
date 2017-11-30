(* The wizard wars are raging. *)

(* The wizards combating in this war are of these races.  *)
type race = Orc | Hobbit | Human


(* There are spells of three schools of magic: firewall and blaze are fire
   spells, resurrect and cripple are of the necrotic school, and renounce and
   banish are angelic.

   Define a type to represent the schools, and a type of spells.
*)


type school = Fire | Necrotic | Angelic


type spell = Blaze | Firewall
           | Renounce | Banish
           | Resurrect | Cripple

(* The skills a wizard has mastered are the list of spells he can cast in one
   round. Define a type `skills'. *)
type skills = spell list

type mana = int
type health = int

(* A wizard has to be given a name, a number of hitpoints (hp), an ability
   level of mana, a race, and the skills he has mastered. Use a record to
   represent this. *)
type wizard = { name : string; hp : health; ability : mana; race : race; skills : spell list}


let string_of_spell = function
  | Blaze -> "Blaze"
  | Firewall -> "Firewall"
  | Renounce -> "Renounce"
  | Banish -> "Banish"
  | Resurrect -> "Resurrect"
  | Cripple -> "Cripple"

(* Write a function that indicates for each spell which school it belongs to. *)
let school_of_spell = function
  | Blaze | Firewall -> Fire
  | Renounce | Banish -> Angelic
  | Resurrect | Cripple -> Necrotic

(* Write a function that computes the mana each spell uses. The values are:
  blaze : 420
  firewall : 35
  renounce : 17
  banish : 103
  resurrect : 178
  cripple : 250

   Hint: use regex-replace in Notepad++
 *)
let mana_of_spell = function
  | Blaze -> 420
  | Firewall -> 35
  | Renounce -> 17
  | Banish -> 103
  | Resurrect -> 178
  | Cripple -> 250

(* Use regex-replace in Notepad++ to build a few example wizards, like merlin *)
(*
name : "Frodo",      ability : 53,   hp : 1000,  skills : [Renounce],                      race : Hobbit
name : "Ajitam",     ability : 1337, hp : 7331,  skills : [Firewall; Resurrect; Firewall], race : Hobbit
name : "Mr Duck",    ability : 7,    hp : 90000, skills : [Cripple],                       race : Orc
name : "Kylo Ren",   ability : 589,  hp : 90,    skills : [Resurrect],                     race : Human
name : "Snoop Dogg", ability : 420,  hp : 4000,  skills : [Blaze],                         race : Orc
*)

let merlin = {name = "Merlin";   ability = 1832; hp = 9001; skills = [Renounce; Banish];  race = Human}
let frodo =  {name = "Frodo";    ability = 53;   hp = 1000;  skills = [Renounce];  race = Hobbit}
let ajitam = {name = "Ajitam";   ability = 1337; hp = 7331; skills = [Firewall; Resurrect; Firewall]; race = Hobbit}
let mrDuck = {name = "Mr Duck";  ability = 7;    hp = 90000; skills = [Cripple]; race = Orc}
let kYloReN = {name = "Kylo Ren"; ability = 589;  hp = 90;    skills = [Resurrect]; race = Human}
let snoop_dogg = {name = "Snoop Dogg"; ability = 420; hp = 4000; skills = [Blaze]; race = Orc}


(* Write a function that computes the wizard with the most mana. *)
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

let rec max_list (xs : 'a list) (max : 'a -> 'a -> 'a) : 'a option =
  match xs with
  | [] -> None
  | x :: xs ->
    begin match max_list xs max with
      | None -> Some x
      | Some y ->
        Some (max x y)
    end

(* Races have either high, normal, or low vulnerability to each school of
   magic. Represent these possibilities as a variant type. *)
type vulnerability = Normal | High | Low

(* Write a function that computes the following vulnerabilities:
   Low vulnerability for orcs:necrotic, hobbits:fire, humans:angelic,
   High vulnerability    hobbit:necrotic, human:fire, orc:angelic
   otherwise normal
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

(* Write a function that computes how vulnerable a wizard is to a spell *)
let vulnerable spell wizard = effectiveness (school_of_spell spell) wizard.race

(* Write a function that computes a damage coefficient. High vulnerability
   incurs double damage, low vulnerability half damage. *)
let coef = function
  | Low -> 0.5
  | Normal -> 1.0
  | High -> 2.0

(* Write a function that calculates how much damages a spell causes to a
   wizard, computed as the mana it uses times the vulnerability coefficient. *)
let damage_caused spell target =
  int_of_float (float_of_int (mana_of_spell spell) *. (coef (vulnerable spell target)))

(* Write a function that calculates the stats of a wizard after getting
   attacked by a particular spell.  *)
let attack wizard spell = {wizard with hp = wizard.hp - damage_caused spell wizard}

(* Write a function cast_spells that casts each of the skills of a wizard, or
   as many as he has mana for. Return the updated caster and the list of
   spells he managed to cast.*)
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


(* Write a function that stands off two wizard in a duel. If the attacker is
   dead, the defender wins. The attacker, if he is still alive, casts his
   spells. If he cannot cast any spells, he loses. After the attacker casts
   his spells, the roles change and the defender takes his turn to attack. *)
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
