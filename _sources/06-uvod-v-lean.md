# Uvod v Lean

Kot smo videli, lahko OCaml uporabljamo za pisanje dokazov, vendar je to precej nerodno. Zato si bomo ogledali Lean, programski jezik, ki je namenjen prav pisanju dokazov. Lean je sicer tudi splošno uporaben programski jezik, vendar se bomo osredotočili predvsem na njegovo uporabo pri dokazovanju.

## Definicije vrednosti

V Leanu funkcije definiramo podobno kot v OCamlu, le da namesto `let` in `=` pišemo `def` in `:=`:

```lean
def podvoji x := 2 * x
```

Lean nima interaktivne konzole tako kot OCaml, temveč se nam v urejevalniku ob strani odpre _InfoView_, v katerem lahko vidimo informacije o trenutno označenem delu kode. Če želimo izračunati preprosto vrednost, lahko uporabimo ukaz `#eval`. Na primer

```lean
#eval podvoji 21
```

Ker bomo Lean prvenstveno uporabljali za preverjanje dokazov, nas končne vrednosti ne bodo zanimale toliko kot tipi. Tako Lean podpira tudi ukaz `#check`, ki nam izpiše tip izraza:

```lean
#check podvoji
```

Vidimo tip `Nat → Nat`, v katerem lahko opazimo nekaj novosti:

- tipe v Leanu običajno pišemo z veliko začetnico
- Lean podpira pisanje kode v Unicodeu, čeprav lahko namesto `→` pišemo tudi `->`.
- Lean ima tudi tip `Nat`, ki ustreza naravnim številom.

Tako kot OCaml je tudi Lean tip funkcije `podvoji` izpeljal samodejno. A običajno bomo tipe pisali sami. Na primer, če želimo funkcijo definirati na celih številih, lahko pišemo:

```lean
def podvoji (x : Int) : Int := 2 * x
```

ali z anonimno funkcijo kot:

```lean
def podvoji : Int → Int := fun x ↦ 2 * x
```

Lean podpira tudi razstavljanje vrednosti. Funkcijo za stikanje seznamov, ki jo v OCamlu poznamo pod `@`, definiramo kot:

```lean
def stakni_seznama_celih (xs : List Int) (ys : List Int) :=
  match xs with
  | [] => ys
  | x :: xs' => x :: stakni_seznama_celih xs' ys

#eval stakni_seznama_celih [1, 2, 3] [4, 5, 6]
```

## Polimorfne funkcije

Zgornja funkcija stika le sezname celih števil, vendar jo lahko naredimo tudi polimorfno. Tu moramo biti malo bolj eksplicitni kot v OCamlu, saj Lean podpira veliko bogatejše tipe, zato moramo bolj natančno povedati, kakšnega želimo. To storimo tako, da funkciji dodamo še dodaten argument `A` tipa `Type`, torej tip.

```lean
def polimorfni_stakni (A : Type) (xs : List A) (ys : List A) :=
  match xs with
  | [] => ys
  | x :: xs' => x :: polimorfni_stakni A xs' ys
```

Curryranje bi lahko podali kot:

```lean
def curry (f : A × B → C) : A → (B → C) :=
  fun x => fun y => f ⟨x, y⟩
```

## Implicitni argumenti

Ko tako funkcijo kličemo, moramo tudi tip podati eksplicitno:

```lean
#check polimorfni_stakni Bool [true, false, true] [true, true]
#check polimorfni_stakni Int [1, 2, 3] [4, 5, 6]
```

Ker je to precej gostobesedno, Lean podpira tudi _implicitne argumente_, ki jih bo poskusil izpolniti sam s pomočjo drugih argumentov. Take argumente pišemo v zavitih oklepajih in jih pri klicih funkcij izpustimo. Na primer, če staknemo dva seznama, mora Lean že za ta dva seznama vedeti, kakšnega tipa sta, zato bo argument `A` lahko izpolnil sam:

```lean
def stakni {A : Type} (xs : List A) (ys : List A) :=
  match xs with
  | [] => ys
  | x :: xs' => x :: stakni xs' ys

#check stakni [true, false, true] [true, true]
#check stakni [1, 2, 3] [4, 5, 6]
```

## Rekurzija

Zdaj, ko smo na kratko spoznali Lean, se lahko vrnemo k pisanju dokazov. Ena izmed težav, ki smo jih imeli v OCamlu, je bila neomejena rekurzija, saj smo z njeno pomočjo lahko napisali program poljubnega tipa. V Leanu pa je rekurzija omejena, saj nam dopušča le funkcije, ki se zagotovo ustavijo. Lean to običajno preveri prek _strukturne rekurzije_, pri kateri so argumenti v rekurzivnih klicih sestavni deli prvotnih argumentov. Na primer v zgornjem stikanju seznamov se rekurzivno pokličemo na repu prvega seznama. Tako je ob vsakem klicu argument strukturno manjši, zato se bo izvajanje na neki točki zagotovo končalo. Če bi se pri pisanju zatipkali in poklicali na prvotnem seznamu, bi Lean to definicijo zavrnil:

```lean
def napacni_stakni {A : Type} (xs : List A) (ys : List A) :=
  match xs with
  | [] => ys
  | x :: xs' => x :: napacni_stakni xs ys
```

Če se ozremo nazaj na rekurzivne funkcije, ki smo jih pisali v OCamlu, za skoraj vse vidimo, da so strukturno rekurzivne. V Leanu lahko pišemo tudi funkcije, ki niso strukturno rekurzivne, pa se vseeno ustavijo, na primer Evklidov algoritem, a moramo v tem primeru Lean z ustreznim dokazom prepričati sami.

## Tip `Prop`

Tako kot v OCamlu bi tudi v Leanu lahko zapisali funkcijo, ki ustreza izjavi $((P \lor Q) \Rightarrow R) \implies (P \Rightarrow R) \land (Q \Rightarrow R)$:

```lean
def dist {P Q R : Type} : (Sum P Q → R) → ((P → R) × (Q → R)) :=
  fun f : (Sum P Q → R) ↦ (
    (fun (x : P) ↦ f (Sum.inl x)),
    (fun (y : Q) ↦ f (Sum.inr y))
  )
```

A Lean za izjave ponuja poseben tip `Prop`, ki igra podobno vlogo kot `Type`, le da ga Lean obravnava le med preverjanjem tipov, pri izvajanju pa ga pobriše. Leanovi tipi so namreč tako močni (to bomo spoznali naslednjič), da lahko v njih izrazimo tudi izjave, ki jim zadoščajo vrednosti. Na primer funkcija za stikanje seznamov ima v OCamlu tip `'a list -> 'a list -> 'a list`. V Leanu ji sicer lahko določimo tip `List A → List A → List A`, lahko pa bi dodatno povedali, da je dolžina vrnjenega seznama vsota dolžin argumentov ali celo kaj takega:

$$ (xs @ ys)_i = \begin{cases}
  xs_i & i < |xs| \\
  ys_{i - |xs|} & i ≥ |xs|
\end{cases} $$

Tako izjavo bi lahko v duhu Curry-Howardovega izomorfizma predstavili z ustreznim tipom, sestavljenim iz funkcij, produktov, vsot in odvisnih tipov, ki jih bomo spoznali naslednjič. Težava je, da ob izvajanju funkcija potem ne bi le stikala seznamov, temveč bi tudi ustrezno prerazporejala kose dokaza. To zahteva svoj čas in prostor, sploh ker so dokazi lastnosti funkcij so ponavadi precej daljši od funkcij samih. K sreči moramo dokaze preveriti le pred izvajanjem, kasneje pa jih ne potrebujemo več. Tudi v matematiki lahko dokaze izjav pozabimo, ko smo jih enkrat preverili (razen na ustnih izpitih ali seveda takrat, ko želimo iz njih črpati ideje za druge dokaze).

V ta namen Lean torej ponuja tip `Prop`, ki ga bo Lean ob izvajanju pobrisal. Tako kot imamo konstruktorje tipov kot sta `×` ali `Sum` (njun tip v Leanu je `Type → Type → Type`), imamo tudi ustrezne konstruktorje izjav, na primer `∧` in `∨` tipa `Prop → Prop → Prop`. Z njimi bi zgornjo izjavo ustrezneje predstavili kot:

```lean
def dist_prop {P Q R : Prop} : (P ∨ Q → R) → ((P → R) ∧ (Q → R)) :=
  fun f : (P ∨ Q → R) ↦ And.intro
    (fun (x : P) ↦ f (Or.inl x))
    (fun (y : Q) ↦ f (Or.inr y))
```

Prednost tipa `Prop` je v tem, da lahko z njim predpostavimo tudi izjave, ki nimajo ustrezne konstrukcije. Na primer v Leanu lahko enostavno predpostavimo aksiom izključene tretje možnosti:

```lean
axiom excluded_middle (P : Prop) : P ∨ ¬ P
```

Če bi podobno hoteli narediti pri tipu `Type`, bi moral Lean ob izvajanju skonstruirati funkcijo tipa `Sum P (P → Empty)`, ki seveda ne obstaja. Pri tipu `Prop` te težave ni, saj se vrednost `excluded_middle` ob izvajanju sploh ne bo pojavila.

## Dokazovanje s taktikami

Poleg pisanja dokazov s funkcijami Lean podpira tudi pisanje dokazov s pomočjo _taktik_. Taktike so ukazi, ki računalniku dajo napotke, kako naj konstruira dokaze. Zgornjo izjavo `dist_prop` smo sicer dokazali z neposredno konstrukcijo funkcije, a pri zapletenejših izjavah je to bolj nerodno. Pri pisanju dokazov lahko Leanu z ukazom `by` sporočimo, naj preklopi na taktični način. V njem lahko v pogledu _InfoView_ vidimo trenutni cilj ter vse predpostavke, ki jih imamo na voljo. Izjavo `dist_prop` bi s taktikami dokazali na sledeč način. Seveda bi taktike pisali interaktivno glede na spreminjajoči se cilj. Tu bomo cilje zapisali v komentarje.

```lean
def dist_prop_s_taktikami {P Q R : Prop} : (P ∨ Q → R) → ((P → R) ∧ (Q → R)) := by
  -- cilj: (P ∨ Q → R) → (P → R) ∧ (Q → R)
  intro H  -- vpelje predpostavko 
  -- predpostavka H : P ∨ Q → R
  -- cilj: (P → R) ∧ (Q → R)
  constructor  -- konjunkcijo sestavi iz dveh delov
  · -- cilj: (P → R)
    intro H_P
    -- predpostavka H : P ∨ Q → R
    -- predpostavka H_P : P
    -- cilj: R
    apply H  -- uporabi predpostavko H
    -- predpostavka H : P ∨ Q → R
    -- predpostavka H_P : P
    -- cilj: P ∨ Q
    left  -- izmed dveh možnih konstruktorjev za P ∨ Q izbere levega
    -- predpostavka H : P ∨ Q → R
    -- predpostavka H_P : P
    -- cilj: P
    assumption  -- cilj dokaže natanko predpostavka H_P

  · -- cilj: (Q → R)
    -- tu bi lahko ponovili zgornje korake ali kaj podobnega, lahko pa tudi na
    -- poljubni točki z exact podamo ekspliciten dokaz
    exact (fun H_Q ↦ H (Or.inr H_Q))
```

S taktikami smo računalnik usmerjali, da je v ozadju spisal ekspliciten dokaz. Ogledamo si ga lahko z ukazom `#print`:

```lean
#print dist_prop_s_taktikami
-- izpiše:
-- def dist_prop_s_taktikami : ∀ {P Q R : Prop}, (P ∨ Q → R) → (P → R) ∧ (Q → R) :=
-- fun {P Q R} H => ⟨fun H_P => H (Or.inl H_P), fun H_Q => H (Or.inr H_Q)⟩
```

Zdaj smo rešili dve težavi od zadnjič: (1) neomejeno rekurzijo in (2) nerodno pisanje dokazov s funkcijami. V naslednjem poglavju si bomo ogledali še, kako bi predstavili predikate in kvantifikatorje.
