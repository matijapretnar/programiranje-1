# Odvisni tipi

Pri implementaciji Curry-Howardovega izomorfizma v OCamlu smo naleteli na tri ovire, ki pa smo jih v Leanu tudi že deloma rešili:

1. Z neomejeno rekurzijo lahko definiramo vrednosti poljubnega tipa, kar pomeni, da lahko dokažemo poljubno izjavo. Lean to reši tako, da nas omeji le na funkcije, za katere imamo dokaz o ustavitvi.
2. Pisanje dokazov neposredno s konstrukcijami vrednosti je nepraktično in ni v duhu dokazov, ki smo jih sicer navajeni. Lean to reši z uvedbo taktik, s katerimi ga usmerjamo pri konstrukciji dokazov.
3. S konstruktorji tipov lahko izrazimo le logične veznike, ne moremo pa izraziti kvantifikatorjev, kar pomeni, da smo v dokazih omejeni le na izjavni račun. Poglejmo si, kako v Leanu to rešimo z uvedbo _odvisnih tipov_.

Spomnimo se, da v Curry-Howardovem izomorfizmu vsaki izjavi ustreza tip, ki ima predstavnika natanko tedaj, kadar izjavo lahko dokažemo. Če želimo v izjavah uporabiti kvantifikatorje, moramo najprej govoriti o predikatih, torej izjavah, odvisnih od vrednosti. Naj bo $S(n)$ izjava, da je naravno število $n$ sodo. Tako je izjava $S(42)$ resnična, izjava $S(43)$ pa ne. Tip, ki bi ustrezal izjavi $S(42)$ bi tako moral biti neprazen, tip $S(43)$ pa prazen. Torej dobimo tip, ki se spreminja glede na vrednost števila $n$. Do sedaj smo spoznali že:

- **Vrednosti, ki se spreminjajo glede na vrednosti**: to so običajne funkcije kot je `podvoji : Nat → Nat` ali `zacetnaVrednost : (Float → Float) → Float`.
- **Tipe, ki se spreminjajo glede na tipe**: to so konstruktorji tipov, na primer `List`, ki iz tipa naravnih števil `Nat` ustvari tip seznamov naravnih števil `List Nat`. V OCamlu so bile take funkcije implicitne v parametričnih tipih, v Leanu pa imamo eksplicitno zapisano, da je `List` funkcija tipa `Type → Type`. Imamo tudi funkcije z več argumenti, na primer `Prod : Type → Type → Type`, ki iz tipov `A` in `B` naredi njun produkt `A × B`.
- **Vrednosti, ki se spreminjajo glede na tipe**: to morda ni bilo tako očitno, ampak v tej vlogi nastopajo polimorfne funkcije. Zadnjič smo videli, da moramo parametrično polimorfnim funkcijam v definicijah in klicih tipe podati kot eksplicitne argumente, na primer `polimorfni_stakni Int [1, 2, 3] [4, 5, 6]`. Lahko bi sicer rekli, da polimorfne funkcije niso zares odvisne od tipov, saj se pri vsakem tipu obnašajo enako, ampak formalno gledano se funkcije vseeno spreminjajo, saj `polimorfni_stakni Int` sprejme dva seznama celih števil, `polimorfni_stakni Int` pa ne. Odvisnost od tipov je še bolj očitna pri ad-hoc polimorfizmu. V Pythonu na primer imamo en sam `+`, ki deluje na različne načine: števila sešteva (pri čemer se `6 * 7` izračuna na popolnoma drug način kot `6.0 * 7.0`), nize in sezname stika, … V OCamlu imamo zaradi strogih tipov za vsako izmed teh različnih operacij svoj simbol: `+` za cela števila, `+.` za števila s plavajočo vejico, `^` za nize, `@` za sezname, … Lean podpira t.i. _razrede tipov_, ki nam omogočajo, da z enim samim simbolom varno in učinkovito predstavimo operacije pri različnih tipih. To je sicer tema za drugič, ampak je pa definitivno primer vrednosti, ki je dejansko odvisna od tipa.

Spoznajmo še **tipe, ki se spreminjajo glede na vrednosti**.

## Vektorji

Za začetek si tak tip definirajmo sami. Poznamo že tip seznamov, ki ga lahko v Leanu zapišemo kot:

```lean
inductive Seznam : Type where
  | prazen : Seznam
  | sestavljen : Int → Seznam → Seznam
```

ali kot

```lean
inductive Seznam : Type → Type where
  | prazen {A : Type} : Seznam A
  | sestavljen {A : Type} : A → Seznam A → Seznam A
```

če želimo parametrični polimorfizem.

Definirajmo še tip _vektorjev_, kar je v računalništvo ime za sezname z dano dolžino. Če je `Seznam` tip seznamov poljubne dolžine, potem bo `Vektor 42` tip seznamov, ki vsebujejo natanko 42 celih števil, `Vektor 0` pa tip, ki vsebuje le prazen seznam. V Leanu tip definiramo kot:

```lean
inductive Vektor : Nat → Type where
  | prazen : Vektor Nat.zero
  | sestavljen {n : Nat} : Int → Vektor n → Vektor (Nat.succ n)
```

Prazen vektor je torej tipa `Vektor 0`, sestavljen vektor pa vsebuje glavo (tipa `Int`) in rep (dolžine `n`), sam pa je dolžine `n + 1`. Tako kot smo stikali sezname z

```lean
def stakniSeznam : Seznam → Seznam → Seznam :=
  fun xs ys =>
    match xs with
    | Seznam.prazen => ys
    | Seznam.sestavljen x xs' =>
        Seznam.sestavljen x (stakniSeznam xs' xs')
```

lahko stikamo tudi vektorje:

```lean
def stakniVektor {m n : Nat} : Vektor m → Vektor n → Vektor (n + m) :=
  fun xs ys =>
    match xs with
    | Vektor.prazen => ys
    | Vektor.sestavljen x xs' =>
        Vektor.sestavljen x (stakniVektor xs' ys)
```

pri čemer bo Lean popazil, da se bodo dolžine dejansko ujemale. Če bi se na primer pri funkciji `stakniSeznam` zatipkali in zapisali `Seznam.prazen => xs`, se Lean ne bi pritožil (oz. bi se, da spremenljivke `ys` nismo uporabili, vendar bi definicijo vseeno sprejel). Če pa bi zapisali `Vektor.prazen => xs`, pa bi dobili napako

```plaintext
Type mismatch
  xs
has type
  Vektor m
but is expected to have type
  Vektor (n + 0)
```

Opazimo lahko, da smo tip funkcije podali kot `Vektor m → Vektor n → Vektor (n + m)` in ne kot `Vektor m → Vektor n → Vektor (m + n)`, kar bi bilo bolj smiselno. To je posledica tega, da je seštevanje naravnih števil v Leanu definirano rekurzivno po drugem argumentu, torej kot `m + Nat.succ n = Nat.succ (m + n)`, stikanje vektorjev pa po prvem. Če torej vzamemo sestavljen vektor `xs : Vektor (Nat.succ m)` in poljuben vektor `ys : Vektor n`, za rep velja `xs' : Vektor m`. Torej bo rekurzivni klic `stakniVektor xs' ys` tipa `Vektor (n + m)`. Če temu dodamo še glavo `x`, dobimo rezultat tipa `Vektor (Nat.succ (n + m))`, kar je po definiciji seštevanja enako kot `Vektor (n + Nat.succ m)`, kar smo tudi pričakovali pri argumentih tipa `Vektor (Nat.succ m)` in `Vektor n`.

Seveda lahko definiramo tudi funkcijo `Vektor m → Vektor n → Vektor (m + n)`, vendar se tu Lean pritoži že pri primeru stikanja s praznim seznamom. Tam za rezultat pričakuje `Vektor (0 + n)`, mi pa smo mu dali `ys : Vektor n`. Po definicij namreč velja samo `n + 0 = n`, ne pa tudi `0 + n = n`. To moramo dokazati ločeno! V tem primeru lahko vstopimo v taktični način, ker za cilj dobimo `Vektor (0 + n)`. Nato uporabimo taktiko `rw`, ki ji kot argument podamo že dokazano trditev `zero_add : ∀ (n : Nat), 0 + n = n`, s pomočjo katere cilj zamenja v `Vektor n`, ki mu lahko zadostimo z `ys`. Podobno s trditvijo `succ_add : ∀ (n m : Nat), (succ n) + m = succ (n + m)` prepričamo Lean tudi v drugem primeru:

```lean
def stakniVektor' : Vektor m -> Vektor n -> Vektor (m + n) :=
  fun xs ys =>
    match xs with
    | Vektor.prazen => by
        rw [Nat.zero_add]
        exact ys
    | Vektor.sestavljen x xs' => by
        rw [Nat.succ_add]
        exact .sestavljen x (stakniVektor' xs' ys)
```

## Predikatni račun

S pomočjo odvisnih tipov lahko sedaj definiramo tudi zgoraj omenjeni tip, ki je pri sodih številih neprazen, pri lihih pa je prazen:

```lean
inductive NeprazenPriSodih : Nat → Type where
  | nicJeSodo : NeprazenPriSodih Nat.zero
  | sodoPlusDvaJeSodo {n : Nat} : NeprazenPriSodih n → NeprazenPriSodih (Nat.succ (Nat.succ n))
```

Gre za manjšo prilagoditev tipa `Vektor`. Tu imamo en element pri tipu `NeprazenPriSodih 0`, ter po en element pri tipu `NeprazenPriSodih (n + 2)`, če imamo "rep" tipa `NeprazenPriSodih n`. Hitro lahko vidimo, da ima tip `NeprazenPriSodih n` predstavnika natanko takrat, kadar je `n` sod. Seveda je še bolje, če namesto `Type` uporabimo `Prop`:

```lean
inductive Sodo : Nat → Prop where
  | nicJeSodo : Sodo Nat.zero
  | sodoPlusDvaJeSodo {n : Nat} : Sodo n → Sodo (Nat.succ (Nat.succ n))
```

Za vajo lahko dokažete `vsota_sodih {m n : Nat} : Sodo m → Sodo n → Sodo (m + n)`. Mi pa si za primer dokaza v predikatnem računu oglejmo paradoks brivca: brivec, ki bi bril vse, ki se ne brijejo sami, ne obstaja. Predpostavimo, da imamo tip ljudi `C` ter predikat `B : C → C → Prop`, ki pove, ali prva oseba brije drugo.

```lean
variable (C : Type)
variable (B : C → C → Prop)

theorem paradoks_brivca : ¬∃ (b : C), ∀ (c : C), B c c ↔ ¬B b c := by
  -- cilj: ¬∃ b, ∀ (c : C), B c c ↔ ¬B b c
  intro H
  -- predpostavka H: ∃ b, ∀ (c : C), B c c ↔ ¬B b c
  -- cilj: False
  cases H -- razstavimo eksistenčni kvantifikator
  rename_i bob koga_brije_bob -- eksplicitno poimenujemo brivca in njegovo lastnost
  -- predpostavka koga_brije_bob: ∀ (c : C), B c c ↔ ¬B bob c
  have kako_se_brije_bob := koga_brije_bob bob  -- poglejmo, ali Bob brije samega sebe
  -- predpostavka kako_se_brije_bob : B bob bob ↔ ¬B bob bob
  -- pokažimo, da se B ne brije sam
  have bob_se_ne_brije : ¬B bob bob := by
    -- cilj vmesne leme: ¬B bob bob
    intro bob_se_brije  -- pokažimo negacijo
    -- predpostavka bob_se_brije: B bob bob
    -- cilj: False
    have bob_se_ne_brije := kako_se_brije_bob.mp bob_se_brije  -- uporabimo Bobovo lastnost
    -- predpostavka bob_se_ne_brije: ¬B bob bob
    contradiction  -- protislovje
  -- s have dobimo:
  -- predpostavka bob_se_ne_brije : ¬B bob bob
  -- cilj ostaja: False
  have bob_se_brije := kako_se_brije_bob.mpr bob_se_ne_brije  -- spet uporabimo Bobovo lastnost
  contradiction  -- protislovje
```
