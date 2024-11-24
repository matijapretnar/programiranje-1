-- Vzamemo stvari iz predavanj
set_option autoImplicit false

inductive Naravno : Type where
  | nic : Naravno
  | naslednik : Naravno → Naravno
deriving Repr


def plus : Naravno → Naravno → Naravno :=
  fun m n =>
    match m with
    | Naravno.nic => n
    | Naravno.naslednik m' =>
        Naravno.naslednik (plus m' n)

-- Vektorji

inductive Vektor : Type → Naravno → Type where
  | prazen : {A : Type} → Vektor A Naravno.nic
  | sestavljen : {A : Type} → {n : Naravno} → A → Vektor A n → Vektor A (Naravno.naslednik n)
deriving Repr

#check (Vektor.sestavljen "a" (Vektor.sestavljen "b" (Vektor.prazen)))

def stakni_vektorja : {A : Type} → {m n : Naravno} → Vektor A m → Vektor A n → Vektor A (plus m n) :=
  fun {A : Type} {m n : Naravno} (xs : Vektor A m) (ys : Vektor A n) =>
    match xs with
    | Vektor.prazen => ys
    | Vektor.sestavljen x xs' => Vektor.sestavljen x (stakni_vektorja xs' ys)


-- Sedaj lahko definiramo `lookup`, ki ne bo nikoli povzročil napake.
inductive Finite : Naravno -> Type where
  | fzero : {n : Naravno} -> Finite (Naravno.naslednik n)
  | fsucc : {n : Naravno} -> Finite n -> Finite (Naravno.naslednik n)


def lookup {A : Type} {n : Naravno} : Vektor A n -> Finite n -> A :=
  sorry


-- Včasih enakost tipov ni takoj očitna in jo moramo izpeljati
-- Dopolnite naslednjo definicijo, vse potrebne leme pa dokažite kar s taktiko `sorry`.

def stakni_vektorja' : {A : Type} → {m n : Naravno} → Vektor A m → Vektor A n → Vektor A (plus n m) :=
  sorry

-- Uporabite samo definicijo `stakni_vektorja'` in taktike `rw` in `exact`.
def stakni_vektorja'' : {A : Type} → {m n : Naravno} → Vektor A m → Vektor A n → Vektor A (plus m n) :=
  sorry
