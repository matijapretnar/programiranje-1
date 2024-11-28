-- Predavanja

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

-- Vaje

inductive Finite : Naravno -> Type where
  | fzero : {n : Naravno} -> Finite (Naravno.naslednik n)
  | fsucc : {n : Naravno} -> Finite n -> Finite (Naravno.naslednik n)

-- Sedaj lahko definiramo `lookup`, ki ne bo nikoli povzročil napake.

def lookup {A : Type} {n : Naravno} : Vektor A n -> Finite n -> A :=
  fun xs i =>
    match xs, i with
    | Vektor.sestavljen x _, Finite.fzero => x
    | Vektor.sestavljen _ xs', Finite.fsucc i' => lookup xs' i'

-- Poučno je pogledati tipe tule:
-- #eval lookup Vektor.prazen Finite.fzero

-- Včasih enakost tipov ni takoj očitna in jo moramo izpeljati
-- Dopolnite naslednjo definicijo, vse potrebne leme pa dokažite kar s taktiko `sorry`.

def stakni_vektorja' : {A : Type} → {m n : Naravno} → Vektor A m → Vektor A n → Vektor A (plus n m) :=
  by
    intro A m n xs ys
    cases xs with
    | prazen =>
      have plus_nic : plus n Naravno.nic = n := sorry
      rw [plus_nic]
      exact ys
    | sestavljen x xs' =>
      have v := Vektor.sestavljen x (stakni_vektorja' xs' ys)
      have add_succ {m n : Naravno} : plus m (Naravno.naslednik n) = Naravno.naslednik (plus m n) := sorry
      rw [add_succ]
      exact v

def stakni_vektorja'' : {A : Type} → {m n : Naravno} → Vektor A m → Vektor A n → Vektor A (plus m n) :=
  by
    intro A m n xs ys
    have v := stakni_vektorja' xs ys
    have add_comm {m n : Naravno} : plus m n = plus n m := sorry
    rw [add_comm]
    exact v

-- Na vajah nismo uporabili `intro` in `cases`, ampak smo definirali funkcijo z `fun`.

-- V tej definiciji je (plus n m) namesto (plus m n) kot v zgornji
def stakni_vektorja'_fun : {A : Type} → {m n : Naravno} → Vektor A m → Vektor A n → Vektor A (plus n m) :=
  fun {A : Type} {m n : Naravno} (xs : Vektor A m) (ys : Vektor A n) =>
    match xs with
    | Vektor.prazen => by
      have plus_nic : plus n Naravno.nic = n := sorry
      rw [plus_nic]
      exact ys
    | Vektor.sestavljen x xs' => by
      have v := Vektor.sestavljen x (stakni_vektorja' xs' ys)
      have add_succ {m n : Naravno} : plus m (Naravno.naslednik n) = Naravno.naslednik (plus m n) := sorry
      rw [add_succ]
      exact v

-- V tej definiciji pa je spet (plus m n), ampak jo naredimo s pomočjo prejšnje
-- Uporabite samo zgornjo definicijo `stakni_vektorja'` in taktike `rw` in `exact`.
def stakni_vektorja''_fun : {A : Type} → {m n : Naravno} → Vektor A m → Vektor A n → Vektor A (plus m n) :=
  fun {A : Type} {m n : Naravno} (xs : Vektor A m) (ys : Vektor A n) => by
    have v := stakni_vektorja' xs ys
    have add_comm {m n : Naravno} : plus m n = plus n m := sorry
    rw [add_comm]
    exact v
