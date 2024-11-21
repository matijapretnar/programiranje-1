-- Da nam Lean preveč stvari ne naredi sam od sebe
set_option autoImplicit false

----------------------------------------------
-- običajen tip
----------------------------------------------

inductive Naravno : Type where
  | nic : Naravno
  | naslednik : Naravno → Naravno
deriving Repr

----------------------------------------------
-- običajna vrednost
----------------------------------------------

def ena : Naravno := Naravno.naslednik Naravno.nic

----------------------------------------------
-- vrednost, odvisna od vrednosti
----------------------------------------------
def plus : Naravno → Naravno → Naravno :=
  fun m n =>
    match m with
    | Naravno.nic => n
    | Naravno.naslednik m' =>
        Naravno.naslednik (plus m' n)

----------------------------------------------
-- tip, odvisen od tipov (zapisan eksplicitno)
----------------------------------------------

inductive SeznamE : Type → Type where
  | prazen : (A : Type) → SeznamE A
  | sestavljen : (A : Type) → A → SeznamE A → SeznamE A
deriving Repr

----------------------------------------------
-- vrednost, odvisna od tipov (zapisana eksplicitno)
----------------------------------------------
def stakniE : (A : Type) → SeznamE A → SeznamE A → SeznamE A :=
  fun (A : Type) (xs : SeznamE A) (ys : SeznamE A) =>
    match xs with
    | SeznamE.prazen _ => ys
    | SeznamE.sestavljen _ x xs' =>
        SeznamE.sestavljen A x (stakniE A xs' ys)

#eval (plus Naravno.nic Naravno.nic)

#eval (stakniE Nat
  (SeznamE.sestavljen Nat 1 (SeznamE.sestavljen Nat 2 (SeznamE.prazen Nat)))
  (SeznamE.sestavljen Nat 3 (SeznamE.sestavljen Nat 4 (SeznamE.prazen Nat))))

#eval (stakniE _
  (SeznamE.sestavljen _ (-1) (SeznamE.sestavljen _ 2 (SeznamE.prazen _)))
  (SeznamE.sestavljen _ 3. (SeznamE.sestavljen _ 4 (SeznamE.prazen _))))

----------------------------------------------
-- tip, odvisen od tipov (zapisan implicitno)
----------------------------------------------

inductive Seznam : Type → Type where
  | prazen : {A : Type} → Seznam A
  | sestavljen : {A : Type} → A → Seznam A → Seznam A
deriving Repr

----------------------------------------------
-- vrednost, odvisna od tipov (zapisana implicitno)
----------------------------------------------

def stakni : {A : Type} → Seznam A → Seznam A → Seznam A :=
  fun {A : Type} (xs : Seznam A) (ys : Seznam A) =>
    match xs with
    | Seznam.prazen => ys
    | Seznam.sestavljen x xs' =>
        Seznam.sestavljen x (stakni xs' ys)

#eval (plus Naravno.nic Naravno.nic)

#eval (stakni
  (Seznam.sestavljen 1 (Seznam.sestavljen 2 (Seznam.prazen)))
  (Seznam.sestavljen 3 (Seznam.sestavljen 4 (Seznam.prazen))))

-- ker smo v prazen podali {A} implicitno, zdaj ne ve, s katerim tipom ima opravka

#eval Seznam.prazen

#eval (Seznam.prazen : Seznam Nat)

----------------------------------------------
-- tip, odvisen od vrednosti (in tipov)
----------------------------------------------

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
