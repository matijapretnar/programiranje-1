-- Izklopimo samodejno določanje tipov, ki jih lahko Lean izpelje iz konteksta.
set_option autoImplicit false

-- Uporabljamo definicije iz predavanj


-- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-- Seznami
inductive SeznamBoolov : Type where
  | prazen : SeznamBoolov
  | sestavljen : Bool -> SeznamBoolov -> SeznamBoolov

def stakniSeznam :
  SeznamBoolov ->
  SeznamBoolov ->
  SeznamBoolov
:=
  fun xs ys =>
    match xs with
    | SeznamBoolov.prazen =>
        ys
    | SeznamBoolov.sestavljen x xs' =>
        .sestavljen x (stakniSeznam xs' ys)
-- Dovoli tudi .sestavljen x (stakniSeznam xs' xs'),
-- Dovoli tudi .sestavljen x (stakniSeznam xs' (SeznamBoolov.sestavljen x xs')) ipd.


-- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-- Vektorji
inductive VektorBoolov : Nat -> Type where
  | prazen : VektorBoolov 0
  | sestavljen :
      {n : Nat} ->
      Bool ->
      VektorBoolov n ->
      VektorBoolov (Nat.succ n)

-- Različne implementacije funkcije za stikanje vektorjev
def stakniVektor {m n : Nat} :
  VektorBoolov m ->
  VektorBoolov n ->
  VektorBoolov (n + m)
:=
  fun xs ys =>
    match xs with
    | VektorBoolov.prazen =>
        ys
    | VektorBoolov.sestavljen x xs' =>
        .sestavljen x (stakniVektor xs' ys)

def stakniVektor' {m n : Nat} :
  VektorBoolov m ->
  VektorBoolov n ->
  VektorBoolov (m + n)
:=
  fun xs ys =>
    match xs with
    | VektorBoolov.prazen =>
        by
          rw [Nat.zero_add]
          exact ys
    | VektorBoolov.sestavljen x xs' =>
        by
          rw [Nat.succ_add]
          exact .sestavljen x (stakniVektor' xs' ys)

-- Primer uporabe Eq.mpr, ki spremeni tip, če imamo dokaz, da sta tipa enaka
#check Eq.mpr
def stakniVektor'' {m n : Nat} :
  VektorBoolov m ->
  VektorBoolov n ->
  VektorBoolov (m + n)
:=
  fun xs ys =>
    match xs with
    -- Manjka dokaz: ⊢ VektorBoolov (0 + n) = VektorBoolov n
    | VektorBoolov.prazen => Eq.mpr (congrArg VektorBoolov (Nat.zero_add n)) ys -- Dokaz (0+n = n) podamo kot argument dokazu enakosti dolžin vektorjev
    | VektorBoolov.sestavljen x xs' =>
        by
          rw [Nat.succ_add]
          exact .sestavljen x (stakniVektor' xs' ys)

-- Pomembno, kje delamo match (indukcijo)
def stakniVektor''' {m n : Nat} :
  VektorBoolov m ->
  VektorBoolov n ->
  VektorBoolov (m + n)
:=
  fun xs ys =>
    match ys with
    | VektorBoolov.prazen =>
        xs
    | VektorBoolov.sestavljen y ys' =>
        .sestavljen y (stakniVektor''' xs ys')


-- ----- ----- ----- ----- -----
-- Varno branje elementov vektorja
-- Definiramo tip, ki predstavlja veljavne indekse za vektor določene dolžine
inductive Finite : Nat -> Type where
  | fzero : {n : Nat} -> Finite (Nat.succ n)
  | fsucc : {n : Nat} -> Finite n -> Finite (Nat.succ n)

-- Vsi elementi v Finite 3:
def elementiFinite3 : List (Finite 3) :=
  [ Finite.fzero,
    Finite.fsucc Finite.fzero,
    Finite.fsucc (Finite.fsucc Finite.fzero)
  ]

-- Funkcija za varno branje elementov vektorja
def vpogled {n : Nat} : VektorBoolov n -> Finite n -> Bool :=
  fun xs i =>
    match xs, i with
    | VektorBoolov.sestavljen x _, Finite.fzero => x
    | VektorBoolov.sestavljen _ xs', Finite.fsucc (fi) => vpogled xs' fi

-- Vektor z dvema elementoma ([true, false])
def testniVektor : VektorBoolov 2 :=
  VektorBoolov.sestavljen true (VektorBoolov.sestavljen false VektorBoolov.prazen)
-- Indeks tipa Finite 2
def index1 : Finite 2 :=
  Finite.fsucc Finite.fzero

#eval vpogled testniVektor index1 -- Izpiše false
-- Če definiramo prazen vektor, potem `vpogled` ne more biti poklican z nobenim indeksom.
#eval vpogled VektorBoolov.prazen Finite.fzero


-- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-- Vektor z elementi poljubnega tipa A

inductive VektorPoljuben : Type ->  Nat -> Type where
  | prazen : {A : Type} -> VektorPoljuben A 0
  | sestavljen :
      {A : Type} ->
      {n : Nat} ->
      A ->
      VektorPoljuben A n ->
      VektorPoljuben A (Nat.succ n)

-- Primer vektorja z elementi tipa Nat
def vektorNaravnihStevil : VektorPoljuben Nat 3 :=
  VektorPoljuben.sestavljen 10
    (VektorPoljuben.sestavljen 20
      (VektorPoljuben.sestavljen 30
        (VektorPoljuben.prazen)))


inductive VektorPoljubenStrog : Type ->  Nat -> Type where
  | prazen : (A : Type) -> VektorPoljubenStrog A 0
  | sestavljen :
      (A : Type) ->
      {n : Nat} ->
      A ->
      VektorPoljubenStrog A n ->
      VektorPoljubenStrog A (Nat.succ n)

def VektorStrogNaravnihStevil : VektorPoljubenStrog Nat 3 :=
  VektorPoljubenStrog.sestavljen Nat 10
    (VektorPoljubenStrog.sestavljen Nat 20
      (VektorPoljubenStrog.sestavljen Nat 30
        (VektorPoljubenStrog.prazen Nat)))
