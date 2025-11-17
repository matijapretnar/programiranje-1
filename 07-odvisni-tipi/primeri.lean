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
        .sestavljen x (stakniSeznam xs' xs')

inductive VektorBoolov : Nat -> Type where
  | prazen : VektorBoolov 0
  | sestavljen :
      {n : Nat} ->
      Bool ->
      VektorBoolov n ->
      VektorBoolov (Nat.succ n)

def stakniVektor :
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

def stakniVektor' :
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

def stakniVektor'' :
  VektorBoolov m ->
  VektorBoolov n ->
  VektorBoolov (m + n)
:=
  fun xs ys =>
    match xs with
    | VektorBoolov.prazen => Eq.mpr sorry ys
    | VektorBoolov.sestavljen x xs' =>
        by
          rw [Nat.succ_add]
          exact .sestavljen x (stakniVektor' xs' ys)

#print stakniVektor
#print stakniVektor'
#check Eq.mpr

def dist (A B C : Type) (f : Sum A B → C) : (A → C) × (B → C) :=
  ⟨
    (fun (x : A) => f (.inl x)),
    (fun (y : B) => f (.inr y))
  ⟩

def vsak_in (X : Type) (P Q : X -> Type) :
  ((x : X) -> P x × Q x)
  ->
  ((x : X) -> P x) × ((x : X) -> Q x)
  :=
    fun f =>
      ((fun x => (f x).fst), (fun x => (f x).snd))

def vsak_in_prop (X : Prop) (P Q : X -> Prop) :
  (∀ (x : X), P x ∧  Q x)
  ->
  (∀ (x : X), P x) ∧  (∀ (x : X), Q x)
  :=
    by
      sorry
