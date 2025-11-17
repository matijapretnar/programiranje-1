-- Izomorfizmi

-- Namesto: type ('a, 'b) sum = In1 of 'a | In2 of 'b uporabimo že definiran tip
-- A ⊕ B , ki je definiran kot
/-
inductive Sum (α : Type u) (β : Type v) where
  /-- Left injection into the sum type `α ⊕ β`. If `a : α` then `.inl a : α ⊕ β`. -/
  | inl (val : α) : Sum α β
  /-- Right injection into the sum type `α ⊕ β`. If `b : β` then `.inr b : α ⊕ β`. -/
  | inr (val : β) : Sum α β
-/
-- Simbol ⊕ dobimo s tabulacijo \oplus
universe u
structure isomorphic (A B : Type u) where
  (phi : A → B)
  (psi : B → A)
  (left_inv : ∀ x : A, psi (phi x) = x)
  (right_inv : ∀ y : B, phi (psi y) = y)

-- Definirajte funkcije, ki realizirajo izomorfizme med tipi in pripadajoče dokaze

-- A × B ≅ B × A


def phi1 {A B : Type} : (A × B) → (B × A) :=
  fun x =>
    match x with
    | (a, b) => (b, a)

def psi1 {A B : Type} : (B × A) → (A × B) :=
  phi1

example : isomorphic (A × B) (B × A) :=
  {
    phi := phi1,
    psi := psi1,
    left_inv := by
      intro x
      cases x
      rfl
      ,
    right_inv := by
      intro x
      cases x
      rfl
  }

-- A ⊕ B ≅ B ⊕ A

def phi2 {A B : Type} : (A ⊕ B) → (B ⊕ A) :=
  fun x =>
    match x with
    | Sum.inl a => Sum.inr a
    | Sum.inr b => Sum.inl b

def psi2 {A B : Type} : (B ⊕ A) → (A ⊕ B) :=
  phi2

example : isomorphic (A ⊕ B) (B ⊕ A) :=
  {
    phi := phi2,
    psi := psi2,
    left_inv := by
      intro x
      cases x
      rfl
      rfl
      ,
    right_inv := by
      intro x
      cases x
      rfl
      rfl
  }

-- (A × B) × C ≅ A × (B × C)

def phi3 {A B C : Type} : ((A × B) × C) → (A × (B × C)) :=
  fun x =>
    match x with
    | ((a, b), c) => (a, (b, c))

def psi3 {A B C : Type} : (A × (B × C)) → ((A × B) × C) :=
  fun x =>
    match x with
    | (a, (b, c)) => ((a, b), c)


example : isomorphic ((A × B) × C) (A × (B × C)) :=
  {
    phi := phi3,
    psi := psi3,
    left_inv := by
      intro x
      cases x
      rfl
      ,
    right_inv := by
      intro x
      cases x
      rfl
  }

-- (A ⊕ B) ⊕ C ≅ A ⊕ (B ⊕ C)

def phi4 {A B C : Type} : (A ⊕ (B ⊕ C)) → ((A ⊕ B) ⊕ C) :=
  fun x =>
    match x with
    | Sum.inl a => Sum.inl (Sum.inl a)
    | Sum.inr (Sum.inl b) => Sum.inl (Sum.inr b)
    | Sum.inr (Sum.inr c) => Sum.inr c

def psi4 {A B C : Type} : ((A ⊕ B) ⊕ C) → (A ⊕ (B ⊕ C)) :=
  fun x =>
    match x with
    | Sum.inl (Sum.inl a) => Sum.inl a
    | Sum.inl (Sum.inr b) => Sum.inr (Sum.inl b)
    | Sum.inr c => Sum.inr (Sum.inr c)


example : isomorphic (A ⊕ (B ⊕ C)) ((A ⊕ B) ⊕ C) :=
  {
    phi := phi4,
    psi := psi4,
    left_inv := by
      intro abc
      cases abc
      case inl a => rfl
      case inr bc =>
        cases bc
        rfl
        rfl
      ,
    right_inv := by
      intro abc
      cases abc
      case inl ab =>
        cases ab
        rfl
        rfl
      case inr c => rfl
  }

-- A × (B ⊕ C) ≅ (A × B) ⊕ (A × C)

def phi5 {A B C : Type} : (A × (B ⊕ C)) → ((A × B) ⊕ (A × C)) :=
  fun x =>
    match x with
    | (a, Sum.inl b) => Sum.inl (a, b)
    | (a, Sum.inr c) => Sum.inr (a, c)

def psi5 {A B C : Type} : ((A × B) ⊕ (A × C)) → (A × (B ⊕ C)) :=
  fun x =>
    match x with
    | Sum.inl (a, b) => (a, Sum.inl b)
    | Sum.inr (a, c) => (a, Sum.inr c)

example : isomorphic (A × (B ⊕ C)) ((A × B) ⊕ (A × C)) :=
  {
    phi := phi5,
    psi := psi5,
    left_inv := by
      intro (a, bc)
      cases bc
      case inl b => rfl
      case inr c => rfl
      ,
    right_inv := by
      intro x
      cases x
      rfl
      rfl
  }

-- (B ⊕ C) → A ≅ (B → A) × (C → A)
-- V pomoč nam bo `funext`


def phi6 {A B C : Type u} : ((B ⊕ C) → A) → ((B → A) × (C → A)) :=
  fun f =>
    ((fun b => f (Sum.inl b)), fun c => f (Sum.inr c))

def psi6 {A B C : Type u} : ((B → A) × (C → A)) → ((B ⊕ C) → A) :=
  fun f x =>
    match x with
    | Sum.inl b => (f.1 b)
    | Sum.inr c => (f.2 c)

example {A B C : Type u} : isomorphic ((B ⊕ C) → A) ((B → A) × (C → A)) :=
  {
    phi := phi6,
    psi := psi6,
    left_inv := by
      intro f
      apply funext
      intro x
      cases x
      rfl
      rfl
      ,
    right_inv := by
     intro(ba, ca)
     rfl
  }

-- C → (A × B) ≅ (C → A) × (C → B)

def phi7 {A B C : Type u} : (C → (A × B)) → ((C → A) × (C → B)) :=
  fun f =>
    ((fun c => (f c).1), fun c => (f c).2)

def psi7 {A B C : Type u} : ((C → A) × (C → B)) → (C → (A × B)) :=
  fun f c => (f.1 c, f.2 c)

example {A B C : Type u} : isomorphic (C → (A × B)) ((C → A) × (C → B)) :=
  {
    phi := phi7,
    psi := psi7,
    left_inv := by
      intro f
      apply funext
      intro c
      rfl
    ,
    right_inv := by
      intro f
      rfl
  }
