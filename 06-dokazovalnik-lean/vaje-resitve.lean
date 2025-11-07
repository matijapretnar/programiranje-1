-- LOGIKA

-- (A ∧ B) ↔ (B ∧ A)

theorem eq1 {A B : Prop} : (A ∧ B) ↔ (B ∧ A) := by
  apply Iff.intro
  . intro h
    apply And.intro
    . exact h.right
    . exact h.left
  . intro h
    apply And.intro
    . exact h.right
    . exact h.left

theorem eq1' {A B : Prop} : (A ∧ B) ↔ (B ∧ A) :=
  Iff.intro
    (fun h => And.intro h.right h.left)
    (fun h => And.intro h.right h.left)

theorem eq1'' {A B : Prop} : (A ∧ B) ↔ (B ∧ A) :=
  Iff.intro
    (fun h => ⟨h.2, h.1⟩)
    (fun h => ⟨h.2, h.1⟩)

-- (A ∨ B) ↔ (B ∨ A)

theorem eq2 {A B : Prop} : (A ∨ B) ↔ (B ∨ A) := by
  apply Iff.intro
  . intro h
    cases h with
    | inl ha =>
      apply Or.inr
      exact ha
    | inr hb => -- bolj neposredno
      exact Or.inl hb
  . intro h
    cases h with
    | inl ha => exact Or.inr ha
    | inr hb => exact Or.inl hb

theorem eq2' {A B : Prop} : (A ∨ B) ↔ (B ∨ A) := by
  apply Iff.intro
  . intro h
    apply Or.elim h
    . exact Or.inr
    . exact Or.inl
  . intro h
    apply Or.elim h
    . exact Or.inr
    . exact Or.inl

theorem eq2'' {A B : Prop} : (A ∨ B) ↔ (B ∨ A) :=
  Iff.intro
    (fun h => h.elim Or.inr Or.inl)
    (fun h => h.elim Or.inr Or.inl)

-- (A ∧ (B ∧ C)) ↔ (B ∧ (A ∧ C))

theorem eq3 {A B C : Prop} : (A ∧ (B ∧ C)) ↔ (B ∧ (A ∧ C)) := by
  apply Iff.intro
  . intro h
    apply And.intro
    . exact h.right.left
    . apply And.intro
      . exact h.left
      . exact h.right.right
  . intro h
    apply And.intro
    . exact h.right.left
    . apply And.intro
      . exact h.left
      . exact h.right.right

theorem eq3' {A B C : Prop} : (A ∧ (B ∧ C)) ↔ (B ∧ (A ∧ C)) :=
  Iff.intro
    (fun h => ⟨h.2.1, ⟨h.1, h.2.2⟩⟩)
    (fun h => ⟨h.2.1, ⟨h.1, h.2.2⟩⟩)

-- (A ∨ (B ∨ C)) ↔ (B ∨ (A ∨ C))

theorem eq4 {A B C : Prop} : (A ∨ (B ∨ C)) ↔ (B ∨ (A ∨ C)) := by
    apply Iff.intro
    intro h
    cases h
    case inl a => exact Or.inr (Or.inl a)
    case inr bc =>
      cases bc
      case inl b => exact Or.inl b
      case inr c => exact Or.inr (Or.inr c)
    intro h
    cases h
    case inl b => exact Or.inr (Or.inl b)
    case inr ac =>
      cases ac
      case inl a => exact Or.inl a
      case inr c => exact Or.inr (Or.inr c)

theorem eq5 {A B C : Prop} : A ∧ (B ∨ C) ↔ (A ∧ B) ∨ (A ∧ C) :=
  Iff.intro
    (fun h => match h with
      | ⟨ha, Or.inl hb⟩ => Or.inl ⟨ha, hb⟩
      | ⟨ha, Or.inr hc⟩ => Or.inr ⟨ha, hc⟩)
    (fun h => match h with
      | Or.inl ⟨ha, hb⟩ => ⟨ha, Or.inl hb⟩
      | Or.inr ⟨ha, hc⟩ => ⟨ha, Or.inr hc⟩)

theorem eq6 {A B C : Prop} : (B ∨ C) → A ↔ (B → A) ∧ (C → A) := by
  apply Iff.intro
  . intro h
    constructor
    . intro hb
      apply h
      left
      assumption
    . intro hc
      apply h
      right
      assumption
  . intro h hbc
    cases hbc with
    | inl hb => exact h.left hb
    | inr hc => exact h.right hc

-- Takole pa izgleda brez pik.
theorem eq7 {A B C : Prop} : C → (A ∧ B) ↔ (C → A) ∧ (C → B) :=
  by
    apply Iff.intro
    intro h
    constructor
    intro c
    exact (h c).1
    intro c
    exact (h c).2
    intro h c
    constructor
    exact h.1 c
    exact h.2 c
