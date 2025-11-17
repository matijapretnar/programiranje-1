variable (α : Type) (p q : α → Prop) (r : Prop)
variable (r : Prop)

-- Dvojne rešitve, da je prikazanih več načinov reševanja.
-- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----

theorem eq1 : (¬ ∃ x, p x) ↔ (∀ x, ¬ p x) :=
  by
    apply Iff.intro
    · intro h x hp
      apply h
      exact ⟨x, hp⟩
    · intro h hp
      let ⟨x, hp⟩ := hp
      exact h x hp

-- Več možnosti
theorem eq1' : (¬ ∃ x, p x) ↔ (∀ x, ¬ p x) :=
  by
    apply Iff.intro
    · intro notExistsPx
      intro x
      intro px
      apply notExistsPx
      -- exact ⟨ x, px ⟩ -- druga rešitev
      apply Exists.intro x
      exact px
    · intro forallNotPx existsPx
      obtain ⟨x, px⟩ := existsPx
      -- apply (forallNotPx x) -- druga rešitev
      -- exact px
      specialize forallNotPx x
      contradiction

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
theorem eq2 : (r → ∀ x, p x) ↔ (∀ x, r → p x) :=
  by
    apply Iff.intro
    · intro h x r
      exact h r x
    · intro h r x
      apply h x r

-- Več korakov
theorem eq2' : (r → ∀ x, p x) ↔ (∀ x, r → p x) :=
  by
    apply Iff.intro
    · intro rToForAllpx x r
      have forAllpx := rToForAllpx r
      specialize forAllpx x
      exact forAllpx
    · intro forAllrToPx r x
      specialize forAllrToPx x
      exact forAllrToPx r

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
theorem eq3 : r ∧ (∃ x, p x) ↔ (∃ x, r ∧ p x) := by
  apply Iff.intro
  · intro ⟨ hr, ⟨ x, hp ⟩ ⟩
    apply Exists.intro x ⟨ hr, hp ⟩
  · intro ⟨ x, ⟨ hr, hp ⟩ ⟩
    constructor
    exact hr
    exists x

-- Podobno
theorem eq3' : r ∧ (∃ x, p x) ↔ (∃ x, r ∧ p x) :=
  by
    apply Iff.intro
    · intro rAndExistsPx
      obtain ⟨ r, existsPx ⟩ := rAndExistsPx
      obtain ⟨ x, px ⟩ := existsPx
      exact ⟨ x, ⟨ r, px ⟩ ⟩
    · intro existsrAndPx
      obtain ⟨ x, rAndPx ⟩ := existsrAndPx
      exact ⟨ rAndPx.left, ⟨ x, rAndPx.right ⟩ ⟩

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
theorem eq4 : r ∨ (∀ x, p x) → (∀ x, r ∨ p x) :=
  by
    intro h x
    cases h -- druga sintaksa za `cases`
    case inl hr => exact Or.inl hr
    case inr hpx => exact Or.inr (hpx x)

-- Samo drugačna sintaksa
theorem eq4' : r ∨ (∀ x, p x) → (∀ x, r ∨ p x) :=
  by
    intro rOrForallPx x
    cases rOrForallPx with
    | inl r => exact Or.inl r
    | inr forAllPx =>
      exact Or.inr (forAllPx x)

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
theorem eq5 : (¬ ∀ x, p x) ↔ (∃ x, ¬ p x) :=
  by
    apply Iff.intro
    · intro h
      -- dokazujemo negacijo z uporabo `byContradiction`
      apply Classical.byContradiction
      intro npx
      apply h
      intro x
      apply Classical.byContradiction
      intro px
      apply npx
      exact ⟨x, px⟩
    · intro h npx
      let ⟨x, npx'⟩ := h
      apply npx' (npx x)

-- V bistvu samo druga imena spremenljivk
theorem eq5' : (¬ ∀ x, p x) ↔ (∃ x, ¬ p x) :=
  by
    apply Iff.intro
    · intro notForallpx
      apply Classical.byContradiction
      intro notExistsNotPx
      apply notForallpx
      intro x
      apply Classical.byContradiction
      intro notPx
      apply notExistsNotPx
      exact ⟨ x, notPx ⟩
    · intro existsNotPx forallPx
      obtain ⟨ x, notPx ⟩ := existsNotPx
      apply notPx
      exact forallPx x

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
theorem eq6 : r ∨ (∀ x, p x) ↔ (∀ x, r ∨ p x) :=
  by
    apply Iff.intro
    · intro h x
      cases h with
      | inl hr => exact Or.inl hr
      | inr hpx => exact Or.inr (hpx x)
    · intro h
      -- Ne moremo deliti `cases h`, ker je v predikatu
      have x := Classical.em r
      cases x with
      | inl hr => exact Or.inl hr
      | inr nhr =>
        right
        intro x
        have xx := h x
        cases xx with
        | inl hr => contradiction
        | inr hpx => exact hpx

-- Podobna rešitev
theorem eq6' : r ∨ (∀ x, p x) ↔ (∀ x, r ∨ p x) :=
  by
    apply Iff.intro
    · intro rOrForallpx x
      cases rOrForallpx with
      | inl hr =>
        exact Or.inl hr
      | inr forallPx =>
        exact Or.inr (forallPx x)
    · intro forallrOrPx
      have emR := Classical.em r
      cases emR with
      | inl r => exact Or.inl r
      | inr notr =>
        apply Or.inr
        intro x
        specialize forallrOrPx x
        cases forallrOrPx with
        | inl r => contradiction
        | inr px => exact px
