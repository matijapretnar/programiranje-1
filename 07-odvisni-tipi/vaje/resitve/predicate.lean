variable (α : Type) (p q : α → Prop) (r : Prop)
variable (r : Prop)

example : (¬ ∃ x, p x) ↔ (∀ x, ¬ p x) :=
  by
    apply Iff.intro
    intro h x hp
    apply h
    exact ⟨x, hp⟩
    intro h hp
    let ⟨x, hp⟩ := hp
    exact h x hp

example : (r → ∀ x, p x) ↔ (∀ x, r → p x) :=
  by
    apply Iff.intro
    intro h x r
    exact h r x
    intro h r x
    apply h x r

example : r ∧ (∃ x, p x) ↔ (∃ x, r ∧ p x) := by
  apply Iff.intro
  intro ⟨ hr, ⟨ x, hp ⟩ ⟩
  apply Exists.intro x ⟨ hr, hp ⟩
  intro ⟨ x, ⟨ hr, hp ⟩ ⟩
  constructor
  exact hr
  exists x

example : r ∨ (∀ x, p x) → (∀ x, r ∨ p x) :=
  by
    intro h x
    cases h
    case inl hr => exact Or.inl hr
    case inr hpx => exact Or.inr (hpx x)

example : (¬ ∀ x, p x) ↔ (∃ x, ¬ p x) :=
  by
    apply Iff.intro
    intro h
    apply Classical.byContradiction
    intro npx
    apply h
    intro x
    apply Classical.byContradiction
    intro px
    apply npx
    exact ⟨x, px⟩
    intro h npx
    let ⟨x, npx'⟩ := h
    apply npx' (npx x)

example : r ∨ (∀ x, p x) ↔ (∀ x, r ∨ p x) :=
  by
    apply Iff.intro
    intro h x
    cases h
    case inl hr => exact Or.inl hr
    case inr hpx => exact Or.inr (hpx x)
    intro h
    -- Can't do cases h
    have x := Classical.em r
    cases x
    case inl hr => exact Or.inl hr
    case inr nhr =>
      right
      intro x
      have xx := h x
      cases xx
      case inl hr => contradiction
      case inr hpx => exact hpx
