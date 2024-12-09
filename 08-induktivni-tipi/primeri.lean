set_option autoImplicit false

theorem brivec  :
  (C : Type) → (B : C → C → Prop) →
  ¬ (∃ (b : C), ∀ (c : C), B b c ↔ ¬ B c c) :=
  by
    intros C B
    intro H_obstaja_brivec
    apply Exists.elim H_obstaja_brivec
    intros brane H_koga_brije_brane
    have H_brane_se_ne_brije : ¬ B brane brane :=
      by
        intro H_brane_se_brije
        have H_kako_brane_brije_braneta := H_koga_brije_brane brane
        have H_brane_se_ne_brije := H_kako_brane_brije_braneta.mp H_brane_se_brije
        exact (H_brane_se_ne_brije H_brane_se_brije)
    have H_brane_se_brije :=
      (H_koga_brije_brane brane).mpr H_brane_se_ne_brije
    contradiction

inductive SeznamNaravnih : Type where
| prazen : SeznamNaravnih
| sestavljen : Nat → SeznamNaravnih → SeznamNaravnih

def stakni {A : Type} : List A → List A → List A :=
  fun xs ys =>
    match xs with
    | [] => ys
    | x :: xs' => x :: stakni xs' ys

set_option autoImplicit true

theorem prazen_stakni : stakni [] xs = xs :=
  by
    simp [stakni]

theorem stakni_prazen : stakni xs [] = xs :=
  by
    induction xs with
    | nil => simp [stakni]
    | cons x xs' ih =>
        calc
          stakni (x :: xs') []
            = x :: stakni xs' [] := by simp [stakni]
          _ = x :: xs' := by rw [ih]

theorem stakni_asoc : stakni xs (stakni ys zs) = stakni (stakni xs ys) zs :=
  by
    induction xs with
    | nil => simp [stakni]
    | cons x xs' ih =>
        calc
          stakni (x :: xs') (stakni ys zs)
            = x :: (stakni xs' (stakni ys zs)) := by simp [stakni]
          _ = x :: (stakni (stakni xs' ys) zs) := by rw [ih]
          _ = stakni (x :: (stakni xs' ys)) zs := by simp [stakni]
          _ = stakni (stakni (x :: xs') ys) zs := by simp [stakni]
