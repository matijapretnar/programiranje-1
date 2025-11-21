theorem brivec  :
  (Č : Type) → (B : Č → Č → Prop) →
  ¬ (∃ (b : Č), ∀ (č : Č), B b č ↔ ¬ B č č) :=
  by
    intro Č
    intro B
    intro H
    cases H
    rename_i bob Hbob
    have ali_se_bob_brije := Hbob bob
    have bob_se_ne_brije : ¬ B bob bob :=
      by
        intro bob_se_brije
        have bob_se_ne_brije :=
          ali_se_bob_brije.mp bob_se_brije
        contradiction
    have bob_se_brije :=
      ali_se_bob_brije.mpr bob_se_ne_brije
    contradiction

def stakni {A} : List A → List A → List A
  | [], ys => ys
  | x :: xs, ys => x :: stakni xs ys

def dolzina {A} : List A → Nat
  | [] => 0
  | _ :: xs => dolzina xs + 1

theorem prazen_stakni {A} {xs : List A} : stakni [] xs = xs :=
  by
    simp [stakni]

theorem stakni_prazen {A} {xs : List A} : stakni xs [] = xs :=
  by
    induction xs with
    | nil => simp [stakni]
    | cons x xs' ih =>
        calc
          stakni (x :: xs') []
          _ = x :: stakni xs' [] := by simp [stakni]
          _ = x :: xs' := by rw [ih]

theorem stakni_dolzina {A} {xs ys : List A} :
  dolzina (stakni xs ys) = dolzina xs + dolzina ys :=
  by
    induction xs with
    | nil =>
        calc
          dolzina (stakni [] ys)
          _ = dolzina ys := by simp [stakni]
          _ = 0 + dolzina ys := by rw [Nat.zero_add]
          _ = dolzina [] + dolzina ys := by simp [dolzina]
    | cons x xs' ih =>
        calc
          dolzina (stakni (x :: xs') ys)
          _ = dolzina (stakni xs' ys) + 1
              := by simp [stakni, dolzina]
          _ = dolzina xs' + dolzina ys + 1
              := by rw [ih]
          _ = (dolzina xs' + 1) + dolzina ys
              := by omega
          _ = dolzina (x :: xs') + dolzina ys
              := by simp [dolzina]

#print stakni_dolzina

inductive NeskoncnoVejeceDrevo where
  | Prazno : NeskoncnoVejeceDrevo
  | Sestavljeno : (Nat -> NeskoncnoVejeceDrevo) -> NeskoncnoVejeceDrevo

inductive TipKiNePodpiraIndukcije where
  | Prazen : TipKiNePodpiraIndukcije
  | Sestavljen : (TipKiNePodpiraIndukcije -> Nat) -> TipKiNePodpiraIndukcije

def vsota : List Int → Int
  | .nil => 0
  | .cons x xs => x + vsota xs

def pomozna : List Int → Int → Int :=
  fun xs => fun acc =>
    match xs with
    | .nil => acc
    | .cons x xs => pomozna xs (acc + x)

def vsota' : List Int → Int :=
  fun xs => pomozna xs 0

theorem vsota_pomozna : forall (xs : List Int) (acc : Int),
  acc + vsota xs = pomozna xs acc :=
  by
    intro xs
    induction xs with
    | nil =>
        simp [pomozna, vsota]
    | cons x xs' ih =>
        intro acc
        calc
          acc + vsota (x :: xs')
          _ = acc + (x + vsota xs') := by simp [vsota]
          _ = (acc + x) + vsota xs' := by omega
          _ = pomozna xs' (acc + x) := by rw [ih]
          _ = pomozna (x :: xs') acc := by simp [pomozna]
