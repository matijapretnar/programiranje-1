def vsota : List Int → Int :=
  fun xs =>
    match xs with
    | .nil => 0
    | .cons x xs => x + vsota xs

def aux : List Int → Int → Int :=
  fun xs => fun acc =>
    match xs with
    | .nil => acc
    | .cons x xs => aux xs (acc + x)

def vsota' : List Int → Int :=
  fun xs => aux xs 0

-- theorem vsota_je_vsota' : vsota xs = vsota' xs :=
--   by
--     induction xs
--     case nil => rfl
--     case cons x xs ih =>
--       calc
--         vsota (x :: xs)
--         _ = x + vsota xs := by rfl
--         _ = x + vsota' xs := by rw [ih]
--         _ = vsota' (x :: xs) := by sorry

theorem vsota_je_aux : ∀ acc, acc + vsota xs = aux xs acc :=
  by
    induction xs with
    | nil =>
      intro acc
      calc
        acc + vsota []
        _ = acc + 0 := by simp [vsota]
        _ = acc := by rw [Int.add_zero]
        _ = aux [] acc := by simp [aux]
    | cons x xs ih =>
      intro acc
      calc
        acc + vsota (x :: xs)
        _ = acc + (x + vsota xs) := by simp [vsota]
        _ = (acc + x) + vsota xs := by rw [Int.add_assoc]
        _ = aux xs (acc + x) := by rw [ih]
        _ = aux (x :: xs) acc := by simp [aux]

theorem vsota_je_vsota' : vsota xs = vsota' xs :=
  by
    calc
      vsota xs
      _ = 0 + vsota xs := by rw [Int.zero_add]
      _ = aux xs 0 := by rw [vsota_je_aux]
      _ = vsota' xs := by simp [vsota']
