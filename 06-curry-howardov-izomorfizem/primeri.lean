#eval 6 * 7

#check 6 * 7

def a := -6.3

#eval a * (a + 1)

def f (x : Int) := x + 3

#check f (-3)

-- def g (x : Int) : Int := g x  -- ne dela

def dokaz_nase_izjave : (A × B → C) → (A → (B → C)) :=
  fun (f : (A × B → C)) => (fun (x : A) => (fun (y : B) => f (x, y)))
