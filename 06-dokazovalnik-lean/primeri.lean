def f1 : Int -> Int := fun x => x + 1
def f2 (x : Int) : Int := x + 1
def f3 (x : Int) := x + 1
def f4 x := x + 1

#eval f 10

#print f

#check f 10

def stakni (xs : List Int) (ys : List Int) :=
  match xs with
  | [] => ys
  | x :: xs' => x :: stakni xs' ys

def sestej (n : Nat) (m : Nat) :=
  match n with
  | 0 => m
  | Nat.succ n' => Nat.succ (sestej n' m)

def sestej_s_taktikami : Nat -> Nat -> Nat :=
  by
    intro m n
    assumption

#print sestej_s_taktikami

def sestej_nat (n m : Nat) :=
  match n with
  | Nat.zero => m
  | Nat.succ n' => Nat.succ (sestej_nat n' m)

def curry (f : A × B → C) : A → (B → C) :=
  fun x => fun y => f ⟨x, y⟩

-- def dist (A : Type) (B : Type) (C : Type) (f : Sum A B → C) : (A → C) × (B → C) :=
def dist (A B C : Type) (f : Sum A B → C) : (A → C) × (B → C) :=
  ⟨
    (fun (x : A) => f (.inl x)),
    (fun (y : B) => f (.inr y))
  ⟩

def dist_prop (A B C : Prop) (f : A ∨ B → C) : (A → C) ∧ (B → C) :=
  ⟨
    (fun (x : A) => f (.inl x)),
    (fun (y : B) => f (.inr y))
  ⟩

def identiteta {A : Type} (x : A) := x

axiom ni_tretje (P : Prop) : P ∨ ¬ P

def dist_prop_s_taktikami (A B C : Prop) (f : A ∨ B → C) : (A → C) ∧ (B → C) :=
  by
    constructor
    intro H
    apply f
    left
    assumption
    intro H
    apply f
    right
    assumption

#print dist_prop

#print dist_prop_s_taktikami
