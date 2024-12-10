def concat {A : Type} : List A → List A → List A :=
  fun xs ys =>
    match xs with
    | [] => ys
    | x :: xs' => x :: concat xs' ys

#check (concat ["a", "b"] ["c", "d"])

def reverse {A : Type} : List A → List A :=
  fun xs =>
    match xs with
    | [] => []
    | x :: xs' => concat (reverse xs') [x]


#check (reverse ["a", "b", "c", "d"])

def length {A : Type} : List A → Nat :=
  fun xs =>
    match xs with
    | [] => 0
    | _ :: xs' => 1 + length xs'


#check (length ["a", "b", "c", "d"])

theorem trd1  {A : Type} {x : A} : reverse [x] = [x] :=
  by
    simp [reverse]
    simp [concat]

theorem trd2 {A : Type} {xs ys : List A} : length (concat xs ys) = length xs + length ys :=
  by
    induction xs with
    | nil =>
      simp [concat, length]
    | cons x xs' ih =>
      simp [concat, length]
      rw [ih, Nat.add_assoc]

-- Tega poznamo že iz predavanj
theorem trd3 {A : Type} {xs : List A} : concat xs [] = xs :=
  by
    induction xs with
    | nil =>
      simp [concat]
    | cons x xs' ih =>
      simp [concat]
      rw [ih]

theorem trd4 {A : Type} {xs ys zs : List A} : concat (concat xs ys) zs = concat xs (concat ys zs) :=
  by
    induction xs with
    | nil =>
      simp [concat]
    | cons x xs' ih =>
      simp [concat]
      rw [ih]

theorem trd5 {A : Type} {xs ys : List A} : reverse (concat xs ys) = concat (reverse ys) (reverse xs) :=
  by
    induction xs with
    | nil =>
      simp [concat, reverse]
      rw [trd3]
    | cons x xs' ih =>
      simp [concat, reverse]
      rw [ih, trd4]

theorem trd6 {A : Type} {xs : List A} : length (reverse xs) = length xs :=
  by
    induction xs with
    | nil =>
      simp [reverse, length]
    | cons x xs' ih =>
      simp [reverse, length]
      rw [trd2, ih]
      simp [length]
      rw [Nat.add_comm]

theorem trd7 {A : Type} {xs : List A} : reverse (reverse xs) = xs :=
  by
    induction xs with
    | nil =>
      simp [reverse]
    | cons x xs' ih =>
      simp [reverse]
      rw [trd5, ih]
      simp [concat]


def map {A B : Type} : (A → B) → List A → List B :=
  fun f xs =>
    match xs with
    | [] => []
    | x :: xs' => f x :: map f xs'

theorem map_assoc {A B C : Type} {f : A → B} {g : B → C} {xs : List A} : map g (map f xs) = map (g ∘ f) xs :=
  by
    induction xs with
    | nil =>
      simp [map]
    | cons x xs' ih =>
      simp [map]
      rw [ih]

theorem map_id {A : Type} {xs : List A} : map id xs = xs :=
  by
    induction xs with
    | nil =>
      simp [map]
    | cons x xs' ih =>
      simp [map]
      rw [ih]

theorem map_concat {A B : Type} {f : A → B} {xs ys : List A} : map f (concat xs ys) = concat (map f xs) (map f ys) :=
  by
    induction xs with
    | nil =>
      simp [concat, map]
    | cons x xs' ih =>
      simp [concat, map]
      rw [ih]


theorem map_reverse {A B : Type} {f : A → B} {xs : List A} : map f (reverse xs) = reverse (map f xs) :=
  by
    induction xs with
    | nil =>
      simp [reverse, map]
    | cons x xs' ih =>
      simp [reverse, map]
      rw [map_concat, ih]
      simp [reverse, map]

inductive tree (A : Type) : Type where
  | empty : tree A
  | node : A → tree A → tree A → tree A

#check tree.rec

def tree_map {A B : Type} : (A → B) → tree A → tree B :=
  fun f t =>
   match t with
    | tree.empty => tree.empty
    | tree.node x l r => tree.node (f x) (tree_map f l) (tree_map f r)

theorem tree_map_empty {A B : Type} {f : A → B} : tree_map f tree.empty = tree.empty :=
  by
    simp [tree_map]

theorem tree_map_comp {A B C : Type} {f : A → B} {g : B → C} {t : tree A} : tree_map g (tree_map f t) = tree_map (g ∘ f) t :=
  by
    induction t with
    | empty =>
      simp [tree_map]
    | node x l r ihl ihr =>
      simp [tree_map]
      rw [ihl, ihr]
      constructor
      rfl
      rfl

def depth {A : Type} : tree A → Nat :=
  fun t =>
    match t with
    | tree.empty => 0
    | tree.node _ l r => 1 + Nat.max (depth l) (depth r)

theorem max_comm {a b : Nat} : Nat.max a b = Nat.max b a :=
  sorry

def mirror {A : Type} : tree A → tree A :=
  fun t =>
    match t with
    | tree.empty => tree.empty
    | tree.node x l r => tree.node x (mirror r) (mirror l)

theorem mirror_depth {A : Type} {t : tree A} : depth (mirror t) = depth t :=
  by
    induction t with
    | empty =>
      simp [mirror, depth]
    | node x l r ihl ihr =>
      simp [mirror, depth]
      rw [ihl, ihr]
      rw [max_comm]

theorem mirror_mirror {A : Type} {t : tree A} : mirror (mirror t) = t :=
  by
    induction t with
    | empty =>
      simp [mirror]
    | node x l r ihl ihr =>
      simp [mirror]
      rw [ihl, ihr]
      simp [mirror]

def collect {A : Type} : tree A → List A :=
  fun t =>
    match t with
    | tree.empty => []
    | tree.node x l r => concat (collect l) (concat [x]  (collect r))

theorem trd8 {A : Type} {x : A} {xs ys : List A} : concat xs (x::ys) = concat (concat xs [x]) ys :=
  by
    induction xs with
    | nil =>
      simp [concat]
    | cons x xs' ih =>
      simp [concat]
      rw [ih]


theorem collect_mirror {A : Type} {t : tree A} : collect (mirror t) = reverse (collect t) :=
  by
    induction t with
    | empty =>
      simp [mirror, collect, reverse]
    | node x l r ihl ihr =>
      simp [mirror, collect, reverse]
      rw [ihl, ihr]
      simp [concat]
      rw [trd5]
      rw [trd8]
      simp [reverse]


def size {A : Type} : tree A → Nat :=
  fun t =>
    match t with
    | tree.empty => 0
    | tree.node _ l r => 1 + size l + size r

theorem size_mirror {A : Type} {t : tree A} : size (mirror t) = size t :=
  by
    induction t with
    | empty =>
      simp [mirror, size]
    | node x l r ihl ihr =>
      simp [mirror, size]
      rw [ihl, ihr]
      rw [Nat.add_assoc, Nat.add_comm (size r) (size l), Nat.add_assoc]


--- Indukcija na pomožnih funkcijah z akumulatorjem

theorem concat2 (x : A) (xs ys : List A) : concat xs (x :: ys) = concat (concat (xs) [x]) ys :=
  by
    induction xs with
    | nil => simp [concat]
    | cons x xs' ih =>
      simp [concat]
      assumption

-- Definirajte repno rekurzivno funkcijo, ki obrne seznam
def reverse_tlrec {A : Type} (xs : List A) : List A :=
  let rec aux : List A → List A → List A
    | [], acc => acc
    | x :: xs', acc => aux xs' (x :: acc)
  aux xs []

theorem reverse_tlrec.aux_eq_reverse {A : Type} :
  ∀ {xs : List A}, ∀ {acc : List A}, reverse_tlrec.aux xs acc = concat (reverse xs) acc :=
  by
    intro xs
    induction xs with
    | nil =>
      simp [reverse, reverse_tlrec.aux, concat]
    | cons x xs' ih =>
      intro acc
      simp [reverse, reverse_tlrec.aux]
      rw [← concat2]
      rw [ih]

-- Dokažite, da je vaša funkcija pravilna
theorem reverse_eq_reverse_tlrec {A : Type} : ∀ {xs : List A}, reverse xs = reverse_tlrec xs :=
  by
    intro xs
    rw [reverse_tlrec]
    rw [reverse_tlrec.aux_eq_reverse]
    rw [trd3]

-- Lepše je z drugače definirano funkcijo reverse

def reverse_direct {A : Type} : List A → List A :=
  fun xs =>
    match xs with
    | [] => []
    | x :: xs' => (reverse_direct xs') ++ [x]

theorem reverse_tlrec.aux_eq_reverse_direct {A : Type} : ∀ {xs : List A}, ∀ {acc : List A},
  reverse_tlrec.aux xs acc = (reverse_direct xs) ++ acc :=
  by
    intro xs
    induction xs with
    | nil =>
      intro acc
      simp [reverse_tlrec.aux, reverse_direct]
    | cons x xs' ih =>
      intro acc
      simp [reverse_tlrec.aux, reverse_direct]
      rw [ih]
