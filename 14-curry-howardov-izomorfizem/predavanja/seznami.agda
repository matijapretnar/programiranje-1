data Nat : Set where
    Z : Nat
    S : Nat -> Nat

plus : Nat -> Nat -> Nat
plus Z n = n
plus (S m) n = S (plus m n)

zmnozi : Nat -> Nat -> Nat
zmnozi Z _ = Z
zmnozi (S m) n = plus (zmnozi m n) n

data List : Set -> Set where
    prazen : (A : Set) -> List A
    sestavljen : {A : Set} -> A -> List A -> List A

dolzina : {A : Set} -> List A -> Nat
dolzina (prazen _) = Z
dolzina (sestavljen _ rep) = S (dolzina rep)

mojSeznam : List Nat
mojSeznam = sestavljen (S Z) (sestavljen Z (prazen _))  -- 1 :: 0 :: []_Nat

data _<_ : Nat -> Nat -> Set where
    <Z : (n : Nat) -> Z < S n
    <S : {m n : Nat} -> m < n -> S m < S n

vzemi : {A : Set} -> (xs : List A) -> (n : Nat) -> (n < dolzina xs) -> A
vzemi (prazen _) _ ()
vzemi (sestavljen glava _) Z (<Z _) = glava
vzemi (sestavljen _ rep) (S n) (<S dokaz_da_je_n_manjsi_od_predhodnika_dolzine_seznama) =
    vzemi rep n dokaz_da_je_n_manjsi_od_predhodnika_dolzine_seznama


data jeUrejen : List Nat -> Set where
   prazenJeUrejen : jeUrejen (prazen Nat)
   singletonJeUrejen : (n : Nat) -> jeUrejen (sestavljen n (prazen Nat))
   sestavljenJeUrejen :
    (m n : Nat) (rep : List Nat) -> m < n -> jeUrejen (sestavljen n rep) -> jeUrejen (sestavljen m (sestavljen n rep))

-- uredi : List Nat -> List Nat
-- uredi (prazen _) = prazen _
-- uredi (sestavljen glava (prazen _)) = sestavljen glava (prazen _)

-- urediDelaPrav : (xs : List Nat) -> jeUrejen (uredi xs)
-- urediDelaPrav (prazen _) = prazenUrejen
-- urediDelaPrav (sestavljen glava (prazen _)) = singletonUrejen glava
-- urediDelaPrav (sestavljen x1 (sestavljen y1 (prazen _)))

