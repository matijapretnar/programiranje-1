class  Vektorski a  where
    (^+^) :: a -> a -> a
    (^*^) :: Double -> a -> a

instance  Vektorski Double  where
    --x ^+^ y = x + y
    (^+^) = (+)
    (^*^) = (*)

instance (Vektorski a, Vektorski b) => Vektorski (a, b) where
    (x1, y1) ^+^ (x2, y2) = (x1 ^+^ x2, y1 ^+^ y2)
    a ^*^ (x, y) = (a ^*^ x, a ^*^ y)

-- Haskell preverja le pravilnost tipov, ne pa tudi tega, ali so definicije
-- smiselne (torej ali je v našem primeru res zadoščeno lastnostim vektorskega
-- prostora).

-- instance  Vektorski [Char]  where
--     x ^+^ y = x ++ y
--     a ^*^ x = (show a) ++ " * " ++ x
-- 
-- Če želimo tip [Char] dodati v razred Vektorski, moramo na vrhu
-- datoteke napisati (odkomentirano seveda)
--   {-# LANGUAGE FlexibleInstances #-}

class  Vektorski a => Skalarni a  where
    (^.^) :: a -> a -> Double

instance  Skalarni Double  where
    (^.^) = (*)

instance (Skalarni a, Skalarni b) => Skalarni (a, b) where
    (x1, y1) ^.^ (x2, y2) = x1 ^.^ x2 + y1 ^.^ y2



data Drevo a
  = Prazno
  | Sestavljeno a (Drevo a) (Drevo a)
  deriving Show


mojeDrevo =
  Sestavljeno 1
    (Sestavljeno 2
      (Sestavljeno 3 Prazno Prazno)
      Prazno
    )
    (Sestavljeno 4 Prazno Prazno)


instance  Functor Drevo  where
    fmap _ Prazno = Prazno
    fmap f (Sestavljeno x l d) =
      Sestavljeno (f x) (fmap f l) (fmap f d)


instance  Foldable Drevo  where
    foldr f z Prazno = z
    foldr f z (Sestavljeno x l d) =
      foldr f (f x (foldr f z d)) l
    -- manjkajo še preostale funkcije


-- Bolje bi bilo, če bi pisali
--
-- data Drevo a
--   = Prazno
--   | Sestavljeno (Drevo a) a (Drevo a)
--  deriving (Show, Functor, Foldable)
--
-- V tem primeru mora biti vrednost na sredini, da se fold izvaja v pravem
-- vrstnem redu.
