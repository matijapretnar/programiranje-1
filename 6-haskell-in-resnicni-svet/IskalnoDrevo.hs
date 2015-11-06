module IskalnoDrevo where
import Test.QuickCheck
import Control.Monad

data Drevo a = Prazno | Sestavljeno a (Drevo a) (Drevo a)
    deriving (Show, Eq)

instance Arbitrary a => Arbitrary (Drevo a) where
  arbitrary = sized tree'
    where
      tree' 0 = return Prazno
      tree' n | n > 0 = 
        oneof [return Prazno,
              liftM3 Sestavljeno arbitrary subtree subtree]
        where subtree = tree' (n `div` 2)


prezrcali :: Drevo a -> Drevo a
prezrcali Prazno = Prazno
prezrcali (Sestavljeno x l d) = Sestavljeno x (prezrcali d) (prezrcali l)


testPrezrcali :: Drevo Int -> Bool
testPrezrcali d = prezrcali (prezrcali d) == d

main = quickCheck testPrezrcali
