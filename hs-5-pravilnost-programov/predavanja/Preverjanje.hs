import Test.QuickCheck

povecaj :: [Int] -> [Int]
povecaj = map succ

prop_SumPovecaj :: [Int] -> Property
prop_SumPovecaj xs =
  collect (length xs) $
  sum xs + length xs == sum (povecaj xs)

prop_SumPovecaj' :: [Int] -> Bool
prop_SumPovecaj' xs =
  sum xs < sum (povecaj xs)


sledi :: Bool -> Bool -> Bool
sledi True False = False
sledi _ _ = True

prop :: a -> [a] -> [a] -> Bool
prop x xs xs' =
  (length xs == length xs')
  `sledi`
  (length (x : xs) == length (x : xs'))

main = quickCheck prop_SumPovecaj
