import Data.Array

len :: Array Integer Integer -> Integer
len xs = snd (Data.Array.bounds xs)

fixedBounded :: Array Integer Integer -> Integer -> Integer -> Maybe Integer
fixedBounded xs minI maxI =
  case compare minI maxI of
    GT -> Nothing
    EQ -> Nothing
    LT ->
      case compare (xs ! mid) mid  of
        LT -> fixedBounded xs (mid + 1) maxI
        EQ -> Just mid
        GT -> fixedBounded xs minI (mid - 1)
  where
        mid = (minI + maxI) `div` 2

fixed :: Array Integer Integer -> Maybe Integer
fixed xs = fixedBounded xs 0 (len xs)
