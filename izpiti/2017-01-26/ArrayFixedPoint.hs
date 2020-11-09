import Data.Array

len :: Array Integer Integer -> Integer
len xs = snd (Data.Array.bounds xs)

fixedBounded :: Array Integer Integer -> Integer -> Integer -> Maybe Integer -> Maybe Integer
fixedBounded xs minI maxI minFix =
  case compare minI maxI of
    GT -> minFix
    EQ -> search
    LT -> search
  where search =
          case compare (xs ! mid) mid  of
            LT -> fixedBounded xs (mid + 1) maxI Nothing
            EQ -> fixedBounded xs minI (mid - 1) (Just mid)
            GT -> fixedBounded xs minI (mid - 1) Nothing
          where
            mid = (minI + maxI) `div` 2

fixed :: Array Integer Integer -> Maybe Integer
fixed xs = fixedBounded xs 0 (len xs) Nothing

toArray :: [Integer] -> Array Integer Integer
toArray xs =
  Data.Array.listArray (0, toInteger (length xs -1)) xs


xs0 :: Array Integer Integer
xs0 = toArray $ [-1, 0, 2]
xs1 :: Array Integer Integer
xs1 = toArray $ [-1, 0, 2] ++ [3..9]
xs2 :: Array Integer Integer
xs2 = toArray $ [-10..0] ++ [11]
xs3 :: Array Integer Integer
xs3 = toArray $ [-9..0] ++ [10]
xs4 :: Array Integer Integer
xs4 = toArray $ [0] ++ [2..10]
xs5 :: Array Integer Integer
xs5 = toArray $ [0] ++ [2..11]
xs6 :: Array Integer Integer
xs6 = toArray $ [-1, 1] ++ [2..11]
