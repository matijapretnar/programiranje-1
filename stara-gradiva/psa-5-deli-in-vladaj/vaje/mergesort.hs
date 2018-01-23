split :: [a] -> ([a], [a])
split [] = ([], [])
split [x] = ([x], [])
split (x : y : xys) = (x : xs, y : ys)
  where (xs, ys) = split xys

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys) =
  case compare x y of
    LT -> x : (merge xs (y:ys))
    EQ -> x : (merge xs (y:ys))
    GT -> y : (merge (x:xs) ys)

mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xys =
  merge (mergesort xs) (mergesort ys)
  where (xs, ys) = split xys
