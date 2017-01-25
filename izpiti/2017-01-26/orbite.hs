orbita :: Eq a => (a -> a) -> a -> [a]
orbita f a = orbita' [] a
  where
    orbita' videni x
      | x `elem` videni = []
      | otherwise = x : orbita' (x : videni) (f x)

generatorji :: Eq a => (a -> a) -> [a] -> [a]
generatorji f or = fst $ generatorji' or
  where
    generatorji' [] = ([], [])
    generatorji' (x:xs) =
      if x `elem` generirani2 && length gen2 <= length gen1 then
        (gen2, generirani2)
      else
        (x : gen1, orb ++ generirani1)
        where
          (gen1, generirani1) = generatorji' (filter (`notElem` orb) xs)
          (gen2, generirani2) = generatorji' xs
          orb = orbita f x
