-- Seznam razdelimo na dva dela - ker pri urejanju z zlivanjem ni pomembno, na
-- kakšen način razdelimo seznam na dva dela, je eleganten način tudi ta,
-- da razdelimo na elemente na lihih in na sodih mestih.
razdeli :: [a] -> ([a], [a])
razdeli [] = ([], [])
razdeli (x:xs) = let (xs1, xs2) = razdeli xs in (x:xs2, xs1)

zlij :: Ord a => [a] -> [a] -> [a]
zlij [] ys = ys
zlij xs [] = xs
zlij (x:xs) (y:ys)
    | x <= y = x:zlij xs (y:ys)
    | otherwise = y:zlij (x:xs) ys

urediZZlivanjem :: Ord a => [a] -> [a]
urediZZlivanjem [] = []
urediZZlivanjem [x] = [x]
urediZZlivanjem xs =
    let (xs1, xs2) = razdeli xs in
    zlij (urediZZlivanjem xs1) (urediZZlivanjem xs2)


hitroUredi :: Ord a => [a] -> [a]

hitroUredi [] = []
hitroUredi (x:xs) =
  hitroUredi xs1 ++ [x] ++ hitroUredi xs2
    where
      (xs1, xs2) = pivotiraj xs x
      pivotiraj [] x = ([], [])
      pivotiraj (y:ys) x =
        let (ys1, ys2) = pivotiraj ys x in
        if y <= x then (y:ys1, ys2) else (ys1, y:ys2)


hitroUredi' :: Ord a => [a] -> [a]

hitroUredi' [] = []
hitroUredi' (x:xs) =
    hitroUredi' (filter (<= x) xs) ++ [x] ++ hitroUredi' (filter (> x) xs)
