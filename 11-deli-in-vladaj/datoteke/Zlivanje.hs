razdeli :: [a] -> ([a], [a])
razdeli [] = ([], [])
razdeli (x:xs) = let (xs1, xs2) = razdeli xs in (x:xs2, xs1)

zlij :: (Ord a) => [a] -> [a] -> [a]
zlij [] ys = ys
zlij xs [] = xs
zlij (x:xs) (y:ys)
    | x <= y = x:zlij xs (y:ys)
    | otherwise = y:zlij (x:xs) ys

uredi :: (Ord a) => [a] -> [a]
uredi [] = []
uredi [x] = [x]
uredi xs =
    let (xs1, xs2) = razdeli xs in
    zlij (uredi xs1) (uredi xs2)

hitroUredi :: (Ord a) => [a] -> [a]
hitroUredi [] = []
hitroUredi (x:xs) = hitroUredi manjsi ++ [x] ++ hitroUredi vecji
    where
        manjsi = filter (<= x) xs
        vecji = filter (> x) xs