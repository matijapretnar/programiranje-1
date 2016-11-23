stakni :: [a] -> [a] -> [a]
stakni [] ys = ys
stakni (x:xs) ys = x : stakni xs ys

-- Kaj, če bi poskusili delati analizo po drugem argumentu?

-- stakni' xs [] = xs
-- 
-- stakni' [] (y:ys) = (y:ys)
-- stakni' (x:xs) (y:ys) = x : stakni xs (y:ys)

-- Vidimo, da smo končali z isto definicijo kot zgoraj,
-- le malo bolj grdo.

-- (([] ++ [3]) ++ [2]) ++ [1] =
-- ([3] ++ [2]) ++ [1] =
-- [3, 2] ++ [1] =
-- [3, 2, 1]
-- 
-- ((([] ++ [10000]) ++ [9999]) ++ ...) ++ [1] =
-- ...
-- ([10000, 9999, 9998, ..., 3] ++ [2]) ++ [1]

obrniPocasi :: [a] -> [a]
obrniPocasi = foldr (\x acc -> acc ++ [x]) []

obrniHitro :: [a] -> [a]
obrniHitro = foldl (\acc x -> x : acc) []
