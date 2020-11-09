-- 1. naloga (Funkcije)

-- a)
naloga1a :: [a -> a] -> a -> a
naloga1a [] x = x
naloga1a (s:ls) x = naloga1a ls (s x)

res1 = naloga1a [negate, (+ 1), subtract 3, (2 -), (* 2)] 5

-- b)
naloga2a :: Ord a => [a -> a] -> a -> a
naloga2a [] x = x
naloga2a (s:ls) x = naloga2a ls (max x (s x))

res2 = naloga2a [negate, (+ 1), subtract 3, (2 -), (* 2)] 5

-- c)
naloga3a :: Ord a => [a -> a] -> a -> a
naloga3a [] x = x
naloga3a (s:ls) x = max (naloga3a ls (s x)) (naloga3a ls x)

res3 = naloga3a [negate, (+ 1), subtract 3, (2 -), (* 2)] 5
