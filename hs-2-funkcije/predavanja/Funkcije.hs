sestej :: Integer -> (Integer -> Integer)
sestej x y = x + y

let mojKoeficient = 2
let mojProstiClen = 1
let mojaFunkcija x = mojKoeficient * x + mojProstiClen

kompozitum :: (b -> c) -> (a -> b) -> (a -> c)
kompozitum f g = \x -> f (g x)

-- (trikrat f) x = f (f (f x))

-- ((trikrat trikrat) succ) 1 =
--     trikrat (trikrat (trikrat succ)) 1 =    
--     trikrat (trikrat (+ 3)) 1 =
--     trikrat (+ 9) 1 =
--     (+ 27) 1 =
--     28
