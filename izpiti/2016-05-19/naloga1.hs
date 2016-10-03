-- 1. naloga (Kopice)

data Drevo a = Drevo a [Drevo a] deriving Show

d :: Drevo Integer
d = Drevo 18 [
  Drevo 12 [],
  Drevo 10 [Drevo 5 []],
  Drevo 8 [Drevo 1 [], Drevo 3 []] ]

  
-- a)
  
vsota :: Num a => Drevo a -> a
vsota (Drevo x ds) = x + sum (map vsota ds)


-- b)

jeKopica :: Ord a => Drevo a -> Bool
jeKopica (Drevo x ds) = all (jeKopica' x) ds
  where
    jeKopica' x (Drevo y ds) = y < x && all (jeKopica' y) ds

    
-- c)

odstraniMax :: Ord a => Drevo a -> (a, Maybe (Drevo a))
odstraniMax (Drevo x []) = (x, Nothing)
odstraniMax (Drevo x ds) = (x, Just (Drevo x' (ds' ++ ds'')))
  where
    (Drevo x' ds', ds'') = odstraniNajvecjegaOtroka ds
    odstraniNajvecjegaOtroka [Drevo x ds] = (Drevo x ds, [])
    odstraniNajvecjegaOtroka (Drevo x ds : ds') =
        let (Drevo x'' ds'', ds''') = odstraniNajvecjegaOtroka ds' in
        if x < x'' then
            (Drevo x'' ds'', Drevo x ds : ds''')
        else
            (Drevo x ds, ds')

            
-- d)

padajociElementi :: Ord a => Drevo a -> [a]
padajociElementi d =
    case odstraniMax d of
        (x, Nothing) -> [x]
        (x, Just d) -> x : padajociElementi d
