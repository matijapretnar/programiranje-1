class Vektorski a where
    (+++) :: a -> a -> a
    (***) :: Double -> a -> a


instance  Vektorski Double  where
    (+++) = (+)
    (***) = (*)


instance  (Vektorski a, Vektorski b) => Vektorski (a, b)  where
    (x1, y1) +++ (x2, y2) = (x1 +++ x2, y1 +++ y2)
    lam *** (x, y) = (lam *** x, lam *** y)


class  Vektorski a => SkalarniProdukt a  where
    (...) :: a -> a -> Double


instance  SkalarniProdukt Double  where
    (...) = (*)

instance  (SkalarniProdukt a, SkalarniProdukt b) => SkalarniProdukt (a, b)  where
    (x1, y1) ... (x2, y2) = (x1 ... x2) + (y1 ... y2)
