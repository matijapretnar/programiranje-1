-- 3. naloga (Drevesa)

data Drevo a = List a
             | Razvejano (Drevo a) (Drevo a)
             deriving (Show)

d = Razvejano
    (Razvejano (List 3) (Razvejano (List 1) (List 4)))
    (Razvejano (List 1) (List 5))

-- a)
naloga3a :: Drevo a -> [a]
naloga3a (List x) = [x]
naloga3a (Razvejano levo desno) = (naloga3a levo) ++ (naloga3a desno)

-- b)
naloga3b' :: Drevo a -> a -> Drevo a
naloga3b' (List _) x = List x
naloga3b' (Razvejano levo desno) x = Razvejano (naloga3b' levo x) (naloga3b' desno x)

naloga3max :: Ord a => Drevo a -> a
naloga3max (List x) = x
naloga3max (Razvejano levo desno) = max (naloga3max levo) (naloga3max desno)

naloga3b :: Ord a => Drevo a -> Drevo a
naloga3b d = naloga3b' d m
    where m = naloga3max d

-- c)
naloga3c' :: Drevo a -> [b] -> (Drevo b, [b])

naloga3c' (List _) (x:lx) = (List x, lx)
naloga3c' (List _) [] = error "seznam je prekratek"

naloga3c' (Razvejano levo desno) lx =
    let
        (novoLevo, lx2) = naloga3c' levo lx
        (novoDesno, lx3) = naloga3c' desno lx2
    in
        (Razvejano novoLevo novoDesno, lx3)

naloga3c :: Drevo a -> [b] -> Drevo b
naloga3c d l =
    let
        (novo, ostanek) = naloga3c' d l
    in
        if length ostanek == 0 then novo else error "seznam je predolg"
