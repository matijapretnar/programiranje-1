-- AVL drevesa

{-  AVL drevo je uravnoteženo dvojiško iskalno drevo, torej takšno iskalno
drevo, v katerem se pri vsakem vozlišču globini levega in desnega poddrevesa
razlikujeta za največ 1. Izumila sta ga sovjetska matematika Adelson-Velskii in
Landis v šestdesetih letih. Podatkovni tip AVLDrevo je že delno implementiran.
Ker si ne moremo privoščiti, da bi globino poddreves računali vsakič znova, te
podatke računamo sproti in hranimo v vozliščih drevesa (konstruktor ima torej
dodaten parameter). -}

data AVLDrevo a = Prazno | Sestavljeno Int (AVLDrevo a) a (AVLDrevo a) deriving (Show)

-- here's a drawing function, which, combined with [putStrLn], lets you visualise a tree
data S = L | R
draw :: Show a => AVL a -> String
draw t = "\n" ++ draw' Nothing t 0 ++ "\n"
  where
    draw' _ Prazno _ = []
    draw' dir (Sestavljeno _ l v r) d =
      draw' (Just R) r (d+1) ++ node dir ++ draw' (Just L) l (d+1)
      where
        node dir' = padding d ++
          case dir' of
            Nothing -> ""
            Just L -> "\\- "
            Just R -> "/- "
          ++ show v ++ "\n"
        padding n = replicate (n*4) ' '


prazno :: AVLDrevo a
prazno = Prazno

-- Sestavi pomožni konstuktor [avldrevo], ki iz treh parametrov (levo, x, desno)
-- sestavi AVLDrevo. V gornji konstruktor je torej potrebno dodati le še globino
-- drevesa.
avldrevo :: AVLDrevo a -> a -> AVLDrevo a -> AVLDrevo a
avldrevo l x d = Sestavljeno h l x d where h = 1 + max (visina l) (visina d)

-- Zdaj sestavi še funkcijo [visina], ki iz AVL drevesa prebere njegovo višino.
visina :: AVLDrevo a -> Int
visina Prazno = 0
visina (Sestavljeno h _ _ _) = h

-- Implementiraj funkcijo [vsebuje], ki ugotovi, če AVL drevo vsebuje element.
vsebuje :: (Ord a) => AVLDrevo a -> a -> Bool
vsebuje Prazno _ = False
vsebuje (Sestavljeno _ l y d) x
    | x < y = vsebuje l x
    | x > y = vsebuje d x
    | otherwise = True

-- AVL drevesa so uravnotežena. Implementiraj pomožno funkcijo [razlika], ki
-- izračuna razliko višin levega in desnega poddrevesa danega AVL drevesa.
razlika :: AVLDrevo a -> Int
razlika Prazno = 0
razlika (Sestavljeno _ l _ d) = visina l - visina d

-- Kadar drevo ni uravnoteženo, ga lahko popravimo z levimi oziroma desnimi
-- rotacijami. Implementiraj funkciji [rotL] in [rotD], ki izvedeta eno levo
-- oziroma desno rotacijo na AVL drevesu.
rotD :: AVLDrevo a -> AVLDrevo a
rotD (Sestavljeno _ (Sestavljeno _ ll xl dl) x d) = avldrevo ll xl (avldrevo dl x d)
rotD _ = undefined

rotL :: AVLDrevo a -> AVLDrevo a
rotL (Sestavljeno _ l x (Sestavljeno _ ld xd dd)) = avldrevo (avldrevo l x ld) xd dd
rotL _ = undefined

-- Implementiraj funkcijo [uravnotezi], ki dano drevo uravnotezi. Predpostavi,
-- da je dano drevo "skoraj" uravnoteženo: dobili smo ga tako, da smo nekemu AVL
-- drevesu dodali ali odstranili eno vozlišče.
uravnotezi :: AVLDrevo a -> AVLDrevo a
uravnotezi Prazno = Prazno
uravnotezi m@(Sestavljeno _ l x d)
    | razlika m == 2 && razlika l == 1 = rotD m
    | razlika m == 2 = rotD $ avldrevo (rotL l) x d
    | razlika m == -2 && razlika d == -1 = rotL m
    | razlika m == -2 = rotL $ avldrevo l x (rotD d)
    | otherwise = m

-- Implementiraj funkcijo [dodaj], ki AVL drevesu doda element. Pri tem
-- upoštevaj, da bo morda potrebno drevo še uravnotežiti.
dodaj :: (Ord a) => AVLDrevo a -> a -> AVLDrevo a
dodaj Prazno x = Sestavljeno 1 Prazno x Prazno
dodaj m@(Sestavljeno _ l y d) x
    | x < y = uravnotezi $ avldrevo (dodaj l x) y d
    | x > y = uravnotezi $ avldrevo l y (dodaj d x)
    | otherwise = m

-- Ko je funkcija [dodaj] implementirana, lahko sestavimo še funkcijo [izSeznama],
-- ki iz seznama sestavi AVL drevo.
izSeznama :: (Ord a) => [a] -> AVLDrevo a
izSeznama = foldl dodaj prazno

-- Implementiraj funkcijo [najboljLevi], ki vrne najbolj levi element
-- danega AVL drevesa.
najboljLevi :: (Ord a) => AVLDrevo a -> Maybe a
najboljLevi Prazno = Nothing
najboljLevi (Sestavljeno _ Prazno x _) = Just x
najboljLevi (Sestavljeno _ l _ _) = najboljLevi l

-- Implementiraj funkcijo [odstrani], ki iz AVL drevesa odstrani element.
odstrani :: Ord a => AVLDrevo a -> a -> AVLDrevo a
odstrani Prazno _ = Prazno
odstrani (Sestavljeno _ l x d) y = uravnotezi $ novoDrevo
    where
        novoDrevo = case compare x y of
            LT -> avldrevo l x (odstrani d y)
            EQ -> case najboljLevi d of
                Nothing -> l
                Just najLevi -> avldrevo l najLevi (odstrani d najLevi)
            GT -> avldrevo (odstrani l y) x d
