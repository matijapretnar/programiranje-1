module IskalnoDrevo where

    data Drevo a = Prazno
                 | Sestavljeno (Drevo a) a (Drevo a)

    instance Show a => Show (Drevo a) where
        show d = show' "" d
          where
            show' zamik Prazno = zamik ++ "x"
            show' zamik (Sestavljeno levo vrednost desno) =
                init $ unlines [
                    show' (zamik ++ "   ") desno,
                    zamik ++ show vrednost,
                    show' (zamik ++ "   ") levo
                ]

    iskalnoDrevo :: (Ord a) => [a] -> Drevo a
    iskalnoDrevo = foldl dodaj Prazno

    dodaj :: (Ord a) => Drevo a -> a -> Drevo a
    dodaj Prazno x = Sestavljeno Prazno x Prazno
    dodaj (Sestavljeno levo vrednost desno) x
        | x < vrednost = Sestavljeno (dodaj levo x) vrednost desno
        | x > vrednost = Sestavljeno levo vrednost (dodaj desno x)
        | otherwise = Sestavljeno levo vrednost desno

    poisci :: (Ord a) => Drevo a -> a -> Bool
    poisci Prazno _ = False
    poisci (Sestavljeno levo vrednost desno) x
       | x < vrednost = poisci levo x
       | x > vrednost = poisci desno x
       | otherwise = True

    izbrisi :: (Ord a) => Drevo a -> a -> Drevo a
    izbrisi Prazno _ = Prazno
    izbrisi (Sestavljeno levo vrednost desno) x
        | x < vrednost = Sestavljeno (izbrisi levo x) vrednost desno
        | x > vrednost = Sestavljeno levo vrednost (izbrisi desno x)
        | otherwise = izbrisiKoren (Sestavljeno levo vrednost desno)

    najboljLevi :: Drevo a -> a
    najboljLevi (Sestavljeno Prazno vrednost _) = vrednost
    najboljLevi (Sestavljeno levo _ _) = najboljLevi levo

    izbrisiKoren :: (Ord a) => Drevo a -> Drevo a
    izbrisiKoren (Sestavljeno Prazno vrednost desno) = desno
    izbrisiKoren (Sestavljeno levo vrednost Prazno) = levo
    izbrisiKoren (Sestavljeno levo vrednost desno) =
        let vrednost' = najboljLevi desno in
        Sestavljeno levo vrednost' (izbrisi desno vrednost')
