-- AVL drevesa

{-  AVL drevo je uravnoteženo dvojiško iskalno drevo, torej takšno iskalno
drevo, v katerem se pri vsakem vozlišču globini levega in desnega poddrevesa
razlikujeta za največ 1. Izumila sta ga sovjetska matematika Adelson-Velskii in
Landis v šestdesetih letih. Podatkovni tip AVLDrevo je že delno implementiran.
Ker si ne moremo privoščiti, da bi globino poddreves računali vsakič znova, te
podatke računamo sproti in hranimo v vozliščih drevesa (konstruktor ima torej
dodaten parameter). -}

import System.Environment

data AVLDrevo a = Prazno | Sestavljeno Int (AVLDrevo a) a (AVLDrevo a) deriving (Show)

prazno :: AVLDrevo a
prazno = Prazno

-- Sestavi pomožni konstuktor [avldrevo], ki iz treh parametrov (levo, x, desno)
-- sestavi AVLDrevo. V gornji konstruktor je torej potrebno dodati le še globino
-- drevesa.
avldrevo :: AVLDrevo a -> a -> AVLDrevo a -> AVLDrevo a
avldrevo = undefined

-- Zdaj sestavi še funkcijo [visina], ki iz AVL drevesa prebere njegovo višino.
visina :: AVLDrevo a -> Int
visina = undefined

-- Implementiraj funkcijo [vsebuje], ki ugotovi, če AVL drevo vsebuje element.
vsebuje :: (Ord a) => AVLDrevo a -> a -> Bool
vsebuje = undefined

-- AVL drevesa so uravnotežena. Implementiraj pomožno funkcijo [razlika], ki
-- izračuna razliko višin levega in desnega poddrevesa danega AVL drevesa.
razlika :: AVLDrevo a -> Int
razlika = undefined

-- Kadar drevo ni uravnoteženo, ga lahko popravimo z levimi oziroma desnimi
-- rotacijami. Implementiraj funkciji [rotL] in [rotD], ki izvedeta eno levo
-- oziroma desno rotacijo na AVL drevesu.
rotD :: AVLDrevo a -> AVLDrevo a
rotD = undefined

rotL :: AVLDrevo a -> AVLDrevo a
rotL = undefined

-- Implementiraj funkcijo [uravnotezi], ki dano drevo uravnotezi. Predpostavi,
-- da je dano drevo "skoraj" uravnoteženo: dobili smo ga tako, da smo nekemu AVL
-- drevesu dodali ali odstranili eno vozlišče.
uravnotezi :: (Ord a) => AVLDrevo a -> AVLDrevo a
uravnotezi = undefined

-- Implementiraj funkcijo [dodaj], ki AVL drevesu doda element. Pri tem
-- upoštevaj, da bo morda potrebno drevo še uravnotežiti.
dodaj :: (Ord a) => AVLDrevo a -> a -> AVLDrevo a
dodaj = undefined

-- Ko je funkcija [dodaj] implementirana, lahko sestavimo še funkcijo [izSeznama],
-- ki iz seznama sestavi AVL drevo.
izSeznama :: (Ord a) => [a] -> AVLDrevo a
izSeznama = undefined

-- Implementiraj funkcijo [najboljLevi], ki vrne najbolj levi element
-- danega AVL drevesa.
najboljLevi :: (Ord a) => AVLDrevo a -> Maybe a
najboljLevi = undefined

-- Implementiraj funkcijo [odstrani], ki iz AVL drevesa odstrani element.
odstrani :: (Ord a, Eq a) => AVLDrevo a -> a -> AVLDrevo a
odstrani = undefined
