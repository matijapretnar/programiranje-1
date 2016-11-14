{-
 - Vaja 2: Funkcije in rekurzija
 -}

-- Funkcija range naj dobi dva argumenta: a, b. Sestavi naj seznam števil od a do b.
-- Funkcija naj bo definirana REKURZIVNO.
-- 
-- Zgled:
-- ghci> range 5 10
-- [5,6,7,8,9,10]
range :: Integral a => a -> a -> [a]
range a b
	| a > b = []
	| otherwise = a : range (a+1) b

-- Napišite funkcijo vstavi, ko dobi tri argumente: x, l in i. Funkcija naj
-- v seznam l vstavi element i na i-to mesto. Funkcija naj bo definirana REKURZIVNO.
-- 
-- Zgled:
-- ghci> vstavi 7 [1,2,3,4,5] 2
-- [1,2,7,3,4,5]
vstavi :: a -> [a] -> Int -> [a]
vstavi x [] 0 = [x]
vstavi x [] _ = error "Seznam je prekratek!"
vstavi x l@(y:ys) i
	| i == 0 = x : l
	| otherwise = y : vstavi x ys (i-1)

-- Sestavite funkcijo poparckaj, ki elemente seznama poparčka. Če je seznam
-- lihe dolžine, zadnji element "zanemarimo". Funkcija naj bo definirana REKURZIVNO.
-- 
-- Zgled:
-- ghci> poparckaj ["Toni", "Majda", "Andrej", "Boris", "Petra"]
-- [("Toni","Majda"),("Andrej","Boris")]
poparckaj :: [a] -> [(a,a)]
poparckaj [] = []
poparckaj [_] = []
poparckaj (x:y:zs) = (x,y) : poparckaj zs

-- Sestavite funkcije jeNepadajoce, ki preveri, če so elementi seznama urejeni
-- nepadajoče.
-- 
-- Zgled:
-- ghci> jeNepadajoce [-1,2,5,5,5,7,7]
-- True
-- ghci> jeNepadajoce [-1,2,5,3,5,7,7]
-- False
jeNepadajoce :: (Ord a) => [a] -> Bool
jeNepadajoce [] = True
jeNepadajoce [_] = True
jeNepadajoce (x:y:zs) = (x <= y) && jeNepadajoce (y:zs)

-- Stirlingova števila druge vrste S(n, k) štejejo razbitja n elemente množice na k
-- nepraznih podmnožic. Za  Strilingova števila druge vrste velja rekurzivna zveza:
-- 
-- S(n + 1, k) = k · S(n, k) + S(n, k - 1) za k > 0.
-- 
-- Začetni pogoji so S(n, 0) = S(0, n) = 0 za n > 0 in S(0, 0) = 1.
-- 
-- Zgled:
-- ghci> stirling2 5 2
-- 15
stirling2 :: Int -> Int -> Int
stirling2 0 0 = 1
stirling2 n 0 = 0
stirling2 0 k = 0
stirling2 n k = k * stirling2 (n-1) k + stirling2 (n-1) (k-1)

-- Sestavite funkcijo cantor, ki kot argument dobi nenegativno celo
-- število in naj vrne niz dolžine 3^n z n-tim približkom Cantorjeve množice.
-- 
-- Zgled:
-- ghci> cantor 0
-- "*"
-- ghci> cantor 1
-- "* *"
-- ghci> cantor 2
-- "* *   * *"
cantor :: Int -> String
cantor 0 = "*"
cantor n = cPrevious ++ replicate power ' ' ++ cPrevious
	where
		cPrevious = cantor (n-1)
		power = 3^(n-1)

-- Funkcija gcd' naj izračuna največji skupni delitelj dveh celih števil.
-- Če sta oba argumenta 0, naj funkcija vrne 0.
-- 
-- Zgled:
-- ghci> gcd' 50 70 
-- 10
gcd' :: Int -> Int -> Int
gcd' n 0 = n
gcd' n m
	| n < m = gcd' m n
	| otherwise = gcd' (n-m) m

-- Sestavite funkcijo permutacije, ki kot argument dobi seznam in vrne seznam
-- vseh permutacija tega seznama.
-- 
-- Zgled:
-- ghci> permutacije [1,2,3]
-- [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
permutacije :: [a] -> [[a]]
permutacije [] = [[]]
permutacije (x:xs) = [vstavi x p i | p <- permutacije xs, i <- [0..(length xs)]]

-- Funkcija hanoi naj kot argument dobi tri cela števila: n, a, b, kjer je 1 <= a, b, <= 3 in
-- a /= b. Vrne naj seznam parov, ki predstavljajo premike, s katerimi prestavimo n diskov
-- spravimo s palice a na palico b.
-- 
-- Zgled: 
-- ghci> hanoi 3 1 3
-- [(1,3),(1,2),(3,2),(1,3),(2,1),(2,3),(1,3)]
-- ghci> hanoi 4 1 3
-- [(1,2),(1,3),(2,3),(1,2),(3,1),(3,2),(1,2),(1,3),(2,3),(2,1),(3,1),(2,3),(1,2),(1,3),(2,3)]
hanoi :: Int -> Int -> Int -> [(Int, Int)]
hanoi n a b
	| n == 0 = []
	| otherwise = hanoi (n-1) a c ++ [(a,b)] ++ hanoi (n-1) c b
	where c = head $ filter (\x -> x/=a && x/=b) $ [1,2,3]

