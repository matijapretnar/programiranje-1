{-
 - Vaja 2: Funkcije in rekurzija
 -}

-- Funkcija range naj dobi dva argumenta: a, b. Sestavi naj seznam števil od a do b.
-- Funkcija naj bo definirana REKURZIVNO.
-- 
-- Zgled:
-- ghci> range 5 10
-- [5,6,7,8,9,10]
range = undefined

-- Napišite funkcijo vstavi, ko dobi tri argumente: x, l in i. Funkcija naj
-- v seznam l vstavi element i na i-to mesto. Funkcija naj bo definirana REKURZIVNO.
-- 
-- Zgled:
-- ghci> vstavi 7 [1,2,3,4,5] 2
-- [1,2,7,3,4,5]
vstavi = undefined

-- Sestavite funkcijo poparckaj, ki elemente seznama poparčka. Če je seznam
-- lihe dolžine, zadnji element "zanemarimo". Funkcija naj bo definirana REKURZIVNO.
-- 
-- Zgled:
-- ghci> poparckaj ["Toni", "Majda", "Andrej", "Boris", "Petra"]
-- [("Toni","Majda"),("Andrej","Boris")]
poparckaj = undefined

-- Sestavite funkcije jeNepadajoce, ki preveri, če so elementi seznama urejeni
-- nepadajoče.
-- 
-- Zgled:
-- ghci> jeNepadajoce [-1,2,5,5,5,7,7]
-- True
-- ghci> jeNepadajoce [-1,2,5,3,5,7,7]
-- False
jeNepadajoce = undefined

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
stirling2 = undefined

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
cantor = undefined

-- Funkcija gcd' naj izračuna največji skupni delitelj dveh celih števil.
-- Če sta oba argumenta 0, naj funkcija vrne 0.
-- 
-- Zgled:
-- ghci> gcd' 50 70 
-- 10
gcd' = undefined

-- Sestavite funkcijo permutacije, ki kot argument dobi seznam in vrne seznam
-- vseh permutacija tega seznama.
-- 
-- Zgled:
-- ghci> permutacije [1,2,3]
-- [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
permutacije = undefined

-- INSERT LAST WEEKS EXERCISES AS RECURSION


-- Funkcija hanoi naj kot argument dobi tri cela števila: n, a, b, kjer je 1 <= a, b, <= 3 in
-- a /= b. Vrne naj seznam parov, ki predstavljajo premike, s katerimi prestavimo n diskov
-- spravimo s palice a na palico b.
-- 
-- Zgled: 
-- ghci> hanoi 3 1 3
-- [(1,3),(1,2),(3,2),(1,3),(2,1),(2,3),(1,3)]
-- ghci> hanoi 4 1 3
-- [(1,2),(1,3),(2,3),(1,2),(3,1),(3,2),(1,2),(1,3),(2,3),(2,1),(3,1),(2,3),(1,2),(1,3),(2,3)]
hanoi = undefined
