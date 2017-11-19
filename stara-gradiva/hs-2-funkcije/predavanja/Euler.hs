-- V tej datoteki so rešitve prvih sedmih problemov na strani Project Euler.
-- Rešitve niti pod razno niso najbolj učinkovite,
-- kot se jih da napisati v Haskellu.

-- 'prastevila' je seznam vseh praštevil.
prastevila = reseto [2..]
  where
    -- Vsa praštevila dobimo tako, da na naravnih številih, večjih od 1
    -- uporabimo Eratostenovo rešeto. To storimo tako, da iz repa seznama
    -- pobrišemo vse člene, ki so deljivi z glavo seznama, nato pa ta postopek
    -- ponovimo na repu.
    reseto [] = []
    reseto (n:ns) = n : reseto [m | m <- ns, m `mod` n /= 0]

-- 'fib' je seznam vseh Fibonaccijevih števil.
fib = posploseniFib 0 1
  where
    -- 'posploseniFib a b' vrne seznam posplošenih Fibonaccijevih števil,
    -- ki se začnejo s členoma 'a' in 'b'
    posploseniFib a b = a : posploseniFib b (a + b)

-- 'euler1 n' vrne vsoto vseh večkratnikov števil 3 ali 5, ki so manjša od 'n'.
euler1 :: Integer -> Integer
euler1 n = sum [a | a <- [1..n - 1], a `mod` 3 == 0 || a `mod` 5 == 0]

-- 'euler2 n' vrne vsoto vseh sodih Fibonaccijevh števil, manjših ali enakih 'n'.
euler2 :: Integer -> Integer
euler2 n = sum $ filter even $ takeWhile (< n) $ fib
  where

-- 'euler3 n' vrne največji praštevilski delitelj števila 'n'.
euler3 :: Integer -> Integer
euler3 n = maximum $ filter deli $ takeWhile niPrevelik $ prastevila
  where
    deli p = n `mod` p == 0
    niPrevelik p = p^2 <= n

-- 'euler4 k' vrne največji produkt dveh 'k'-mestnih števil, ki je palindrom.
-- Tip prvega argumenta omejimo na 'Int', saj daljših števil ne potrebujemo.
euler4 :: Int -> Integer
euler4 k = maximum [m * n | m <- [najmanjse..najvecje], n <- [m..najvecje], jePalindrom (m * n)]
  where
    najmanjse = 10^(k - 1)
    najvecje = 10^k - 1
    jePalindrom n = let niz = show n in niz == reverse niz

-- 'euler5 n' vrne najmanjše število, ki je deljivo z vsemi števili od 1 do 'n'.
euler5 :: Integer -> Integer
euler5 n = foldr1 lcm [1..n]

-- 'euler6 n' vrne razliko med kvadratom vsote in vsoto kvadratov števil od 1 do 'n'.
euler6 :: Integer -> Integer
euler6 n = (sum [1..n])^2 - sum [a^2 | a <- [1..n]]

-- 'euler7 n' vrne 'n'-to praštevilo
euler7 :: Int -> Integer
euler7 k = prastevila !! (k - 1)
