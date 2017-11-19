import Slovar

-- Tukaj bomo sestavili preprost telefonski imenik. To bo slovar, ki bo pri
-- vsakem imenu imel vrednost. Telefonske številke so lahko precej različne,
-- zato bomo za te vrednosti uporabili tip String.

type Imenik = Slovar String String

-- Uporabniku bodo na voljo naslednje operacije za delo z imenikom:
data Operacija = Dodaj | Poisci | Odstrani | IzpisiVse

-- Definirajmo še tip Ukaz, ki je lahko znana operacija, nekaj nepoznanega ali 
-- poseben ukaz Izhod.
data Ukaz = Op Operacija | NepoznanUkaz String | Izhod

-- Funkcija [preberiIme] prosi uporabnika za vnos imena in ga prebere iz ukazne vrstice.
preberiIme :: IO String
preberiIme = do
  putStr "Vnesi ime: "
  getLine

-- Implementiraj še funkcijo [preberiStevilko], ki prosi uporabnika za vnos telefonske
-- stevilke in jo prebere.
preberiStevilko :: IO String
preberiStevilko = undefined

-- Implementiraj funkcijo [preberiUkaz], ki prosi uporabnika, da vnese enega od
-- ukazov, ki so na voljo, in potem vrne [Ukaz], ki ga je uporabnik vnesel.
preberiUkaz :: IO Ukaz
preberiUkaz = undefined

-- [izvediOperacijo imenik op] izvede operacijo [op] na imeniku [imenik].
izvediOperacijo :: Imenik -> Operacija -> IO Imenik

izvediOperacijo imenik IzpisiVse = do
  putStrLn $ show imenik
  return imenik

izvediOperacijo imenik Poisci = undefined
izvediOperacijo imenik Dodaj = undefined
izvediOperacijo imenik Odstrani = undefined

-- [interakcijskaZanka imenik] prebere [Ukaz] in izvede naslednje: 
-- * če je ukaz operacija, jo izvede
-- * če je ukaz nepoznan, obvesti uporabnika o tem, da ukaza ne pozna
-- * če je ukaz [Izhod], se funkcija prekine, torej izvede se `return ()`.
interakcijskaZanka :: Imenik -> IO ()
interakcijskaZanka imenik = undefined

main :: IO ()
main = interakcijskaZanka prazen

-- Nazadnje ta program prevedi v strojno kodo in sestavi .exe datoteko.
