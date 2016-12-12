import qualified Slovar

type Imenik = Slovar.Slovar String Integer

-- Uporabniku bodo na voljo naslednje operacije za delo z imenikom:
data Operacija = Dodaj | Poisci

data Ukaz = Op Operacija | Izhod

-- Funkcija [preberiIme] prosi uporabnika za vnos imena in ga prebere iz ukazne vrstice.
preberiIme :: IO String
preberiIme = do
  putStr "Vnesi ime: "
  getLine

-- Implementiraj še funkcijo [preberiStevilko], ki prosi uporabnika za vnos telefonske
-- stevilke in jo prebere.
preberiStevilko :: IO Integer
preberiStevilko = undefined

-- Implementiraj funkcijo [preberiUkaz], ki prosi uporabnika, da vnese enega od
-- ukazov, ki so na voljo, in potem vrne [Ukaz], ki ga je uporabnik vnesel.
preberiUkaz :: IO Ukaz
preberiUkaz = do
    putStr "Kateri ukaz bi rad? (dodaj/poisci/izhod)"
    ukaz <- getLine
    case ukaz of
        "dodaj" -> return (Op Dodaj)
        "poisci" -> return (Op Poisci)
        "izhod" -> return Izhod

-- [izvediOperacijo imenik op] izvede operacijo [op] na imeniku [imenik].
izvediOperacijo :: Imenik -> Operacija -> IO Imenik
izvediOperacijo imenik Poisci = do
    putStrLn "Nic nisem nasel"
    putStrLn "Na vajah bos naredil to do konca"
    putStr "Ali si vesel?"
    _ <- getLine
    putStrLn "Me ne zanima"
    return imenik

izvediOperacijo imenik Dodaj = undefined

-- [interaktivnaZanka imenik] prebere [Ukaz] in izvede naslednje: 
-- * če je ukaz operacija, jo izvede
-- * če je ukaz [Izhod], se funkcija prekine, torej izvede se `return ()`.
interaktivnaZanka :: Imenik -> IO ()
interaktivnaZanka imenik = do
    ukaz <- preberiUkaz
    case ukaz of
        Op op -> do
            noviImenik <- izvediOperacijo imenik op
            interaktivnaZanka noviImenik
        Izhod -> return ()

main :: IO ()
main = interaktivnaZanka Slovar.prazen
