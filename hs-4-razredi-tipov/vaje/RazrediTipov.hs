{-# LANGUAGE MultiParamTypeClasses #-}

-- Algebraične stukture
-- ====================

-- Razred Polgrupa lahko definiramo na sledeč način:

class  Polgrupa a  where
    (***) :: a -> a -> a

-- Definirajte še naslednje razrede:
--
-- PolgrupaZEnoto (s posebno vrednostjo "enota")
--
-- Grupa (s posebno vrednostjo "inv")
--
-- Kolobar



-- Pokažite, da cela števila pripadajo razredu Kolobar. 

-- Pokažite, da tip Bool pripada razredu Grupa.

-- Pokažite, da podatkovni tip Z_2 (definiran spodaj) pripada razredu Grupa.

data Z_2 =  Nic | Ena deriving (Show)

-- Pokažite, da kartezični produkt tipov v razredu Grupa pripada razredu Grupa.



-- Naj tipa a in b pripadata razredu Grupa. Če želimo povedati, da sta grupi a
-- in b izomorfni, lahko uporabimo razred Izomorfno:

class  Izomorfno a b  where
    naprej :: a -> b
    nazaj :: b -> a

-- Pokažite, da je grupa Bool izomorfna grupi Z_2.


-- Premikanje v prostoru
-- =====================

-- Definirajte podatkovni tip Tocka, ki naj vsebuje parametre {ime, koordinataX,
-- koordinataY, koordinataZ}. Implementirajte funkcijo show za tip Tocka. 


-- Točka se bo premikala v prostoru. Da se premikanje lahko začne, moramo točko
-- najprej poiskati. V ta namen definirajte razred tipov Pozicioniran (z
-- vrednostjo pridobiLokacijo) in pokažite, da Tocka pripada tipu Pozicioniran.


-- Definirajte razred tipov Premakljiv (z vrednostjo nastaviNovoLokacijo) in
-- pokažite, da Tocka pripada tipu Premakljiv.


-- Za spremenljivke, katerih tip pripada razredu Premakljiv, definirajte
-- funkcijo premakniZa, ki spremenljivko premakne za določen vektor. Ta funkcija
-- bo tako delovala tudi za poljubno točko!


-- Znano je, da samice mnogih vrst pajkov po parjenju pojejo samca. Samice
-- pajkov, ki pojedo samca, bodo izlegle več jajčec, nov zarodek pa bo močnejši
-- in večji. Ena izmed teorij, zakaj je temu tako, se veže na to, da so takšne
-- samice bolj agresivne in so zato tudi boljše pri lovu. Druga teorija pa
-- ugotavlja, da so samci odličen vir pomembnih hranljivih snovi.
--
-- Predstavljaj si samico, locirano v koordinati (0,0,0). Samica želi pojesti
-- samca, ki je trenutno lociran v koordinati (3,3,3). Predpostavimo, da sta
-- pajka omejena na gibanje v kocki velikosti 10x10x10 (torej je vsaka
-- koordinata vedno med 0 in 9).
--
-- Samica in samec se premikata po potezah.  Najprej se premakne samica, ki se
-- želi čim bolj približati samcu.  Potem se premakne samec, ki se želi čim bolj
-- oddaljiti od samice.
--
-- Sestavite funkcijo poteza, ki sprejme podatke o samici in samcu ter vrne
-- vsakega od obeh pajkov ustrezno premakne.
--
-- Nazadnje sestavite funkcijo simuliraj, ki simulira obnašanje samice in samca.

