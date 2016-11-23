type Barva = String
type Kocka = (Int, Int, Barva)

posvetli :: Barva -> Barva
posvetli b = "svetlo " ++ b
