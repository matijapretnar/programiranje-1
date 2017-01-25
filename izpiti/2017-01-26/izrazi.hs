data Izraz = Samo Integer | Plus Izraz Izraz | Krat Izraz Izraz deriving Show

izracunaj :: Izraz -> Integer
izracunaj (Samo x) = x
izracunaj (Plus x y) = (izracunaj x) + (izracunaj y)
izracunaj (Krat x y) = (izracunaj x) * (izracunaj y)

instance Num Izraz where
  x + y = Plus x y
  x * y = Krat x y
  fromInteger x = Samo x
  abs = undefined
  signum = undefined
  negate = undefined
