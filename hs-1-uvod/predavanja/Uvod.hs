sestej x y = x + y

x +++ y = 5 * (x + y)

sestejVektorja a b =
  (fst a + fst b, snd a + snd b)

ucbenik [] naslov =
  naslov  
ucbenik [avtor] naslov =
  avtor ++ ": " ++ naslov
ucbenik (prviAvtor:_) naslov =
  prviAvtor ++ " in ostali: " ++ naslov

prvi' (x:_) = x

zmnozi :: Integer -> Integer -> Integer
zmnozi x y = x * y

