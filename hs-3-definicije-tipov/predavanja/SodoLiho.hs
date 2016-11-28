data Sodo = Nic
          | NaslednikL Liho
          deriving Show

data Liho = NaslednikS Sodo
          deriving Show

data MogoceInt = NiInta
               | JeInt Integer

data MogoceString = NiStringa
                  | JeString String

data Mogoce a = NiVrednosti
              | JeVrednost a

