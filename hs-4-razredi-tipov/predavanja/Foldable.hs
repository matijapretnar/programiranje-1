data Drevo a
  = P
  | S (Drevo a) a (Drevo a)
  deriving Show

instance  Foldable Drevo  where
    foldr f z P = z
    foldr f z (S l x d) =
      foldr f (f x (foldr f z d)) l
