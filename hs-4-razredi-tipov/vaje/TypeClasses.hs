{-# LANGUAGE MultiParamTypeClasses #-}

-- Complex Numbers
-- ===============
--
-- Show that the types Natural, Complex and Polynomial of the previous lesson
-- belong to the Num class:

data Natural = Zero | Succ Natural deriving (Show)

data Complex = Complex Double Double deriving (Show)

data Polynomial = Polynomial [Rational] deriving (Show)

instance Num Natural where
  Zero + n = n
  (Succ m) + n = Succ (m + n)
  Zero * n = n
  (Succ m) * n = n + (m * n)
  fromInteger 0 = Zero
  fromInteger n = Succ (fromInteger (n-1))
  abs n = n
  signum Zero = 0
  signum (Succ _) = 1
  negate = error "Natural: operation negate not available"

instance Num Complex where
    fromInteger n = Complex (fromInteger n) 0
    (Complex x1 y1) + (Complex x2 y2) = Complex (x1 + x2) (y1 + y2)
    negate (Complex x y) = Complex (-x) (-y)
    (Complex x1 y1) * (Complex x2 y2) = Complex (x1 * x2 - y1 * y2) (y1 * x2 + x1 * y2)
    abs (Complex x y) = Complex (sqrt (x**2 + y**2)) 0
    signum (Complex 0 0) = Complex 0 0
    signum (Complex x y) = Complex (x / norm) (y / norm)
      where norm = sqrt (x**2 + y**2)


scalarMult :: Rational -> [Rational] -> [Rational]
scalarMult x ys = map ((*) x) ys
addCoefs :: [Rational] -> [Rational] -> [Rational]
addCoefs = zipWith (+)

multCoefs :: [Rational] -> [Rational] -> [Rational]
multCoefs _ [] = []
multCoefs [] _ = []
multCoefs (x:xs) (y:ys) = (x * y) : scalarMult x ys `addCoefs` (xs `multCoefs` (y:ys))

instance Num Polynomial where
    negate (Polynomial coefs) = Polynomial $ map ((*) (-1)) coefs
    fromInteger x = Polynomial $ [fromInteger x]
    abs    = error "Polynomial: operation abs does not make sense"
    signum = error "Polynomial: operation signum does not make sense"
    (Polynomial coefs1) + (Polynomial coefs2) = Polynomial $ zipWith (+) coefs1 coefs2
    (Polynomial coefs1) * (Polynomial coefs2) = Polynomial $ multCoefs coefs1 coefs2


-- Algebraic structures
-- ====================

-- The [Semigroup] class can be defined like this:

class  Semigroup a  where
    (***) :: a -> a -> a


-- Define the following classes (extensions):
-- + SemigroupWithUnit (with a special element "unit")

class  Semigroup a => SemigroupWithUnit a  where
  unit :: a

-- + Group (with an "inv" function)

class  SemigroupWithUnit a => Group a  where
  inv :: a -> a

-- + Ring

-- kind of a bad example because we need overlapping instances over the base

-- class  (Group a, SemigroupWithUnit a) => Ring a  where
--   add :: a -> a -> a
class  Ring a  where
  add :: a -> a -> a
  mult :: a -> a -> a
  add_inv :: a -> a
  add_unit  :: a
  mult_unit :: a


-- Show that the integers belong to the Ring class

instance Ring Integer where
  add = (Prelude.+)
  mult = (Prelude.*)
  add_inv x = -x
  add_unit = 0
  mult_unit = 1

-- Show that Bool belongs to Group

instance  Semigroup Bool where
  True *** False = True
  False *** True = True
  _ *** _ = False

instance  SemigroupWithUnit Bool where
  unit = False

instance  Group Bool  where
  inv = id

-- Show that the type [Z_2] as defined below belongs to Group

data Z_2 =  Zero_2 | One_2 deriving (Show)

instance  Semigroup Z_2 where
  Zero_2 *** One_2 = One_2
  One_2 *** Zero_2 = One_2
  _ *** _ = Zero_2

instance  SemigroupWithUnit Z_2 where
  unit = Zero_2

instance  Group Z_2  where
  inv = id


-- Show that the cartesian product type of two types in the Group class belongs
-- to the Group class

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  (***) (a, b) (a', b') = (a *** a', b *** b')

instance (SemigroupWithUnit a, SemigroupWithUnit b) => SemigroupWithUnit (a, b) where
  unit = (unit, unit)

instance (Group a, Group b) => Group (a, b) where
  inv (a, b) = (inv a, inv b)

-- Let types [a] and [b] belong to the Group class. To say that [a] and [b] are
-- isomorphic, we can define the Isomorphism class:

class  Isomorphism a b  where
    towards :: a -> b
    backwards :: b -> a

-- Show that [Bool] and [Z_2] are isomorphic as groups

instance Isomorphism Bool Z_2 where
  towards False = Zero_2
  towards True = One_2
  backwards One_2 = True
  backwards Zero_2 = False


-- Distributions
-- =============

-- We define the data type of distributions:

data Distribution a = Distribution [(a, Rational)] deriving Show

-- Two simple examples:

data Coin = Heads | Tails
coin :: Distribution Coin
coin = Distribution [(Heads, 1/2), (Tails, 1/2)]

die :: Distribution Int
die = Distribution [(1, 1/6), (2, 1/6), (3, 1/6), (4, 1/6), (5, 1/6), (6, 1/6)]

-- [isDistribution d] checks that d really is a distribution, ie that the
-- probabilities sum up to 1.
isDistribution :: Distribution a -> Bool
isDistribution (Distribution d) = foldl (\s e -> s + snd e) 0 d == 1

-- [cleanDistribution d] merges together repeated events
cleanDistribution :: Eq a => Distribution a -> Distribution a
cleanDistribution (Distribution d) =
  Distribution d_clean
  where d_clean = foldl merger [] d
          where merger =
                  \d' el@(ev,p) ->
                    case lookup ev d' of
                      Nothing -> el : d'
                      Just p' -> (ev, p+p') : d'

-- [mostLikely d] returns the most likely event in [d], and the last one listed
-- if it is not unique.
mostLikely :: Distribution a -> a
mostLikely (Distribution []) = undefined
mostLikely (Distribution (base : d)) =
  fst $ foldl chooser base d
  where
    chooser (el@(_,p)) el'@(_,p') = if p' >= p then el' else el

-- [uniform d] returns a uniform distribution of the events in d
uniform :: Distribution a -> Distribution a
uniform (Distribution d) =
  Distribution $ map (\(ev,_) -> (ev,p)) d
  where p = 1 / (toRational $ length d)

-- [expectation d] returns the expected value of d
expectation :: Distribution Rational -> Rational
expectation (Distribution d) =
  foldl (\ex (ev,p) -> ex + ev * p) 0 d

-- [weightedSum p d1 d2] computes a distribution obtained by merging the
-- (compatible) distributions [d1] and [d2], scaling events in [d1] by
-- the weight [p] and events in [d2] by (1-p).
weightedSum :: Rational -> Distribution a -> Distribution a -> Distribution a
weightedSum p (Distribution d1) (Distribution d2) =
  Distribution $ (map (scale p) d1) ++ (map (scale (1-p)) d2)
  where scale weight (ev,p') = (ev,weight*p')

-- Show that the type constructor [Distribution] belongs to the [Functor] type
-- class.

instance  Functor Distribution  where
    fmap f (Distribution d) = Distribution $ map (\(ev,p) -> (f ev, p)) d



-- Moving in space
-- ===============
type Position = (Int, Int, Int)

-- Define a [Point] type, which should have the parameters (name, X-coordinate,
-- Y-coordinate, Z-coordinate). Implement a [show] function for [Point]
data Point = Point (String, Position) deriving Show


-- The point will move through space. Before we can move, we must first be
-- somewhere. Define a type class [Positioned] with a [currentPosition] value
-- and show that [Point] is a member of [Positioned]

class Positioned a where
  currentPosition :: a -> Position

instance Positioned Point where
  currentPosition (Point (_, pos)) = pos


-- Define the [Movable] type class that implements [setNewLocation] and make
-- [Point] a member of it.
class (Positioned a)  => Movable a where
  setNewLocation :: a -> Position -> a

instance Movable Point where
  setNewLocation (Point (n, _)) pos = Point (n, pos)

-- For variables whose type belongs to the class [Movable], define the function
-- [moveFor] that moves the variable for a specific vector.

moveFor :: (Movable a) => a -> (Int, Int, Int) -> a
moveFor t (x,y,z) = setNewLocation t (x + x0, y + y0, z + z0)
    where
        (x0, y0, z0) = currentPosition t

-- In some species of spiders, the female is known to eat the male after
-- mating. The females which eat the males lay more eggs, which produce
-- stronger and bigger embryos. One tentative explanation is that such females
-- are more aggressive and therefore better hunters. Another theory says that
-- males simply are an excellent source of essential nutrients.
--
-- Imagine a female spider at location (0,0,0). The female wants to eat the
-- male which is at coordinates (3,3,3). Assume that the spiders' movements are
-- confined to a 10x10x10 cube (ie each coordinate is always between 0 and 9).
--
-- We will make both spiders move around. First moves the female, which tries
-- to get as close as possible to the male's position. Then moves the male,
-- which wants to maximize the distance from the female.

-- Write a [move f m] function that maps a position of a female [f] and of a
-- male [m] to a pair of moves.

moveFemale :: Int -> Int -> Int
moveFemale u v
    | u == v = 0
    | u > v = -1
    | u < v = 1

moveMale :: Int -> Int -> Int
moveMale u v
    | v == 9 = 0
    | v == 0 = 0
    | u < v = 1
    | v < u = -1

move :: Point -> Point -> (Point, Point)
move female male = (moveFor female deltaFemale, moveFor male deltaMale)
    where
        deltaFemale = (moveFemale x0 x1, moveFemale y0 y1, moveFemale z0 z1)
        deltaMale = (moveMale x0 x1, moveMale y0 y1, moveMale z0 z1)
        (x0, y0, z0) = currentPosition female
        (x1, y1, z1) = currentPosition male

x = Point ("female", (0,0,0))
y = Point ("male", (3,3,2))

-- Finally, write [simulate], which simulates the behaviour of a female and a male.

simulate :: Point -> Point -> [(Point, Point)] 
simulate female male
    | (maximum $ map abs [x0 - x1, y0 - y1, z0 - z1]) <= 1 = [(female, male)]
    | otherwise = (female, male) : (simulate newFemale newMale)
    where
        (x0, y0, z0) = currentPosition female
        (x1, y1, z1) = currentPosition male
        (newFemale, newMale) = move female male
