{-
 - Exercise set 3: Datatypes
 -}

-- Natural numbers
-- ===============

data Natural = Zero | Succ Natural deriving (Show)

-- 'add m n' returns the sum of natural numbers 'm' and 'n'
add :: Natural -> Natural -> Natural
add Zero n = n
add (Succ m) n = add m (Succ n)

-- 'multiply m n' returns the product of natural numbers 'm' and 'n'
multiply :: Natural -> Natural -> Natural
multiply Zero n = n
multiply (Succ m) n = add n (multiply m n)

-- 'toNatural n' converts an integer 'n' into a natural number
--
-- Example:
-- ghci> toNatural 0
-- Zero
-- ghci> toNatural 2
-- Succ (Succ Zero)
toNatural :: Integer -> Natural
toNatural 0 = Zero
toNatural n = Succ (toNatural (n-1))

-- 'fromNatural n' converts a natural number 'n' to an integer
--
-- Example:
-- ghci> fromNatural Zero
-- 0
-- ghci> fromNatural (Succ (Succ Zero))
-- 2
fromNatural :: Natural -> Integer
fromNatural Zero = 0
fromNatural (Succ m) = 1+ (fromNatural m)



-- Trees
-- =====

-- Here we define the recursive datatype Tree. We will add more functions that
-- work on trees, like the sumTree example, which calculates the sum of the
-- elements of a tree.

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show)

sumTree :: Num a => Tree a -> a
sumTree Leaf = 0
sumTree (Node x left right) = x + sumTree left + sumTree right

-- 'depth tr' returns the depth of a tree. Leaves have depth 0.
--
-- Example:
-- ghci> let d = Node 3 (Node 7 Leaf (Node 2 Leaf Leaf)) (Node 8 Leaf Leaf)
-- ghci> globina d
-- 3
depth :: Tree a -> Int
depth Leaf = 0
depth (Node _ l r) = max (depth l) (depth r)

-- 'numberOfElements tr', for 'tr' of type 'Tree alpha' computes the number of
-- alpha's ("elements") in tr.
--
-- Example:
-- ghci> let tr = Node 3 (Node 7 Leaf (Node 2 Leaf Leaf)) (Node 8 Leaf Leaf)
-- ghci> numberOfElements tr
-- 4
numberOfElements :: Tree a -> Int
numberOfElements Leaf = 0
numberOfElements (Node _ l r) = 1 + numberOfElements l + numberOfElements r

-- 'treeFlip tr' swaps the left and right subtrees of each node in 'tr'
--
-- Example:
-- ghci> let tr = Node 3 (Node 7 Leaf (Node 2 Leaf Leaf)) (Node 8 Leaf Leaf)
-- ghci> treeFlip tr
-- Node 3 (Node 8 Leaf Leaf) (Node 7 (Node 2 Leaf Leaf) Leaf)
treeFlip :: Tree a -> Tree a
treeFlip Leaf = Leaf
treeFlip (Node x l r) = Node x (treeFlip r) (treeFlip l)

-- 'leftMost tr' returns the left-most element in 'tr'.
-- Using the 'Maybe' type will allow you to return a sensible result in the
-- case where your tree is a Leaf.
-- Example:
-- ghci> let tr = Node 3 (Node 7 Leaf (Node 2 Leaf Leaf)) (Node 8 Leaf Leaf)
-- ghci> leftMost tr
-- 7
leftMost :: Tree a -> Maybe a
leftMost Leaf = Nothing
leftMost (Node x Leaf _) = Just x
leftMost (Node _ l _) = leftMost l

-- Complex Numbers
-- ==================

-- We have defined a datatype Complex, which represents complex numbers. We
-- will add some functions to work with complex numbers.

data Complex = Complex Double Double deriving (Show)

-- 're x' returns the real part of the complex number x.
re :: Complex -> Double
re (Complex x _) = x

-- 'im x' returns the imaginary part of the complex number x.
im :: Complex -> Double
im (Complex _ y) = y

-- 'conjugate x' returns the complex conjugate of x.
conjugate :: Complex -> Complex
conjugate (Complex x y) = (Complex x (-y))


-- Polynomials
-- ===========

-- We define a datatype Polynomial, which represents polynomials in one
-- variable over the field of rationals, with coefficients in increasing order.
-- We will add more functions to work with polynomials.

data Polynomial = Polynomial [Rational] deriving (Show)

-- Example: the polynomial x^1 + 0
p_x :: Polynomial
p_x = Polynomial [0, 1]


-- 'eval p x' evaluates the polynomial 'p' at point 'x'.
--
-- Example:
-- ghci> let p = Polynomial [2,0,-1]
-- ghci> eval p 2
-- -2
eval :: Polynomial -> Rational -> Rational
eval (Polynomial p) x =
  snd $ foldl (\ (pow, res) k -> (pow+1, res + k * (x ^ pow))) (0::Int, 0) p

-- 'derivative p' computes the first derivative of p.

derivative :: Polynomial -> Polynomial
derivative (Polynomial []) = Polynomial []
derivative (Polynomial (_ : p)) = Polynomial $ zipWith (*) p [1..]

-- 'integral p' computes the indefinite integral of p.
integral :: Polynomial -> Polynomial
integral (Polynomial []) = Polynomial []
integral (Polynomial p) = Polynomial $ 0 : (zipWith (/) p [1..])
