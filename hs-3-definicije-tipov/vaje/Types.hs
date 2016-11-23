{-
 - Exercise set 3: Datatypes
 -}

-- Trees
-- =====

-- Here we define the recursive datatype Tree. We will add more functions that
-- work on trees, like the sumTree example, which calculates the sum of the
-- elements of a tree.

data Tree a = Leaf | Node a (Tree a) (Tree a)

sumTree :: Num a => Tree a -> a
sumTree Leaf = 0
sumTree (Node x left right) = x + sumTree left + sumTree right

-- The following code allows us to display trees.

instance (Show a) => Show (Tree a) where
    show Leaf = "Leaf"
    show (Node x Leaf Leaf) = "Node " ++ show x ++ " Leaf Leaf"
    show (Node x Leaf right) = "Node " ++ show x ++ " Leaf (" ++ show right ++ ")"
    show (Node x left Leaf) = "Node " ++ show x ++ " (" ++ show left ++ ") Leaf"
    show (Node x left right) = "Node " ++ show x ++ " (" ++ show left ++ ") (" ++ show right ++ ")"

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

data Complex = Complex Double Double

-- 're x' returns the real part of the complex number x.
re :: Complex -> Double
re (Complex x _) = x

-- 'im x' returns the imaginary part of the complex number x.
im :: Complex -> Double
im (Complex _ y) = y

-- 'conjugate x' returns the complex conjugate of x.
conjugate :: Complex -> Complex
conjugate (Complex x y) = (Complex x (-y))

-- Make Complex a member of the type class Num.

instance Num Complex where
    fromInteger n = Complex (fromInteger n) 0
    (Complex x1 y1) + (Complex x2 y2) = Complex (x1 + x2) (y1 + y2)
    negate (Complex x y) = Complex (-x) (-y)
    (Complex x1 y1) * (Complex x2 y2) = Complex (x1 * x2 - y1 * y2) (y1 * x2 + x1 * y2)
    abs (Complex x y) = Complex (sqrt (x**2 + y**2)) 0
    signum (Complex 0 0) = Complex 0 0
    signum (Complex x y) = Complex (x / norm) (y / norm)
      where norm = sqrt (x**2 + y**2)

-- Ensure that we can display complex numbers nicely (in the form of 3 + 5i)
-- bonus: what to do with 0?

instance Show Complex where
    show (Complex 0 0) = "0"
    show (Complex x 0) = show x
    show (Complex 0 y) = show y ++ "i"
    show (Complex x y)
        | y < 0 = show x ++ " - " ++ show (-y) ++ "i"
        | y > 0 = show x ++ " + " ++ show y ++ "i"

-- Polynomials
-- ===========

-- We define a datatype Polynomial, which represents polynomials in one
-- variable over the ring of integers, with coefficients in increasing order.
-- We will add more functions to work with polynomials.

data Polynomial = Polynomial [Integer]

-- Example: the polynomial x^1 + 0
p_x :: Polynomial
p_x = Polynomial [0, 1]


-- 'eval p x' evaluates the polynomial 'p' at point 'x'.
--
-- Example:
-- ghci> let p = Polynomial [2,0,-1]
-- ghci> eval p 2
-- -2
eval :: Polynomial -> 
eval = undefined

-- 'derivative p' computes the first derivative of p.

derivative = undefined

-- 'integral p' computes the indefinite integral of p.

integral = undefined

-- Make the Polynomial datatype an instance of the Num typeclass.

instance Num Polynomial where
    signum = error "Polynomial: The operation \"signum\" does not make sense"

-- Ensure that we can display polynomials.

instance Show Polynomial where
