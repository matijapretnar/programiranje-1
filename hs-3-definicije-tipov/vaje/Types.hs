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

depth = undefined

-- 'numberOfElements tr', for 'tr' of type 'Tree alpha' computes the number of
-- alpha's ("elements") in tr.
--
-- Example:
-- ghci> let tr = Node 3 (Node 7 Leaf (Node 2 Leaf Leaf)) (Node 8 Leaf Leaf)
-- ghci> numberOfElements tr
-- 4

numberOfElements = undefined

-- 'flip tr' swaps the left and right subtrees of each node in 'tr'
--
-- Example:
-- ghci> let tr = Node 3 (Node 7 Leaf (Node 2 Leaf Leaf)) (Node 8 Leaf Leaf)
-- ghci> flip tr
-- Node 3 (Node 8 Leaf Leaf) (Node 7 (Node 2 Leaf Leaf) Leaf)

flip = undefined

-- 'leftMost tr' returns the left-most element in 'tr'.
-- Example:
-- ghci> let tr = Node 3 (Node 7 Leaf (Node 2 Leaf Leaf)) (Node 8 Leaf Leaf)
-- ghci> leftMost tr
-- 7

leftMost = undefined



-- Complex Numbers
-- ==================

-- We have defined a datatype Complex, which represents complex numbers. We
-- will add some functions to work with complex numbers.

data Complex = Complex Double Double

-- 're x' returns the real part of the complex number x.

re = undefined

-- 'im x' returns the imaginary part of the complex number x.

im = undefined

-- 'conjugate x' returns the complex conjugate of x.

conjugate = undefined

-- Make Complex a member of the type class Num.

instance Num Complex where
    (Complex x1 y1) + (Complex x2 y2) = undefined

-- Ensure that we can display complex numbers nicely (in the form of 3 + 5i)
-- bonus: what to do with 0?

instance Show Complex where


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
