{-
 - Exercise 1: Introduction to Haskell
 -}

-- penultimateElement l returns the second-to-last element of the list l
penultimateElement :: [a] -> a
penultimateElement l = last $ init l

-- get k l returns the k-th element in the list l
-- Example:
-- ghci> get 2 [0,0,1,0,0,0]
-- 1
get :: Int -> [a] -> a
get k l = head $ drop k l

-- double l "doubles" the list l
-- Example:
-- ghci> double [1,2,3,3]
-- [1,1,2,2,3,3,3,3]
-- Hint: The concat function creates a list containing the elements of the lists in a list
double :: [a] -> [a]
double l = concat [[x,x] | x <- l]

-- divide k l divides the list l into a pair of a list of the first k elements
-- of l and the rest
-- Example:
-- ghci> divide 2 [1,1,1,2,2,2]
-- ([1,1],[1,2,2,2])
divide :: Int -> [a] -> ([a], [a])
divide k l = (take k l, drop k l)

-- delete k l returns the list l with the k-th element removed
-- Example:
-- ghci> delete 3 [0,0,0,1,0,0,0]
-- [0,0,0,0,0,0]
delete :: Int -> [a] -> [a]
delete k l = take k l ++ drop (k+1) l

-- slice i k l returns the sub-list of l from the i-th up to (excluding) the k-th element

-- Example:
-- ghci> slice 3 6 [0,0,0,1,2,3,0,0,0]
-- [1,2,3]
slice :: Int -> Int -> [a] -> [a]
slice i k l = drop i $ take k l

-- insert x k l inserts x at index k into l
-- Example:
-- ghci> insert 2 5 [0,0,0,0,0,0]
-- [0,0,0,0,0,2,0]
insert :: a -> Int -> [a] -> [a]
insert x k l = take k l ++ [x] ++ drop k l

-- rotate n l rotates l to the left by n places
-- Example:
-- ghci> rotate 2 [1,2,3,4,5]
-- [3,4,5,1,2]
rotate :: Int -> [a] -> [a]
rotate n l = drop n l ++ take n l

-- remove x l returns a l with *all* occurances of x removed
-- Example:
-- ghci> remove 'a' "abrakadabra"
-- "brkdbr"
remove :: Eq a => a -> [a] -> [a]
remove x l = [y | y <- l, y /= x]

-- isPalindrome is a predicate that checks if a list is a palindrome
-- Example:
-- ghci> isPalindrome [1,2,3,2,1]
-- True
-- ghci> isPalindrome [1,2,2,1]
-- True
-- ghci> isPalindrome [1,2,3]
-- False
isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = l == reverse l

-- pointwiseMax l1 l2 returns the list of maximum elements in l1 and l2 at each
-- index, stopping at the shorter list of l1 and l2.
-- Example:
-- ghci> pointwiseMax [1,10,5,6] [2,3,7,4,8]
-- [2,10,7,6]
pointwiseMax :: Ord a => [a] -> [a] -> [a]
pointwiseMax l1 l2 = [ max x y | (x,y) <- zip l1 l2 ]

-- secondLargest l returns the second largest element of l.
-- l has to contain at least two distinct elements!
-- Example:
-- ghci> secondLargest [1,10,5,6]
-- 6
secondLargest :: Ord a => [a] -> a
secondLargest l = maximum $ remove (maximum l) l
