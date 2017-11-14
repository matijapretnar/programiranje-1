{-
 - Exercise 2.2: Functions and recursion
 -}

-- **
-- 'range a b' computes a list of numbers from a to b, including the boundaries.
-- It should be defined recursively.
--
-- Example:
-- ghci> range 5 10
-- [5,6,7,8,9,10]
-- ghci> range 10 5
-- []
range :: Integral a => a -> a -> [a]
range a b =
  if a <= b
  then a : (range (a + 1) b)
  else []

-- **
-- 'insert x lst i' inserts 'x' into the list 'lst' at index 'i'.
-- 'i' has to be a valid index.
-- It should be defined by recursion.
--
-- Example:
-- ghci> insert 7 [1,2,3,4,5] 2
-- [1,2,7,3,4,5]
insert :: Integral n => a -> [a] -> n -> [a]
insert x lst 0 = x : lst
insert _x [] _n = undefined
insert x (y : lst) i = y : (insert x lst (i - 1))

-- ***
-- 'couples lst' pairs each two consecutive elements in 'lst' into a couple.
-- If 'lst' is of odd length, the last element is omitted.
-- This function is to be defined by recursion.
--
-- Example:
-- ghci> couples ["Toni", "Majda", "Andrej", "Boris", "Petra"]
-- [("Toni","Majda"),("Andrej","Boris")]
couples :: [a] -> [(a, a)]
couples (x : y : lst) = (x, y) : couples lst
couples [_x] = []
couples [] = []

-- **
-- 'isNonDecreasing lst' checks that the elements of 'lst' are in non decreasing order.
--
-- Example:
-- ghci> isNonDecreasing [-1,2,5,5,5,7,7]
-- True
-- ghci> isNonDecreasing [-1,2,5,3,5,7,7]
-- False
isNonDecreasing :: Ord a => [a] -> Bool
isNonDecreasing (x : y : lst) = x <= y && isNonDecreasing (y : lst)
isNonDecreasing [_x] = True
isNonDecreasing [] = True

-- **
-- A Stirling number of the second kind (or Stirling partition number), denoted
-- by S(n, k) is the number of ways to partition a set of n objects into k
-- non-empty subsets.
-- It can be defined recursively as follows:
-- S(n + 1, k) = k · S(n, k) + S(n, k - 1) for k > 0
-- The base cases are:
-- S(n, 0) = S(0, n) = 0 for n > 0, and
-- S(0, 0) = 1
--
-- Example:
-- ghci> stirling2 5 2
-- 15
stirling2 :: Integral n => n -> n -> n
stirling2 0 0 = 1
stirling2 _ 0 = 0
stirling2 0 _ = 0
stirling2 s_n k = k * (stirling2 n k) + (stirling2 n (k - 1))
  where n = s_n - 1

-- ***
-- Write a function 'cantor n' that computes the n-th approximation of the
-- Cantor set as a string of length 3^n.
--
-- Example:
-- ghci> cantor 0
-- "*"
-- ghci> cantor 1
-- "* *"
-- ghci> cantor 2
-- "* *   * *"
cantor :: Int -> String
cantor 0 = "*"
cantor n = prev ++ (replicate (3 ^ (n - 1)) ' ') ++ prev
  where prev = cantor (n - 1)

-- **
-- 'myGcd n m' calculates the greatest common divisor (gcd) of m and n.
-- We assume the convention that gcd 0 0 = 0.
--
-- Example:
-- ghci> myGcd 50 70
-- 10
myGcd :: Integral a => a -> a -> a
myGcd n m
  | n == m    =  n
  | n >  m    =  myGcd (n - m) m
  | otherwise =  myGcd n (m - n)


-- ****
-- 'permutations lst' builds a list of all the permutations of 'lst'.
--
-- Example:
-- ghci> permutations [1,2,3]
-- [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
permutations :: [a] -> [[a]]
permutations []        = [[]]
permutations (h : lst) =
  let perms = permutations lst in
    let n = length lst in
      [ insert h perm i | i <- (range 0 n), perm <- perms ]


-- ***
-- We want to solve the puzzle of the Tower of Hanoi for 'n' discs on 3 rods,
-- assuming that initially all discs are on the first rod.
-- 'hanoi n a b' is defined only for n, a, b such that 1 ≤ a, b ≤ 3 ∧ a ≠ b.
-- It returns a list of couples representing the moves that get the 'n' discs
-- from rod 'a' to rod 'b'.
--
-- Example:
-- ghci> hanoi 3 1 3
-- [(1,3),(1,2),(3,2),(1,3),(2,1),(2,3),(1,3)]
-- ghci> hanoi 4 1 3
-- [(1,2),(1,3),(2,3),(1,2),(3,1),(3,2),(1,2),(1,3),(2,3),(2,1),(3,1),(2,3),(1,2),(1,3),(2,3)]
hanoi :: Int -> Int -> Int -> [(Int, Int)]
hanoi 0 _ _ = []
hanoi n a b =
  (hanoi (n - 1) a c) ++        -- move all but the last disk to the other rod
  [(a, b)] ++                   -- move the biggest disk to the actual destination
  (hanoi (n - 1) c b)           -- move the remaining disks to the destination
  where c = 6 - a - b           -- this is the other rod (ie the one that is
                                -- not 'a' or 'b')
