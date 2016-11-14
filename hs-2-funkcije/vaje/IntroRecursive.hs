{-
  Exercise 2.1: Introduction to Haskell, recursively!

  Solve last week's exercises using recursion instead of built-in functions.

  Like last week, be careful to write the type signatures first.

  Watch out for hints!
  Hints may also apply to later constructions, keep them in mind.

  The stars indicate roughly the difficulty of the expected solution:
  * should be easy,
  ** is only slightly more complicated,
  *** means that you may have to pause and think for a bit,
  **** can be tricky; it's okay to get stuck on these, consider moving past
       them and returning to them at the end of the exercise set.
 -}

-- *
-- 'penultimateElement l' returns the second-to-last element of the list l
penultimateElement = undefined

-- **
-- 'append l1 l2' puts the elements of l1 in front of l2, like the (++) operator
append = undefined

-- **
-- 'get k l' returns the k-th element in the list l
-- Example:
-- ghci> get 2 [0,0,1,0,0,0]
-- 1
get = undefined

-- **
-- 'double l' "doubles" the list l
-- Hint: You may use 'append'.
-- Example:
-- ghci> double [1,2,3,3]
-- [1,1,2,2,3,3,3,3]
double = undefined

-- ***
-- 'divide k l' divides the list l into a pair of a list of the first k elements
-- of l and the rest
-- Example:
-- ghci> divide 2 [1,1,1,2,2,2]
-- ([1,1],[1,2,2,2])
divide = undefined

-- **
-- 'delete k l' returns the list l with the k-th element removed
-- Example:
-- ghci> delete 3 [0,0,0,1,0,0,0]
-- [0,0,0,0,0,0]
delete = undefined

-- ***
-- 'slice i k l' returns the sub-list of l from the i-th up to (excluding) the k-th element
-- Hint: This is a recursion followed by another recursion. Define a local
--       auxiliary function.
-- Example:
-- ghci> slice 3 6 [0,0,0,1,2,3,0,0,0]
-- [1,2,3]
slice = undefined

-- **
-- 'insert x k l' inserts x at index k into l
-- Example:
-- ghci> insert 2 5 [0,0,0,0,0,0]
-- [0,0,0,0,0,2,0]
insert = undefined

-- **
-- 'rotate n l' rotates l to the left by n places
-- Example:
-- ghci> rotate 2 [1,2,3,4,5]
-- [3,4,5,1,2]
rotate = undefined

-- **
-- 'remove x l' returns a l with *all* occurances of x removed
-- Example:
-- ghci> remove 'a' "abrakadabra"
-- "brkdbr"
remove = undefined

-- **
-- 'reverseLength lst' computes a couple of the reversed list of 'lst' and its length
-- Note: you should compute both results with one single recursion.
reverseLength = undefined

-- ***
-- 'isPalindrome lst' is a predicate that checks if 'lst' is a palindrome
-- Last week, you compared 'lst' to its reverse. This does some unneccessary
-- work. In fact, you can save half the work if you stop when you reach the
-- middle of the two lists.
-- Hint: use 'reverseLength' and 'floor'
-- Example:
-- ghci> isPalindrome [1,2,3,2,1]
-- True
-- ghci> isPalindrome [1,2,2,1]
-- True
-- ghci> isPalindrome [1,2,3]
-- False
isPalindrome = undefined

-- **
-- 'pointwiseMax l1 l2' returns the list of maximum elements in l1 and l2 at each
-- index, stopping at the shorter list of l1 and l2.
-- Example:
-- ghci> pointwiseMax [1,10,5,6] [2,3,7,4,8]
-- [2,10,7,6]
pointwiseMax = undefined

-- ****
-- 'secondLargest l' returns the second largest element of l.
-- l has to contain at least two distinct elements!
-- Hint: Use an auxiliary function for the recursion.
-- Hint2: The auxiliary function will look exactly like 'foldl'.
-- Example:
-- ghci> secondLargest [1,10,5,6]
-- 6
secondLargest = undefined

-- **
-- rewrite secondLargest without recursion but instead using 'foldl'
secondLargest' = undefined
