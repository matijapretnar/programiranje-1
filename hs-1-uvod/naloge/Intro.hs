{-
 - Exercise 1: Introduction to Haskell
 -}

-- penultimateElement l returns the second-to-last element of the list l
penultimateElement l = undefined

-- get k l returns the k-th element in the list l
-- Example:
-- ghci> get 2 [0,0,1,0,0,0]
-- 1
get k l = undefined

-- double l "doubles" the list l
-- Example:
-- ghci> double [1,2,3,3]
-- [1,1,2,2,3,3,3,3]
-- Hint: The concat function creates a list containing the elements of the lists in a list
double l = undefined

-- divide k l divides the list l into a pair of a list of the first k elements
-- of l and the rest
-- Example:
-- ghci> divide 2 [1,1,1,2,2,2]
-- ([1,1],[1,2,2,2])
divide k l = undefined

-- delete k l returns the list l with the k-th element removed
-- Example:
-- ghci> delete 3 [0,0,0,1,0,0,0]
-- [0,0,0,0,0,0]
delete k l = undefined

-- slice i k l returns the sub-list of l from the i-th up to (excluding) the k-th element

-- Example:
-- ghci> slice 3 6 [0,0,0,1,2,3,0,0,0]
-- [1,2,3]
slice i k l = undefined

-- insert x k l inserts x at index k into l
-- Example:
-- ghci> insert 2 5 [0,0,0,0,0,0]
-- [0,0,0,0,0,2,0]
insert x k l = undefined

-- rotate n l rotates l to the left by n places
-- Example:
-- ghci> rotate 2 [1,2,3,4,5]
-- [3,4,5,1,2]
rotate n l = undefined

-- remove x l returns a l with *all* occurances of x removed
-- Example:
-- ghci> remove 'a' "abrakadabra"
-- "brkdbr"
remove x l = undefined

-- isPalindrome is a predicate that checks if a list is a palindrome
-- Example:
-- ghci> isPalindrome [1,2,3,2,1]
-- True
-- ghci> isPalindrome [1,2,2,1]
-- True
-- ghci> isPalindrome [1,2,3]
-- False
isPalindrome l = undefined

-- pointwiseMax l1 l2 returns the list of maximum elements in l1 and l2 at each
-- index, stopping at the shorter list of l1 and l2.
-- Example:
-- ghci> pointwiseMax [1,10,5,6] [2,3,7,4,8]
-- [2,10,7,6]
pointwiseMax l1 l2 = undefined

-- secondLargest l returns the second largest element of l.
-- l has to contain at least two distinct elements!
-- Example:
-- ghci> secondLargest [1,10,5,6]
-- 6
secondLargest l = undefined
