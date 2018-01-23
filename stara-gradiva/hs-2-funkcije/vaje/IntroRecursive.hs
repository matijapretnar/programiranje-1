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
penultimateElement :: [a] -> a
penultimateElement [] = undefined
penultimateElement [_x] = undefined
penultimateElement ([x, _y]) = x
penultimateElement (_h : l) = penultimateElement l

-- **
-- 'append l1 l2' puts the elements of l1 in front of l2, like the (++) operator
append :: [a] -> [a] -> [a]
append [] l2 = l2
append (h : l1) l2 = h : (append l1 l2)

-- **
-- 'get k l' returns the k-th element in the list l
-- Example:
-- ghci> get 2 [0,0,1,0,0,0]
-- 1
get :: Integral n => n -> [a] -> a
get _n [] = undefined
get 0 (h : _lst) = h
get n (_h : lst) = get (n - 1) lst

-- **
-- 'double l' "doubles" the list l
-- Hint: You may use 'append'.
-- Example:
-- ghci> double [1,2,3,3]
-- [1,1,2,2,3,3,3,3]
double :: [a] -> [a]
double [] = []
double (x : xs) = x : x : double xs

-- ***
-- 'divide k l' divides the list l into a pair of a list of the first k elements
-- of l and the rest
-- Example:
-- ghci> divide 2 [1,1,1,2,2,2]
-- ([1,1],[1,2,2,2])
divide :: Integral n => n -> [a] -> ([a], [a])
divide 0 l = ([], l)
divide _n [] = undefined
divide n (h : l) =
  let (l1, l2) = divide (n - 1) l in
    (h : l1, l2)

-- **
-- 'delete k l' returns the list l with the k-th element removed
-- Example:
-- ghci> delete 3 [0,0,0,1,0,0,0]
-- [0,0,0,0,0,0]
delete :: Integral n => n -> [a] -> [a]
delete _n [] = undefined
delete 0 (_h : l) = l
delete n (h : l) = h : (delete (n - 1) l)

-- ***
-- 'slice i k l' returns the sub-list of l from the i-th up to (excluding) the k-th element
-- Hint: This is a recursion followed by another recursion. Define a local
--       auxiliary function.
-- Example:
-- ghci> slice 3 6 [0,0,0,1,2,3,0,0,0]
-- [1,2,3]
slice :: Integral n => n -> n -> [a] -> [a]
slice 0 k lst = take' k lst
  where take' 0 _l = []
        take' _n [] = undefined
        take' k' (h : l) = h : (take' (k' - 1) l)
slice _i _k [] = undefined
slice n k (_h : l) = slice (n - 1) (k - 1) l

-- **
-- 'insert x k l' inserts x at index k into l
-- Example:
-- ghci> insert 2 5 [0,0,0,0,0,0]
-- [0,0,0,0,0,2,0]
insert :: Integral n => a -> n -> [a] -> [a]
insert x 0 l = x : l
insert _x _n [] = undefined
insert x n (h : l) = h : (insert x (n - 1) l)

-- **
-- 'rotate n l' rotates l to the left by n places
-- Example:
-- ghci> rotate 2 [1,2,3,4,5]
-- [3,4,5,1,2]
rotate :: Integral n => n -> [a] -> [a]
rotate _n [] = []
rotate 0 l = l
rotate n (h : l) = rotate (n - 1) (l `append` [h])

-- **
-- 'remove x l' returns a l with *all* occurances of x removed
-- Example:
-- ghci> remove 'a' "abrakadabra"
-- "brkdbr"
remove :: Eq a => a -> [a] -> [a]
remove _x [] = []
remove x (h : l)
  | x == h    = rest
  | otherwise = h : rest
  where rest = remove x l

-- **
-- 'reverseLength lst' computes a couple of the reversed list of 'lst' and its length
-- Note: you should compute both results with one single recursion.
reverseLength :: [a] -> ([a], Double)
reverseLength [] = ([], 0)
reverseLength (h : l) =
  let (l', n) = reverseLength l in
    (append l' [h], n + 1)

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
isPalindrome :: Eq a => [a] -> Bool
isPalindrome l =
  let (rev, len) = reverseLength l in
    let middle = floor $ len / 2 in
      equalPrefix l rev middle
  where
    equalPrefix :: Eq a => [a] -> [a] -> Integer -> Bool
    equalPrefix _l' _rev 0 = True
    equalPrefix [] _rev _n = undefined
    equalPrefix _l' [] _n = undefined
    equalPrefix (h : l') (r : rev) n =
      h == r && equalPrefix l' rev (n - 1)

-- **
-- 'pointwiseMax l1 l2' returns the list of maximum elements in l1 and l2 at each
-- index, stopping at the shorter list of l1 and l2.
-- Example:
-- ghci> pointwiseMax [1,10,5,6] [2,3,7,4,8]
-- [2,10,7,6]
pointwiseMax :: Ord a => [a] -> [a] -> [a]
pointwiseMax [] _l2 = []
pointwiseMax _l1 [] = []
pointwiseMax (h1 : l1) (h2 : l2)
  | h1 > h2   = h1 : rest
  | otherwise = h2 : rest
  where rest = pointwiseMax l1 l2

-- ****
-- 'secondLargest l' returns the second largest element of l.
-- l has to contain at least two distinct elements!
-- Hint: Use an auxiliary function for the recursion.
-- Hint2: The auxiliary function will look exactly like 'foldl'.
-- Example:
-- ghci> secondLargest [1,10,5,6]
-- 6
secondLargest :: Ord a => [a] -> a
secondLargest [] = undefined
secondLargest [_x] = undefined
secondLargest (x : y : l) =
   let (lar_base, sec_base) = if x > y then (x, y) else (y, x) in
      fold' lar_base sec_base l
    where fold' _lar sec [] = sec
          fold' lar sec (h : l') =
            let (lar', sec') =
                  if h > lar
                  then (h, lar)
                  else if h > sec
                       then (lar, h)
                       else (lar, sec)
            in
              fold' lar' sec' l'

-- **
-- rewrite secondLargest without recursion but instead using 'foldl'
secondLargest' :: Ord a => [a] -> a
secondLargest' [] = undefined
secondLargest' [_x] = undefined
secondLargest' (x : y : l) =
   let base = if x > y then (x, y) else (y, x) in
    snd $ foldl
   (\ (lar, sec) h ->
       if h > lar then (h, lar) else if h > sec then (lar, h) else (lar, sec)) base l
