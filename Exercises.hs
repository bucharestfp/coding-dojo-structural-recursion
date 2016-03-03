import Prelude hiding ((++), (!!), all, any, concat, drop, filter, find, foldl, foldr, head, length, map, reverse, tail, take, zip)

--
-- ghci> :i []
-- data [] a = [] | a : [a]  -- Defined in ‘GHC.Types’
--


-- Alternative definition of linked lists. This is what's called an
-- Algebraic Data Type, or ADT for short.
data List a
  = Nil
  | Cons a (List a)


--
-- Here's how to approach these exercises:
--
-- 1. Define the function's signature based on the given examples.
-- 2. Decompose the appropriate parameter using pattern matching:
--    2.1. handle the base case, i.e., the empty list `[]`
--    2.2. handle the recursive case, i.e., `:`
-- 3. Rejoice!
--

--
-- Extract the head of a list, if any. Use `error` when it's missing.
--
-- ghci> head []
-- *** Exception: Prelude.head: empty list
--
-- ghci> head [1,2,3]
-- 1
--
head :: [a] -> a
head list =
  case list of
    [] -> error "empty list"
    x : xs -> x


-- data Maybe a =
--     Nothing
--   | Just a

--
-- Extract the head of the list, if any. Use Maybe to convey optionality.
--
-- ghci> headMaybe []
-- Nothing
--
-- ghci> headMaybe [1,2,3]
-- Just 1
--
headMaybe :: [a] -> Maybe a
headMaybe list =
  case list of
    [] -> Nothing
    x : _ -> Just x


--
-- Extract the tail of a list, if any. Use `error` when it's missing.
--
-- ghci> tail []
-- *** Exception: Prelude.tail: empty list
--
-- ghci> tail [1,2,3]
-- [2,3]
--
tail :: [a] -> [a]
tail [] = error "empty list"
tail (_ : xs) = xs


--
-- Extract the tail of the list, if any. Use Maybe to convey optionality.
--
-- ghci> tailMaybe []
-- Nothing
--
-- ghci> tailMaybe [1,2,3]
-- Just [2,3]
--
tailMaybe :: [a] -> Maybe [a]
tailMaybe [] = Nothing
tailMaybe (_ : xs) = Just xs


--
-- Calculate the length of the list.
--
-- ghci> length []
-- 0
-- ghci> length [1,2,3]
-- 3
--
length :: [a] -> Int
length [] = 0
length (_ : xs) =
  1 + length xs


lengthTCO :: [a] -> Int
lengthTCO list =
  let
    loop []     accum = accum
    loop (x:xs) accum = loop xs (accum + 1)
  in
    loop list 0


--
-- Find and return the nth element of a list. Use `error "message"` to produce
-- errors for illegal indices.
--
-- ghci> [1,2,3] !! 0
-- 1
-- ghci> [1,2,3] !! 1
-- 2
-- ghci> [1,2,3] !! 2
-- 3
-- ghci> [1,2,3] !! 3
-- *** Exception: index too large
-- ghci> [1,2,3] !! (-3)
-- *** Exception: negative index
-- ghci> [] !! 0
-- *** Exception: index too large
--
(!!) :: [a] -> Int -> a
(!!) list n =
  case n of
    _ | n < 0 -> error "negative index"
    _ -> case list of
          [] -> error "index too large"
          (x : xs) -> case n of 0 -> x
                                _ -> xs !! (n-1)


--
-- Call a function over each element in a list, while accumulating the results
-- in a new list, which is the value returned.
--
-- ghci> map (\a -> a + 2) []
-- []
-- ghci> map (\a -> a + 2) [1,2,3]
-- [3,4,5]
--
map :: (a -> b) -> [a] -> [b]
map func [] = []
map func (x : xs) = (func x) : (map func xs)


mapTCO :: (a -> b) -> [a] -> [b]
mapTCO func list =
  let
    loop [] accum = accum
    loop (x : xs) accum = loop xs (accum ++ [func x])
  in loop list []


--
-- Find the first element in the list that satisfies the given predicate
-- function. Use Maybe to reflect that there may be no elements that satisfy
-- the predicate.
--
-- ghci> find (==2) [1,2,3]
-- Just 2
--
-- ghci> find (==2) []
-- Nothing
--
-- ghci> find (==4) [1,2,3]
-- Nothing
--
find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find pred (x : xs) = if pred x then Just x else find pred xs


-- ghci> filter odd []
-- []
--
-- ghci> filter even []
-- []
--
-- ghci> filter odd [1,2,3]
-- [1,3]
--
-- ghci> filter even [1,2,3]
-- [2]
--
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter func (x : xs) =
  if func x
  then x : filter func xs
  else filter func xs


-- TEMĂ
filterTCO :: (a -> Bool) -> [a] -> [a]
filterTCO = undefined


--
-- Determine whether *all elements* of a list satisfy the given predicate.
--
-- ghci> all odd []
-- True
--
-- ghci> all odd [1,3]
-- True
--
-- ghci> all odd [2,4]
-- False
--
all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all func (x : xs) =
  func x && all func xs


--
-- Determine whether *any element* of a list satisfy the given predicate.
--
-- ghci> any odd []
-- False
--
-- ghci> any odd [1]
-- True
--
-- ghci> any odd [1,2]
-- True
--
-- ghci> any odd [2]
-- False
--
any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any func (x : xs) = if func x then True else any func xs


--
-- Flatten a list *of lists* of elements to just a list of elements.
--
-- ghci> concat []
-- []
--
-- ghci> concat [[1,2,3], [4,5,6]]
-- [1,2,3,4,5,6]
--
-- ghci> concat [[1,2,3], [], [4,5,6]]
-- [1,2,3,4,5,6]
--

concat :: [[a]] -> [a]
concat [] = []
concat ([] : xs) = concat xs
concat ((x : xs) : rest) = x : (concat (xs : rest))


concatTCO :: [[a]] -> [a]
concatTCO = undefined


--
-- Produce a list with the first `n` elements of a list, where `n` is a
-- user-supplied argument.
--
-- ghci> take 0 []
-- []
--
-- ghci> take 1 []
-- []
--
-- ghci> take 0 [1,2,3]
-- []
--
-- ghci> take 1 [1,2,3]
-- [1]
--
-- ghci> take 2 [1,2,3]
-- [1,2]
--
-- ghci> take 3 [1,2,3]
-- [1,2,3]
--
-- ghci> take 4 [1,2,3]
-- [1
take :: Int -> [a] -> [a]
take 0 _ = []
--take 1 (x:xs) = [x]
take n (x:xs) = x : (take (n-1) xs)
take _ [] = []


takeTCO :: Int -> [a] -> [a]
takeTCO = undefined


--
-- Produce a list without the first `n` elements, where `n` is user-supplied.
--
-- ghci> drop 0 []
-- []
--
-- ghci> drop 1 []
-- []
--
-- ghci> drop 0 [1,2,3]
-- [1,2,3]
--
-- ghci> drop 1 [1,2,3]
-- [2,3]
--
-- ghci> drop 2 [1,2,3]
-- [3]
--
-- ghci> drop 3 [1,2,3]
-- []
--
-- ghci> drop 4 [1,2,3]
-- []
--
drop :: Int -> [a] -> [a]
drop 0 list = list
drop n [] = []
drop n (x : xs) = drop (n - 1) xs


--
-- Pair the elements of two lists. Hint: pattern match on both lists at the
-- same time using an intermediary tuple.
--
-- ghci> zip [] []
-- []
--
-- ghci> zip [] [1,2,3]
-- []
--
-- ghci> zip [1,2,3] []
-- []
--
-- ghci> zip [1,2,3] [4,5,6]
-- [(1,4), (2,5), (3,6)]
--
-- ghci> zip [1,2,3] [4,5]
-- [(1,4), (2,5)]
--
-- ghci> zip [1,2] [4,5,6]
-- [(1,4), (2,5)]
--
zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x : xs) (y : ys) = (x, y) : (zip xs ys)


zipTCO :: [a] -> [b] -> [(a, b)]
zipTCO = undefined


--
-- Append two lists together. Hint: pattern match on the first list.
--
-- ghci> [] ++ []
-- []
--
-- ghci> [] ++ [4,5,6]
-- [4,5,6]
--
-- ghci> [1,2,3] ++ []
-- [1,2,3]
--
-- ghci> [1,2,3] ++ [4,5,6]
-- [1,2,3,4,5,6]
--
(++) :: [a] -> [a] -> [a]
[]       ++ xs = xs
(x : xs) ++ ys = x : (xs ++ ys)


appendTCO :: [a] -> [a] -> [a]
appendTCO = undefined


--
-- Reverse a list. Hint: use (++).
--
-- ghci> reverse []
-- []
--
-- ghci> reverse [1,2,3]
-- [3,2,1]
--
reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = (reverse xs) ++ [x]


reverseTCO :: [a] -> [a]
reverseTCO = undefined


--
-- ghci> foldl (+) 0 []
-- 0
--
-- ghci> foldl (+) 0 [1,2,3]
-- 6
--
-- ghci> foldl (-) 0 [1,2,3]
-- -6
--
foldl = undefined


--
-- ghci> foldr (+) 0 []
-- 0
--
-- ghci> foldr (+) 0 [1,2,3]
-- 6
--
-- ghci> foldr (-) 0 [1,2,3]
-- 2
--
foldr = undefined


--
-- Reformulate all of the above in terms of either foldl or foldr.
--

lengthFold :: [a] -> Int
lengthFold = undefined

nthFold :: [a] -> Int -> a
nthFold = undefined

mapFold :: (a -> b) -> [a] -> [b]
mapFold = undefined

findFold :: (a -> Bool) -> [a] -> Maybe a
findFold = undefined

filterFold :: (a -> Bool) -> [a] -> [a]
filterFold = undefined

allFold :: (a -> Bool) -> [a] -> Bool
allFold = undefined

anyFold :: (a -> Bool) -> [a] -> Bool
anyFold = undefined

concatFold :: [[a]] -> [a]
concatFold = undefined

takeFold :: Int -> [a] -> [a]
takeFold = undefined

dropFold :: Int -> [a] -> [a]
dropFold = undefined

zipFold :: [a] -> [b] -> [(a, b)]
zipFold = undefined

appendFold :: [a] -> [a] -> [a]
appendFold = undefined

reverseFold :: [a] -> [a]
reverseFold = undefined
