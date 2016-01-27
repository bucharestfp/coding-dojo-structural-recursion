import Prelude hiding ((++), (!!), all, any, concat, drop, filter, find, foldl, foldr, head, length, map, reverse, tail, take, zip)

--
-- ghci> :i []
-- data [] a = [] | a : [a]  -- Defined in ‘GHC.Types’
--


-- Alternative recursive definition of linked lists.
data List a = Nil | Cons a (List a)


--
-- Here's how to approach these exercises.
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
head = undefined


--
-- Extract the head of the list, if any. Use Maybe to convey optionality.
--
-- ghci> headMaybe []
-- Nothing
--
-- ghci> headMaybe [1,2,3]
-- Just 1
--
headMaybe = undefined


--
-- Extract the tail of a list, if any. Use `error` when it's missing.
--
-- ghci> tail []
-- *** Exception: Prelude.tail: empty list
--
-- ghci> tail [1,2,3]
-- [2,3]
--
tail = undefined


--
-- Extract the tail of the list, if any. Use Maybe to convey optionality.
--
-- ghci> tailMaybe []
-- Nothing
--
-- ghci> tailMaybe [1,2,3]
-- Just [2,3]
--
tailMaybe = undefined


--
-- Calculate the length of the list.
--
-- ghci> length []
-- 0
-- ghci> length [1,2,3]
-- 3
--
length = undefined


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
(!!) = undefined


--
-- Call a function over each element in a list, while accumulating the results
-- in a new list, which is the value returned.
--
-- ghci> map (\a -> a + 2) []
-- []
-- ghci> map (\a -> a + 2) [1,2,3]
-- [3,4,5]
--
map = undefined


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
find = undefined


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
filter = undefined


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
all = undefined


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
any = undefined


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
concat = undefined


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
-- [1,2,3]
--
take = undefined


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
drop = undefined


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
zip = undefined


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
(++) = undefined


--
-- Reverse a list. Hint: use (++).
--
-- ghci> reverse []
-- []
--
-- ghci> reverse [1,2,3]
-- [3,2,1]
--
reverse = undefined


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
