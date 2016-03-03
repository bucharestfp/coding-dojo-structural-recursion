import Prelude hiding ((++), (!!), all, any, concat, drop, filter, find, foldl, foldr, head, length, map, reverse, tail, take, zip)

--
-- ghci> :i []
-- data [] a = [] | a : [a]  -- Defined in ‘GHC.Types’
--

head :: [a] -> a
head list =
  case list of
    [] -> error "empty list"
    x : xs -> x


headMaybe :: [a] -> Maybe a
headMaybe list =
  case list of
    [] -> Nothing
    x : xs -> Just x


tail :: [a] -> [a]
tail list =
  case list of
    [] -> error "empty list"
    x : xs -> xs


tailMaybe :: [a] -> Maybe ([a])
tailMaybe list =
  case list of
    [] -> Nothing
    x : xs -> Just xs


length :: [a] -> Int
length list =
  case list of
    [] -> 0
    x : xs -> 1 + (length xs)


lengthTCO :: [a] -> Int
lengthTCO list =
  let
    loop list count =
      case list of
        [] -> count
        x : xs -> loop xs (count + 1)
  in
    loop list 0


lengthFold :: [a] -> Int
lengthFold list =
  let
    init = 0
    fold count _ = count + 1
  in
    foldl fold init list


(!!) :: [a] -> Int -> a
list !! index =
  if index < 0
  then error "negative index"
  else
    case list of
      [] -> error "index too large"
      x : xs ->
        if index == 0
        then x
        else xs !! (index - 1)


nthFold :: [a] -> Int -> a
nthFold list n =
  if n < 0
  then error "negative index"
  else
    let
      fold (Nothing, n') x
        | n == n'   = (Just x, n)
        | otherwise = (Nothing, n' + 1)
      fold result _ = result
      init = (Nothing, 0)
    in
      case foldl fold init list of
        (Just elem, _) -> elem
        (Nothing, _) -> error "index too large"


map :: (a -> b) -> [a] -> [b]
map fn list =
  case list of
    [] -> []
    x : xs -> (fn x) : (map fn xs)


mapTCO :: (a -> b) -> [a] -> [b]
mapTCO fn list =
  let
    loop list result =
      case list of
        [] -> reverse result
        x : xs -> loop xs ((fn x) : result)
  in
    loop list []


mapFold :: (a -> b) -> [a] -> [b]
mapFold fn list =
  let
    init = []
    fold result x = (fn x) : result
  in
    reverse (foldl fold init list)
    reverse $ foldl fold init list
    -- foldr (flip fold) init list


find :: (a -> Bool) -> [a] -> Maybe a
find predicate list =
  case list of
    [] -> Nothing
    x : xs ->
      if predicate x
      then Just x
      else find predicate xs


findFold :: (a -> Bool) -> [a] -> Maybe a
findFold predicate list =
  let
    init = Nothing
    fold Nothing x | predicate x = Just x
                   | otherwise = Nothing
    fold result _ = result
  in
    foldl fold init list


filter :: (a -> Bool) -> [a] -> [a]
filter predicate list =
  case list of
    [] -> []
    x : xs ->
      if predicate x
      then x : (filter predicate xs)
      else filter predicate xs


filterTCO :: (a -> Bool) -> [a] -> [a]
filterTCO predicate list =
  let
    loop list result =
      case list of
        [] -> reverse result
        x : xs -> loop xs (if predicate x then x : result else result)
  in
    loop list []


filterFold :: (a -> Bool) -> [a] -> [a]
filterFold pred =
  let
    init = []
    fold result x
      | pred x = x : result
      | otherwise = result
  in
    reverse . foldl fold init
    -- foldr (flip fold) init


all :: (a -> Bool) -> [a] -> Bool
all predicate list =
  case list of
    [] -> True
    x : xs ->
      if predicate x
      then all predicate xs
      else False


allFold :: (a -> Bool) -> [a] -> Bool
allFold predicate list =
  let
    fold result x = predicate x && result
    init = True
  in
    foldl fold init list


any :: (a -> Bool) -> [a] -> Bool
any predicate list =
  case list of
    [] -> False
    x : xs ->
      if predicate x
      then True
      else any predicate xs


anyFold :: (a -> Bool) -> [a] -> Bool
anyFold predicate list =
  let
    init = False
    fold _ x = predicate x
  in
    foldl fold init list


concat :: [[a]] -> [a]
concat lists =
  case lists of
    [] -> []
    xs : xss -> xs ++ (concat xss)


concatTCO :: [[a]] -> [a]
concatTCO lists =
  let
    loop lists result =
      case lists of
        [] -> reverse result
        [] : xs -> loop xs result
        (x : xs) : rest -> loop (xs : rest) (x : result)
  in
    loop lists []


concatFold :: [[a]] -> [a]
concatFold lists =
  let
    init = []
    -- fold' result x = x : result
    -- fold result xs = reverse (foldl fold' (reverse result) xs)
    fold result xs = reverse (foldl (flip (:)) (reverse result) xs)
  in
    foldl fold init lists


take :: Int -> [a] -> [a]
take n list =
  case list of
    [] -> []
    x : xs ->
      if n == 0
      then []
      else x : (take (n - 1) xs)


takeTCO :: Int -> [a] -> [a]
takeTCO n list =
  let
    loop list n result =
      case list of
        [] -> reverse result
        x : xs ->
          if n == 0
          then reverse result
          else loop xs (n - 1) (x : result)
  in
    loop list n []


takeFold :: Int -> [a] -> [a]
takeFold n =
  let
    init = ([], n)
    fold (result, 0) _ = (result, 0)
    fold (result, n) x = (x : result, n - 1)
  in
    reverse . fst . foldl fold init


drop :: Int -> [a] -> [a]
drop n list =
  case list of
    [] -> []
    x : xs ->
      if n == 0
      then list
      else drop (n - 1) xs


dropFold :: Int -> [a] -> [a]
dropFold n =
  let
    init = ([], n)
    fold (result, 0) x = (x : result, 0)
    fold (result, n) _ = (result, n - 1)
  in
    reverse . fst . foldl fold init


zip :: [a] -> [b] -> [(a, b)]
zip xs ys =
  case (xs, ys) of
    ([], _) -> []
    (_, []) -> []
    (x : xs, y : ys) -> (x, y) : (zip xs ys)


zipTCO :: [a] -> [b] -> [(a,b)]
zipTCO xs ys =
  let
    loop xs ys result =
      case (xs, ys) of
        ([], _) -> reverse result
        (_, []) -> reverse result
        (x : xs, y : ys) -> loop xs ys ((x, y) : result)
  in
    loop xs ys []


zipFold :: [a] -> [b] -> [(a, b)]
zipFold as bs =
  let
    init = ([], bs)
    fold (result, []) a = (result, [])
    fold (result, b : bs) a = ((a, b) : result, bs)
  in
    reverse . fst $ foldl fold init as


(++) :: [a] -> [a] -> [a]
xs ++ ys =
  case xs of
    [] -> ys
    x : xs -> x : (xs ++ ys)


appendTCO :: [a] -> [a] -> [a]
appendTCO xs ys =
  let
    loop ys result =
      case ys of
        [] -> reverse result
        y : ys -> loop ys (y : result)
  in
    loop ys (reverse xs)


appendFold :: [a] -> [a] -> [a]
appendFold as bs =
  let
    init = reverse as
    fold result b = b : result
  in
    reverse (foldl fold init bs)


reverse :: [a] -> [a]
reverse list =
  case list of
    [] -> []
    x : xs -> reverse xs ++ [x]


reverseTCO :: [a] -> [a]
reverseTCO xs =
  let
    loop xs result =
      case xs of
        [] -> result
        x : xs -> loop xs (x : result)
  in
    loop xs []


reverseFold :: [a] -> [a]
reverseFold = foldl (flip (:)) []


foldl :: (b -> a -> b) -> b -> [a] -> b
foldl fn init list =
  case list of
    [] -> init
    x : xs -> foldl fn (fn init x) xs


foldr :: (a -> b -> b) -> b -> [a] -> b
foldr fn init list =
  case list of
    [] -> init
    x : xs -> fn x (foldr fn init xs)
